
library(tidyverse)
library(xml2)
library(glue)



get_text <- function(xm, tag){
  xml_child(xm, tag) %>% xml_text()
}



file.create2 <- function(filename){
  filedir <- dirname(filename)
  if(!exists(filedir))dir.create(filedir, recursive = TRUE)
  file.create(filename)
  filename
}

html2markdown <- function(string){
  if(string == ""){return("")} else{
    html_file <- tempfile(fileext = ".html")
    md_file <- tempfile(fileext = ".md")
    write_lines(string, html_file)
    rmarkdown::pandoc_convert(html_file, to = "markdown_strict", output = md_file)
    read_lines(md_file)
  }
}

canonicals <- list.files("articles_xml2/",full.names = TRUE) %>%
  map_dfr(function(x){
    cont <- x %>%
      xml2::read_xml() %>%
      xml2::xml_find_all("content")

    canonic <- map_chr(cont, ~get_text(.x, "canonical"))
    tibble(canonic = canonic, x = x)
  })

images_df <- list.files("articles_xml2/",full.names = TRUE) %>%
  map_dfr(function(x){
    cont <- x %>%
      xml2::read_xml() %>%
      xml2::xml_find_all("content")


    map(cont, function(x){get_text(x, "images") %>% jsonlite::fromJSON()})

  })


images_df %>%
  select(image_intro, image_fulltext) %>%
  pivot_longer(c(image_intro, image_fulltext)) %>%
  filter(value != "") %>%
  pull(value) %>%
  map_dfr(~tibble(file = .x, exists = file.exists(.x))) %>%
  filter(!exists) %>%
  pull(file) %>% cat(sep = "\n")


map_chr(str_split(canonicals$canonic, "/"), ~.x[2]) %>%
  unique() %>%
  paste0('"',.,'"') %>%
  cat(sep = ",")

xml2rmd <- function(canonical_keyword, makenews = FALSE){

  stopifnot(canonical_keyword %in% c("14-news","projekt","wissenschaft","2-uncategorised","faszination-kleinraubtiere","mithelfen-anpacken","impressum","","login","ueber-uns","17-partner","10-das-projekt"))

  list.files("articles_xml2/",full.names = TRUE) %>%
    map(function(y){
      cont <- y %>%
        xml2::read_xml() %>%
        xml2::xml_find_all("content")

      featured <- cont %>%
        xml2::xml_find_all("featured") %>%
        xml2::xml_text()

      if(makenews){
        cont <- cont[featured != 0]
      }

      canon <- map_chr(cont, ~get_text(.x, "canonical"))
      mynodes <- cont[grepl(glue("^/{canonical_keyword}"),canon)]

      if(length(mynodes)>0){
        mynodes %>%
          # head(1) %>%
          map(function(x){


            title <- get_text(x, "title")


            catid <- get_text(x, "catid")
            introtext <- get_text(x, "introtext")
            fulltext <- get_text(x, "fulltext")
            created <- get_text(x, "created")
            publish_up <- get_text(x, "publish_up")
            publish_up <- parse_datetime(publish_up,locale = locale(tz = "CET"))
            publish_date <- as.Date(publish_up)


            created_by <- get_text(x, "created_by")
            canonical <- get_text(x, "canonical")

            introtext_md <- html2markdown(introtext) %>%
              paste(collapse = "\n")
            fulltext_md <- html2markdown(fulltext) %>%
              paste(collapse = "\n")

            if(fulltext == ""){fulltext <- introtext}

            canonical2 <- str_replace(canonical, "^/", "_")
            dirnam <- case_when(
              grepl("ueber-uns",canonical_keyword)~map_chr(str_split(canonical2, "/"), ~.x[1]),
              makenews~"_news",
              TRUE~map_chr(str_split(canonical2, "/"), ~paste(.x[1],.x[2], sep = "-"))
            )

            basenam <- basename(canonical)
            basenam <- str_sub(basenam, 1, 40)
            if(makenews){
              basenam <- str_sub(basenam, 1, 30)
              basenam <- paste(publish_date,basenam, sep = "-")
            }

            filenam <- paste0(basenam, ".Rmd")

            filename_full <- file.path(dirnam, basenam, filenam)

            file.create2(filename_full)
            print(filename_full)

            ## Get Images ######################################################
            images <- get_text(x, "images")%>%
              jsonlite::fromJSON()

            images <- images[map_lgl(images, ~.x != "")]

            if(length(images)>0){
              imap(images, function(image_name_old,y){
                if(file.exists(image_name_old)){
                  image_new <- file.path(dirnam, basenam, basename(image_name_old))
                  file.copy(image_name_old,image_new)
                } else{
                  paste("Image not found:",image_name_old)
                }

              })
            }

            image_chunk <- if(!is.null(images$image_fulltext)){
              paste(
                paste0("```{r ", ", echo = FALSE}"),
                paste0("knitr::include_graphics('", basename(images$image_fulltext), "', error = FALSE) #",images$image_fulltext),
                paste0("```"),
                sep = "\n"
              )
            } else{""}

            image_preview <- if(!is.null(images$image_intro)){
              glue("preview: {basename(images$image_intro)}")
            } else{""}



            images_chr <- paste(imap_chr(images, ~glue("{.y}: {.x}")),collapse = "\n")

            ####################################################################



            urlchunk <- paste(
              "```{r,results='asis', echo = FALSE}",
              "pander::pandoc.horizontal.rule()",
              "",
              "pander::pandoc.link(url = glue::glue('{params$editurl}{params$rmdpath}'),text = 'Artikel bearbeiten')",
              "```",
              sep = "\n"
            )




            ####################################################################
            desription <- xml2::read_html(introtext) %>% xml2::xml_text()
            desription <- str_replace_all(desription, "\n", " ")

            mylines <- glue(
              "---",
              "title: '{title}'",
              "description: |",
              "  {desription}",
              # "author: {created_by}",
              "date: {publish_date}",
              image_preview,
              images_chr,
              "output:",
              "  distill::distill_article:",
              "    self_contained: false",
              "canonical: {canonical}",
              "params:",
              "  rmdpath: {filename_full}",
              "  editurl: https://github.com/wieselundco/website/edit/master/",
              "---",
              "",
              "",
              "{image_chunk}",
              "",
              "{fulltext_md}",
              "",
              "{urlchunk}",
              "",
              "<!--http://wieselundco.ch{canonical}-->",
              .sep= "\n"
            )

            write_lines(
              mylines,
              file = filename_full,
              append = FALSE
            )
            tryCatch(rmarkdown::render(filename_full), error = function(x){print(paste("failed",filename_full))})
          }
          )

      }


    })
}


# trashdirs <- list.files(pattern = "^_")



c(
  "14-news" = TRUE,
  "2-uncategorised" = TRUE,
  "projekt" = FALSE,
  "wissenschaft" = FALSE,
  "faszination-kleinraubtiere" = FALSE,
  "mithelfen-anpacken" = FALSE,
  # "impressum" = FALSE,
  # "" = FALSE,
  # "login" = FALSE,
  "ueber-uns" = FALSE
  # "17-partner" = FALSE
  # "10-das-projekt" = FALSE,
) %>%
  # head(1) %>%
  imap(~xml2rmd(canonical_keyword = .y,makenews = .x))

 xml2rmd(canonical_keyword = "2-uncategorised",makenews = TRUE)



list.files(path = "_news", pattern = "\\.+Rmd$", recursive = TRUE,full.names = TRUE) %>%
  map_chr(~tryCatch(rmarkdown::render(.x), error = function(x){.x}))





