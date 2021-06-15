
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

list.files("articles_xml2/",full.names = TRUE) %>%
  map_dfr(function(x){
    cont <- x %>%
      xml2::read_xml() %>%
      xml2::xml_find_all("content")

    canonic <- map_chr(cont, ~get_text(.x, "canonical"))
    tibble(canonic = canonic, x = x)
  }) %>% View

list.files("articles_xml2/",full.names = TRUE) %>%
  map(function(y){
    cont <- y %>%
      xml2::read_xml() %>%
      xml2::xml_find_all("content")

    canon <- map_chr(cont, ~get_text(.x, "canonical"))
    news <- FALSE
    cont[grepl("^/wissenschaft",canon)] %>%
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

        images <- get_text(x, "images")
        created_by <- get_text(x, "created_by")
        canonical <- get_text(x, "canonical")

        introtext_md <- html2markdown(introtext) %>%
          paste(collapse = "\n")
        fulltext_md <- html2markdown(fulltext) %>%
          paste(collapse = "\n")

        if(fulltext == ""){fulltext <- introtext}

        canonical2 <- str_replace(canonical, "^/", "_")
        dirnam <- map(str_split(canonical2, "/"), ~paste(.x[1],.x[2], sep = "-"))
        # categ <- if(news){""}else{map(str_split(canonical2, "/"), ~.x[2])}

        basenam <- basename(canonical)
        basenam <- str_sub(basenam, 1, 40)
        if(news){basenam <- paste(publish_date,basenam, sep = "-")}

        filenam <- paste0(basenam, ".Rmd")

        filename_full <- file.path(dirnam, basenam, filenam)

        file.create2(filename_full)
        print(filename_full)

        desription <- xml2::read_html(introtext) %>% xml2::xml_text()
        desription <- str_replace_all(desription, "\n", " ")

        write_lines(
          glue(
            "---",
            "title: {title}",
            "description: |",
            "  {desription}",
            "author: {created_by}",
            "date: {publish_date}",
            # ifelse(news,"","categories:"),
            # ifelse(news,"", "  - {categ}"), # <- this does not work yet!
            "output:",
            "  distill::distill_article:",
            "    self_contained: false",
            "canonical: {canonical}",
            "---",
            "",
            "",
            "{fulltext_md}",
            "",
            "<!--http://wieselundco.ch{canonical}-->",
            .sep= "\n"
          ),
          file = filename_full,
          append = FALSE
        )
        tryCatch(rmarkdown::render(filename_full), error = function(x){print(paste("failed",filename_full))})
      }
      )

  })






