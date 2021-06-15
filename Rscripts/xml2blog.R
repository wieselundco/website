
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

xml_content <- xml2::read_xml("articles/j2xml1920020210615075249.xml") %>%
  xml2::xml_find_all("content")

canon <- map_chr(xml_content, ~get_text(.x, "canonical"))


xml_content[grepl("^/wissenschaft",canon)] %>%
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

    publish_up
    canonical

    introtext_md <- html2markdown(introtext) %>%
      paste(collapse = "\n")
    fulltext_md <- html2markdown(fulltext) %>%
      paste(collapse = "\n")

    if(fulltext == ""){fulltext <- introtext}

    dirnam <- map(str_split(str_replace(canonical, "^/", "_"), "/"), ~.x[1])
    categ <- map(str_split(str_replace(canonical, "^/", "_"), "/"), ~.x[2])


    basenam <- basename(canonical)
    basenam <- str_sub(basenam, 1, 30)

    filenam <- paste0(basenam, ".Rmd")

    filename_full <- file.path(dirnam, basenam, filenam)

    file.create2(filename_full)
    print(filename_full)

    desription <- xml2::read_html(introtext) %>% xml2::xml_text()

    write_lines(
      glue(
        "---",
        "title: {title}",
        "description: |",
        "  {desription}",
        "author: {created_by}",
        "date: {publish_date}",
        "categories:",
        "  - {categ}",
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

