
library(tidyverse)
library(glue)

mydirs <- list.dirs(recursive = FALSE,full.names = FALSE)

these_files <- map(mydirs[startsWith(mydirs, "_")], ~list.files(.x,"\\.Rmd$",full.names = TRUE,recursive = TRUE)) %>%
  unlist()


read_yamlheader <- function(fi, element = NULL){
  rl <- read_lines(fi)

  if(rl[1] == "---"){
    headerpos <- which(str_detect(rl, "---"))[1:2]

    yamlheader <- rl[(headerpos[1]+1):(headerpos[2]-1)]
    yaml_list <- yaml::yaml.load(yamlheader)
    if(!is.null(element)){
      yaml_list[[element]]
    } else{
      yaml_list
    }
  } else{
    warning(paste("File does not seem to have a yaml header (it does not start with '---'",fi))
    NULL
  }

}

map_dfr(these_files, function(x){

  image_intro <- read_yamlheader(x, "image_intro")
  preview <- read_yamlheader(x, "preview")

  image_intro <- ifelse(is.null(image_intro),NA_character_,image_intro)
  preview <- ifelse(is.null(preview),NA_character_,preview)


  tibble(
    image_intro = image_intro,
    preview = preview ,
    file = x)
}) -> intr

intr$preview <- file.path(dirname(intr$file), intr$preview)
# intr$image_intro_size <- file.size(intr$image_intro)
# intr$image_preview_size <- file.size(intr$preview)

intr %>%
  filter(!is.na(image_intro)) %>%
  pmap(function(image_intro, preview, file){
    image_intro_size <- file.size(image_intro)
    image_preview_size <- file.size(preview)
    if(image_intro_size < image_preview_size){
      file.copy(image_intro, preview,overwrite = TRUE)
    }
  })
# map_dfr(these_files, function(fi){
#
#   guess_encoding(fi) %>%
#     pivot_wider(names_from = encoding, values_from = confidence) %>%
#     janitor::clean_names() %>%
#     mutate(fi = fi)
#
# }) -> enc

# map("_site.yml", function(fi){
#
#   fi_dir <- dirname(fi)
#   fi_file <- basename(fi)
#
#   fi_file_new <- str_replace(fi_file, "\\.yml",".utf8.yml")
#   fi_new <- file.path(fi_dir, fi_file_new)
#
#   system(glue("iconv -f utf-8 -t utf-8 -c {fi} > {fi_new}"))
#   file.remove(fi)
#   file.rename(fi_new, fi)
#
# })


map(these_files, function(x){
  read_lines(x) -> rl
  urls <- str_match(rl, "\\[.+\\]\\((.+)\\)")[,2]
  urls[!is.na(urls)]
}) %>%
  unlist() %>%
  cat(sep = "\n")


these_files <- list.files("_wissenschaft-externe-projekte/",".Rmd$", recursive = TRUE, full.names = TRUE)


map(these_files, ~file.edit(.x))

map(these_files, ~rmarkdown::render(.x))



# <.+|.+>
