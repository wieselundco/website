
b <- function(str){
  filpath <- str_remove(str, "\\(.+\\)") %>%
    str_trim()

  file.edit(filpath)
  rstudioapi::filesPaneNavigate(dirname(filpath))
  return(TRUE)
}


get_rmd_files <- function(levels = "both", excludes = c("./docs", "./www", "./images", "./Rscripts")){
  level0 <- list.files(pattern = ".Rmd",recursive = FALSE)

  level1 <- list.dirs(recursive = FALSE)
  level1 <- level1[!level1 %in% excludes]

  level1 <- map(level1, ~list.files(path = .x, pattern = ".Rmd", recursive = TRUE, full.names = TRUE)) %>%
    unlist()

  if(levels == "both"){
    c(level0, level1)

  } else if(levels == "0"){
    level0
  } else if(levels == "1"){
    level1
  }
}


move_article <- function(keyword, folder){

  all_rmd_files <- get_rmd_files()

  res <- all_rmd_files[str_detect(all_rmd_files, keyword)]

  dir <- dirname(res)

  dir
}


keyword_open <- function(keyword, open_i = NULL){
  library(tidyverse)

  all_rmd_files <- get_rmd_files()

  res <- all_rmd_files[str_detect(all_rmd_files, keyword)]

  if(!is.null(open_i)) res <- res[open_i]

  res_len <- length(res)

  if(res_len == 0){
    print("Sorry, noting found")
  }  else if(res_len == 1){

    print(paste("opening",res))
    b(res)
  } else(
    res
  )
}




render_all <- function(levels = "1", startfrom = 1){
  cleaned <- get_rmd_files(levels) %>%
    str_trim() %>%
    unique()

  tibble(files = cleaned, i = seq_along(cleaned)) %>%
    filter(i >= startfrom) %>%
    pmap(function(files, i){
      print(paste("i:",i))
      print(files)
      rmarkdown::render(files)
    })

}
render_all()


