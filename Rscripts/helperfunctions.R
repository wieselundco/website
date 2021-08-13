
b <- function(str){
  filpath <- str_remove(str, "\\(.+\\)") %>%
    str_trim()

  file.edit(filpath)
  rstudioapi::filesPaneNavigate(dirname(filpath))
  return(TRUE)
}


keyword_open <- function(keyword){
  library(tidyverse)

  level0 <- list.files(pattern = ".Rmd",recursive = FALSE)

  level1 <- list.dirs(recursive = FALSE) %>%
    map(~list.files(path = .x, pattern = ".Rmd", recursive = TRUE, full.names = TRUE)) %>%
    unlist()

  all_rmd_files <- c(level0, level1)

  res <- all_rmd_files[str_detect(all_rmd_files, keyword)]

  res_len <- length(res)
  if(res_len == 0){
    print("Sorry, noting found")
  }  else if(res_len == 1){
    print(paste("opening",res))
    b(res)

  } else{
    print(res)
  }
}
