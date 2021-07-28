
b <- function(str){
  filpath <- str_remove(str, "\\(.+\\)") %>%
    str_trim()

  file.edit(filpath)
  rstudioapi::filesPaneNavigate(dirname(filpath))
  return(TRUE)
}
