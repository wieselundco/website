
library(tidyverse)


mydirs <- list.dirs(recursive = FALSE,full.names = FALSE)

these_files <- map(mydirs[startsWith(mydirs, "_")], ~list.files(.x,"\\.Rmd$",full.names = TRUE,recursive = TRUE)) %>%
  unlist()


map(these_files, function(x){
  read_lines(x) -> rl
  urls <- str_match(rl, "\\[.+\\]\\((.+)\\)")[,2]
  urls[!is.na(urls)]
}) %>%
  unlist() %>%
  cat(sep = "\n")


# these_files <- list.files("_news/",".Rmd$", recursive = TRUE, full.names = TRUE)


# map(these_files, ~file.edit(.x))

map(these_files, ~rmarkdown::render(.x))



# <.+|.+>
