
library(tidyverse)

all_rmds <- list.files(pattern = ".Rmd", recursive = TRUE, full.names = TRUE)

map_dfr(all_rmds, function(x){
    rl <- read_lines(x)

    cha <- rl[str_detect(rl, "\\[.+\\]")]

    res <- str_match(rl, "\\[(.+)\\]\\((.+)\\)")

    as.data.frame(res) %>%
      mutate(file = x) %>%
      select(-V1, -V2)


  }) %>%
  filter(!is.na(V3)) %>%
  knitr::kable(format = "pipe")

cat(res$char, sep = "\n")

res <- res %>%
  mutate(g = str_replace_all(str_match(gallery, "\\}(.+)\\{")[,2], fixed("\\"),""))

res %>%
  mutate(
    fd = paste0("- [ ] ",file, " (", g, ")")
  ) %>% pull(fd) %>% cat(sep = "\n")



res %>%
  mutate(web = map_chr(str_split(file, "/"), ~paste(.x[2:3], collapse = "/"))) %>%
  mutate(web = paste0("https://wieselundco.github.io/website/",str_remove(web, "_"))) %>%
  pull(web) %>%
  paste("- [ ]",.) %>%
  cat(sep = "\n")






imagess <- str_match(images, "(images(/\\w+)*\\.\\w+)")[,2]

imagess[!is.na(imagess)]

cat(paste("- [ ]", images), sep = "\n")
