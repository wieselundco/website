
library(tidyverse)

all_rmds <- list.files(pattern = ".Rmd", recursive = TRUE, full.names = TRUE)

res <- map_dfr(all_rmds, function(x){
    rl <- read_lines(x)

    cha <- rl[str_detect(rl, "\\{gallery\\}")]

    if(length(cha) == 0){cha <- NA}

    data.frame(file = x, gallery = cha)

  }) %>%
  filter(!is.na(gallery))

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
