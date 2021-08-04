
library(tidyverse)
all_rmds <- list.files(pattern = ".Rmd", recursive = TRUE, full.names = TRUE)


## Gets all urls
url_df <- map_dfr(all_rmds, function(x){

    rl <- read_lines(x)

    res <- str_match(rl, "\\[(.+)\\]\\((http.+)\\)")


    as.data.frame(res) %>%
      mutate(file = x)


  })



## converts external urls (starting with http) into html urls
imap(all_rmds, function(x, i){

  print(paste(i, x))

  rl <- read_lines(x)

  rl_new <- str_replace(rl,"\\[(.+)\\]\\((http.+)\\)", "<a href='\\2' target='_blank'>\\1</a>")

  write_lines(rl_new, file = x)
})

## checks if urls are external

url_df %>%
  filter(!is.na(V1)) %>%
  filter(startsWith(V3, "http")) %>% View


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
