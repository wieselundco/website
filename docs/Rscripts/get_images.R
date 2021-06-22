images <- list.files(pattern = ".Rmd", recursive = TRUE, full.names = TRUE) %>%
  map(function(x){
    rl <- read_lines(x)

    rl[str_detect(rl, "images")]

  }) %>%
  unlist()


imagess <- str_match(images, "(images(/\\w+)*\\.\\w+)")[,2]

imagess[!is.na(imagess)]

cat(images, sep = "\n")
