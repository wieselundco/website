
list.files(pattern = "\\.Rmd") %>%
  map(function(x){
    rl <- read_lines(x)
    mat <- str_match(rl, "listing: (.+)")[,2]
    mat[!is.na(mat)]
    }) %>%
  unlist() %>%
  map(function(x){
    mydirs <- list.dirs(paste0("_",x),recursive = FALSE,full.names = TRUE)
    map(mydirs, list.files())
    })
