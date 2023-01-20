library(tidyverse) 
library(here)

file.path = here::here("analysis/scripts")

list.files(file.path, full.names = TRUE) %>% 
  map(source)
