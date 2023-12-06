library(tidyverse)
embed <- readRDS("get_videoembeddings/embeddings.RDS")

embed <- embed %>%
  select(Img, Video, V209)

embed %>% 
  arrange(V209) %>% 
  group_by(Video) %>%
  #slice(1) %>%
  View()
