library(tidyverse)

embed <- readRDS("get_videoembeddings/embeddings.RDS")

high <- embed %>%
  group_by(Video) %>%
  arrange(desc(V1)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(V1)) %>%
  slice(1:5)

high <- high$Img


low <- embed %>%
  group_by(Video) %>%
  arrange(V1) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(V1) %>%
  slice(1:5)

low <- low$Img

imgs <- c(low, high)
img_paths <- 

