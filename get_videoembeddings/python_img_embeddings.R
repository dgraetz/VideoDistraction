library(reticulate)
library(lsa)
library(tidyverse)

m <- import("img2vec_pytorch")
pil <- import("PIL")

img2vec <- function(path){
  image <- pil$Image$open(path)
  return(m$Img2Vec(cuda = TRUE)$get_vec(image, tensor=TRUE)$numpy() %>% as.vector())
}

imgs <- list.files("videostimuli/videoframes_HQ/", pattern = ".jpg", full.names = TRUE, recursive = TRUE)
imgs <- imgs[!grepl("shuff", imgs)]

embeddings <- list()
for (i in 1:length(imgs)){
  embeddings[[i]] <- img2vec(imgs[i]) %>% matrix(nrow = 1) %>% as.data.frame()
  print(paste0(round((i/length(imgs))*100, 4), " % complete."))
}

embeddings_df <- bind_rows(embeddings)
Video <- gsub("(?<=mp4)[[:print:]]*", "", imgs %>% basename(), perl = TRUE)
Img <- imgs %>% basename()
Frame <- str_extract(basename(imgs), "[[:digit:]]*(?=.jpg)") %>% as.numeric()
embeddings_df <- cbind(Img, Video, Frame, embeddings_df) %>%
  arrange(Video, Frame)
saveRDS(embeddings_df, file = "get_videoembeddings/results/embeddings.RDS")



