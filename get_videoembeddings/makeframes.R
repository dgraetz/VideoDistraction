library(imager)
library(doParallel)
library(foreach)
library(abind)


cl <- makeCluster(10)
registerDoParallel(cl)

vids <- list.files("videostimuli/v3/", pattern = ".mp4", full.names = TRUE)
output_path <- "videostimuli/videoframes_HQ/"

vids <- vids[!dir.exists(paste0(output_path, basename(vids)))]

foreach (i = 1:length(vids), .packages = "imager") %dopar% {
  
  dir.create(paste0(output_path, basename(vids[i])))
  vid <- load.video(vids[i], maxSize = 10) #x, y, frame, RGB
  frames <- dim(vid)[3]
  
  for(j in 1:frames){
    save.image(as.cimg(vid[,,j,]), paste0(output_path, basename(vids[i]), "/", basename(vids[i]), "_", j, ".jpg"), quality = 100)
  }
  
}

stopCluster(cl)
source("get_videoembeddings/python_img_embeddings.R")
