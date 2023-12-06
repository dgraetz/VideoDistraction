#only needs to be executed once
#converts eyetracking files from EDF to ASC
#reads in eyetracking files, merges with behavioral info
#determines if looked
#only one row per frame

library(eyelinker)
library(rio)
library(tidyverse)
library(doParallel)
library(foreach)

#Those are the areas of interest
left_t_close <-    c( 410-50, 340,  810-50, 740)
left_t_far <-      c( 210-50, 340,  610-50, 740)
right_t_far <-     c(1310+50, 340, 1710+50, 740)
right_t_close <-   c(1110+50, 340, 1510+50, 740)
left_close <-      c( 460-50, 390,  760-50, 690)
left_far <-        c( 260-50, 390,  560-50, 690)
right_far <-       c(1360+50, 390, 1660+50, 690)
right_close <-     c(1160+50, 390, 1460+50, 690)
left_trigger <-    c(  10-50, 290,  810-50, 790)
right_trigger <-   c(1110+50, 290, 1910+50, 790)

source("Analysis/edf2asc.R")

isInRect <- function(x, y, rect){
  return(as.numeric(x >= rect[1] & x <= rect[3] & y >= rect[2] & y <= rect[4]) )
}

edf2asc(list.files("data", full.names = TRUE))

#Reading in data
data <- list()

behs <- list.files("data/", pattern = "txt", full.names = TRUE)
eyes <- list.files("data/", pattern = "asc", full.names = TRUE)

proc.time()
cluster <- makeCluster(detectCores()-2)
registerDoParallel(cluster)
data <- foreach (i = 1:length(behs), .packages = c("rio", "eyelinker")) %dopar%{
  beh <- import(behs[i])
  ID <- beh$ID[1]
  eye_file <- eyes[grepl(paste0(ID, "_VID.asc"), basename(eyes))]
  
  if (length(eye_file) == 1){
    eye <- read.asc(eyes[grepl(paste0(ID, "_VID.asc"), basename(eyes))])
    return(list(beh = beh, eye = eye))
  } else {
    return(NULL)
  }
  
}
stopCluster(cluster)

data <- lapply(data, function(x){
  
  blinks <- apply(x$eye$blinks %>% select(stime, etime), 1, function(blinks){blinks[1]:blinks[2]}) %>% unlist()
  
  eye_new <- left_join(x$eye$raw, x$eye$msg, by = c("block", "time")) %>%
    mutate(Block = str_extract(text, "(?<=BLOCK)[[:digit:]]*") %>% as.numeric(),
           Trials = str_extract(text, "(?<=TRIAL)[[:digit:]]*") %>% as.numeric(),
           Frame = str_extract(text, "(?<=FRAME)[[:digit:]]*") %>% as.numeric(),
           StimOnset = NA,
           StimOnset = ifelse(grepl("STIMONSET", text), 1, StimOnset),
           isInBlink = ifelse(time %in% blinks, 1, 0)) %>%
    fill(Block, Trials) %>%
    group_by(Block, Trials) %>%
    mutate(time = time-time[1]) %>%
    fill(StimOnset, Frame) %>%
    filter(StimOnset == 1) %>%
    filter(!is.na(Frame)) %>%
    mutate(Frame_id = 0,
           Frame_id = ifelse(!is.na(lag(Frame, 1) != Frame) & lag(Frame, 1) != Frame, 1, Frame_id),
           Frame_id = cumsum(Frame_id)
    ) %>%
    filter(isInBlink == 0)
  
  beh <- x$beh %>%
    select(ID, Block, Trials, Practice, ITI, Video_pos, Video_dist, RewardCond, Video, VideoStart)
  
  eye_new <- left_join(eye_new, beh, by = c("Block", "Trials")) %>%
    filter(Practice == 0, VideoStart == "auto") %>%
    filter(!is.na(xp)) %>%
    mutate(isInVid = 0,
           isInVid = ifelse(Video_pos == "left"  & Video_dist == "far"   & isInRect(xp, yp, left_t_far),    1, isInVid),
           isInVid = ifelse(Video_pos == "left"  & Video_dist == "close" & isInRect(xp, yp, left_t_close),  1, isInVid),
           isInVid = ifelse(Video_pos == "right" & Video_dist == "far"   & isInRect(xp, yp, right_t_far),   1, isInVid),
           isInVid = ifelse(Video_pos == "right" & Video_dist == "close" & isInRect(xp, yp, right_t_close), 1, isInVid)
    ) %>%
    group_by(Block, Trials, Frame_id) %>%
    mutate(isInVid = ifelse(sum(isInVid) >= 1, 1, 0)) %>%
    slice(1) %>%
    ungroup() %>%
    select(-Frame_id, -Practice, -Video_pos, -Video_dist, -VideoStart, -cr.info, -text, -block, -xp, -yp, -ps, -isInBlink, -StimOnset)
  
}
)

data <- bind_rows(data)

saveRDS(data, file = "analysis/results/all_subj.RDS")
