edf2asc <- function(edf_paths,
                    exe_path = "C:/Program Files (x86)/SR Research/EyeLink/bin/edf2asc.exe",
                    max_cores = NULL){
  
  #dependent on conditions, requires parallel, doParallel and foreach libraries
  #requires the Eyelink developer kit to be installed from the forum https://www.sr-research.com/support/thread-7674.html
  
  if(is.null(max_cores)){
    library(parallel)
    max_cores <- detectCores()
  }
  
  #do files exist?
  dont_exist <- !file.exists(edf_paths)
  
  #warning if not
  if (sum(dont_exist) > 0){
    warning(paste(edf_paths[dont_exist], "don't exist."))
  }
  
  #remove edf paths to files that don't exist
  edf_paths <- edf_paths[!dont_exist]
  
  #check if target (asc) files exist already
  asc_paths <- gsub("edf", "asc", edf_paths)
  already_exists <- file.exists(asc_paths)
  
  if (sum(already_exists) > 0){
    warning(paste(asc_paths[already_exists], "already exist."))
  }
  
  #remove edf files for which asc exists already
  edf_paths <- edf_paths[!already_exists]
  
  if (length(edf_paths) > 1){
    library(foreach)
    library(doParallel)
    
    # max_cores <- detectCores()-2
    # cores <- 1:max_cores
    # 
    # cores <- cores[length(edf_paths) %% cores == 0]
    # cores <- max(cores)
    # 
    # cores <- ifelse(cores == 1, max_cores, cores)
    
    max_cores <- detectCores()-2
    
    cores <- ifelse(length(edf_paths) < max_cores, length(edf_paths), max_cores)
    
    if(length(cores) == 0){
      cores <- max_cores
    }
    
    
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    print(paste0("Using ", cores," cores."))

  
    results <- foreach (i = 1:length(edf_paths)) %dopar% {
      
      system2(exe_path,
                   args = shQuote(edf_paths[i], type = "cmd2"),
                   stdout = TRUE)
    }
  
    stopCluster(cl)
    
  } else if (length(edf_paths != 0)) {
    
    for(i in 1:length(edf_paths)){
    
      quiet <- system2(exe_path,
                   args = shQuote(edf_paths[i], type = "cmd2"),
                   stdout = TRUE)
    }
  }
  
}
