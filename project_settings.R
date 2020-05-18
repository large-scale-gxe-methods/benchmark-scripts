# gsutil_dir <- "/Users/amanning/google-cloud-sdk/bin/"
gsutil_dir <- "/Users/kennywesterman/google-cloud-sdk/bin/"

get_terra_bgen_files_size <- function(file_path) {
  ## input bgen is a parameter listed in GEM_Input.param file
 param_files <- system(paste0(gsutil_dir, "gsutil ls -R ",file_path,"/shard-*/GEM_Input.param"),intern = TRUE)

 param_files_content <- system(paste0(gsutil_dir, "gsutil cat ",paste(param_files,collapse=" ")),intern=T)
 #param_files_content
 bgen_files <- sub("/cromwell_root/","gs://",param_files_content[grep(pattern=" GENO_FILE_PATH",param_files_content)+1])

 bgen_file_sizes <- system(paste0(gsutil_dir, "gsutil du -h ",paste(bgen_files,collapse=" ")),intern=TRUE)

 bgen_file_sizes_table <- t(sapply(bgen_file_sizes,function(x){strsplit(x,split="  ")[[1]][c(2,1)]}))
 row.names(bgen_file_sizes_table) <- param_files
 return(bgen_file_sizes_table)
}


get_terra_log_files  <- function(file_path) {
  ## input bgen is a parameter listed in GEM_Input.param file
  log_files <- system(paste0(gsutil_dir, "gsutil ls -R ",file_path,"/shard-*/resource_usage.log"),intern = TRUE)

  log_file_content <- list()
  
   for(log_file in log_files) {
     log_file_content[[log_file]] <- system(paste0(gsutil_dir, "gsutil cat ",log_file),intern=TRUE)
    
   }

     log_file_data <- list()
   for(log_file in log_files) {
     log_file_data[[log_file]] <- t(sapply(log_file_content[[log_file]][-c(1:4)],function(x){
       tmp<-gsub(pattern="\033[0;0m",replacement="",x,fixed=TRUE)
       tmp2 <- strsplit(gsub("|"," ",tmp,fixed=TRUE),split=" +")[[1]]
       if(length(tmp2)>11) {tmp2 <- tmp2[-c(1)]}
       return(tmp2)
        },USE.NAMES = FALSE))
     colnames(log_file_data[[log_file]]) <- c("cpu-usr","cpu-sys","cpu-idl","cpu-wai","cpu-stl","dsk-read","dsk-writ","mem-used","mem-free","mem-buff","mem-cach")
   }

  return(log_file_data)
}

get_terra_time_data  <- function(file_path) {
  ## input bgen is a parameter listed in GEM_Input.param file
  stdout_files <- system(paste0(gsutil_dir, "gsutil ls -R ",file_path,"/shard-*/stdout"),intern = TRUE)
  
  stdout_file_content <- list()
  
  for(stdout_file in stdout_files) {
    stdout_file_content[[stdout_file]] <- system(paste0(gsutil_dir, "gsutil cat ",stdout_file),intern=TRUE)
    
  }
  
  return(stdout_file_content)
}