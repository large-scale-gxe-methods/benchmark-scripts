

### December 27, 2019
# > system("/Users/amanning/google-cloud-sdk/bin/gsutil cat gs://fc-5fe875e4-6194-4747-8b9d-c2e459422651/00df1a40-9f69-475f-a952-dc7859c9f076/run_GEM/52ae7ca0-9932-40eb-96c1-cdabbc8e5e88/call-run_tests/shard-1/GEM_Input.param",intern=T)
# [1] "SAMPLE_ID_HEADER"                                                                                                                                                                        
# [2] "id"                                                                                                                                                                                      
# [3] " PHENOTYPE"                                                                                                                                                                              
# [4] "0"                                                                                                                                                                                       
# [5] " PHENO_HEADER"                                                                                                                                                                           
# [6] "bp_sim"                                                                                                                                                                                  
# [7] " COVARIATES_HEADERS"                                                                                                                                                                     
# [8] "smk_sim sex age"                                                                                                                                                                         
# [9] " MISSING"                                                                                                                                                                                
# [10] "NA"                                                                                                                                                                                      
# [11] " ROBUST"                                                                                                                                                                                 
# [12] "1"                                                                                                                                                                                       
# [13] " STREAM_SNPS"                                                                                                                                                                            
# [14] "20"                                                                                                                                                                                      
# [15] " NUM_OF_INTER_COVARIATE"                                                                                                                                                                 
# [16] "1"                                                                                                                                                                                       
# [17] " LOGISTIC_CONVERG_TOL"                                                                                                                                                                   
# [18] "1.0E-6"                                                                                                                                                                                  
# [19] " DELIMINATOR"                                                                                                                                                                            
# [20] ","                                                                                                                                                                                       
# [21] " GENO_FILE_PATH"                                                                                                                                                                         
# [22] "/cromwell_root/fc-5fe875e4-6194-4747-8b9d-c2e459422651/872089e9-72ac-4e1b-bdc5-dcdaa8c41660/convert/0e56a186-5382-46ca-8eed-f195a945d594/call-vcf_to_bgen/shard-1/chr10_maf01.dose.bgen" 
# [23] " PHENO_FILE_PATH"                                                                                                                                                                        
# [24] "/cromwell_root/fc-5fe875e4-6194-4747-8b9d-c2e459422651/1000G/smk_bp_sim_phenos.csv"                                                                                                      
# [25] " SAMPLE_FILE_PATH"                                                                                                                                                                       
# [26] "/cromwell_root/fc-5fe875e4-6194-4747-8b9d-c2e459422651/c139c633-e6a5-4aa3-85c5-e6a1d0830f95/convert/9ea0205f-9607-4180-8417-fe92e997e0f4/call-vcf_to_bgen/shard-0/chr1_maf01.dose.sample"
# [27] " OUTPUT_PATH"                                                                                                                                                                            
# [28] "gem_res"       

get_terra_bgen_files_size <- function(file_path) {
  ## input bgen is a parameter listed in GEM_Input.param file
 param_files <- system(paste0("/Users/amanning/google-cloud-sdk/bin/gsutil ls -R ",file_path,"/shard-*/GEM_Input.param"),intern = TRUE)

 param_files_content <- system(paste0("/Users/amanning/google-cloud-sdk/bin/gsutil cat ",paste(param_files,collapse=" ")),intern=T)
 #param_files_content
 bgen_files <- sub("/cromwell_root/","gs://",param_files_content[grep(pattern=" GENO_FILE_PATH",param_files_content)+1])

 bgen_file_sizes <- system(paste0("/Users/amanning/google-cloud-sdk/bin/gsutil du -h ",paste(bgen_files,collapse=" ")),intern=TRUE)

 bgen_file_sizes_table <- t(sapply(bgen_file_sizes,function(x){strsplit(x,split="  ")[[1]][c(2,1)]}))
 row.names(bgen_file_sizes_table) <- param_files
 return(bgen_file_sizes_table)
}


get_terra_log_files  <- function(file_path) {
  ## input bgen is a parameter listed in GEM_Input.param file
  log_files <- system(paste0("/Users/amanning/google-cloud-sdk/bin/gsutil ls -R ",file_path,"/shard-*/resource_usage.log"),intern = TRUE)

  log_file_content <- list()
  
   for(log_file in log_files) {
     log_file_content[[log_file]] <- system(paste0("/Users/amanning/google-cloud-sdk/bin/gsutil cat ",log_file),intern=TRUE)
    
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