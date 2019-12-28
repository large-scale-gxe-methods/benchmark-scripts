#install.packages("gdata")


get_bgen_files_size <- get_terra_bgen_files_size
get_log_files <- get_terra_log_files

# Laying out multiple plots on a page
# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
# https://stackoverflow.com/questions/30299529/ggplot2-define-plot-layout-with-grid-arrange-as-argument-of-do-call

# Converting G, M, k to a single unit
# https://www.rdocumentation.org/packages/gdata/versions/2.18.0/topics/humanReadable
# https://stackoverflow.com/questions/10910688/converting-kilobytes-megabytes-etc-to-bytes-in-r
convb <- function(x){
  ptn <- "(\\d*(.\\d+)*)(.*)"
  num  <- as.numeric(sub(ptn, "\\1", x))
  unit <- sub(ptn, "\\3", x)             
  unit[unit==""] <- "1" 
  
  mult <- c("B"=1, "1"=1, "K"=1024,"k"=1024, "M"=1024^2, "G"=1024^3)
  num * unname(mult[unit])
}

make_cpu_plot <- function(cpu_table) {
  cpu_table <- data.frame(cbind(time=seq(1,nrow(cpu_table)),
                     usr=as.numeric(cpu_table[,"cpu-usr"]),
                     sys=as.numeric(cpu_table[,"cpu-sys"]),
                     idl=as.numeric(cpu_table[,"cpu-idl"]),
                     wai=as.numeric(cpu_table[,"cpu-wai"]),
                     stl=as.numeric(cpu_table[,"cpu-stl"])))
  
  ggplot(data=cpu_table,aes(x=time)) + 
    geom_line(aes(y=usr,color="usr")) +
    geom_line(aes(y=sys,color="sys")) +
    geom_line(aes(y=idl,color="idl")) +
    geom_line(aes(y=wai,color="wai")) +
    geom_line(aes(y=stl,color="stl")) +
    scale_colour_manual("", values = c("usr"="blue", "sys"="purple","idl"="orange","wai"="pink","stl"="red")) +
    scale_y_continuous("Percent CPU", limits = c(0,100))
}

make_dsk_plot <- function(dsk_table) {
  dsk_table <- data.frame(cbind(time=seq(1,nrow(dsk_table)),
                                read=convb(dsk_table[,"dsk-read"])+1, # +1 to deal with 0's and log scale in plot
                                write=convb(dsk_table[,"dsk-writ"])+1))
  
  ggplot(data=dsk_table,aes(x=time)) + 
    geom_line(aes(y=read,color="read")) +
    geom_line(aes(y=write,color="write")) + scale_colour_manual("", values = c("read"="blue", "write"="purple")) +
    scale_y_continuous("Read/Write in Bytes",trans="log10")
  
}

make_mem_plot <- function(mem_table) { # used  free  buff  cach
  mem_table <- data.frame(cbind(time=seq(1,nrow(mem_table)),
                                used=convb(mem_table[,"mem-used"])+1,
                                free=convb(mem_table[,"mem-free"])+1,
                                buffered=convb(mem_table[,"mem-buff"])+1,
                                cached=convb(mem_table[,"mem-cach"])+1))

  ggplot(data=mem_table,aes(x=time)) + 
    geom_line(aes(y=used,color="used")) +
    geom_line(aes(y=free,color="free")) +
    geom_line(aes(y=buffered,color="buffered")) +
    geom_line(aes(y=cached,color="cached")) +
    scale_colour_manual("", values = c("used"="blue", "free"="purple","buffered"="orange","cached"="red")) +
    scale_y_continuous("Memory in Bytes",trans="log10")
}
  
parameters = c("bgen file",
               "bgen size",
               "workflow CPUs",
               "workflow disk size",
               "workflow memory",
               "workflow maf",
               "workflow stream_snps",
               "run_tests task duration")

plot_log_file_data <- function(parameters_table,log_table,plot_description) {
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(gdata)
  
  main_title <- textGrob(plot_description,gp=gpar(fontsize=12))
  cpu_plot <- make_cpu_plot(log_table[,1:5])
  dsk_plot <- make_dsk_plot(log_table[,6:7])
  mem_plot <- make_mem_plot(log_table[,8:11])

  grid.arrange(main_title,
                tableGrob(parameters_table,theme=ttheme_default(base_size = 8)),
                cpu_plot,
                dsk_plot,
                mem_plot, nrow=5)
  
}

do_plots <- function(path_to_workflow,cpus,disk,memory,maf,stream_snps,run_tests_runtime=NA,plot_description) {
  bgen_files_size <- get_bgen_files_size(path_to_workflow)

  log_file_data <- get_log_files(path_to_workflow)

  for(i in 1:nrow(bgen_files_size)) {
   parameter_values = c("bgen file"=basename(bgen_files_size[i,1]),
                       "bgen size"=bgen_files_size[i,2],
                       "workflow CPUs"=cpus,
                       "workflow disk size"=disk,
                       "workflow memory"=memory,
                       "workflow maf"=maf,
                       "workflow stream_snps"=stream_snps,
                       "run_tests task duration"=run_tests_runtime)
  
  parameters_table <- matrix(parameter_values,ncol=1,dimnames = list(parameters,"values"))
  
  plot_log_file_data(parameters_table=parameters_table,
                     log_table=log_file_data[[i]],
                     plot_description=plot_description)
  }
}
