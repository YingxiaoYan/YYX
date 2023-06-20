#' evaluate time and memory
#' @param code
#' @param how_many_times
#' @export
#' @example
## code0="rnorm(1000000)"
## code1="rnorm(1000000)
##       rnorm(100000)"
##       code2="rnorm(1000000);rnorm(1000000)"
##       code3="fff=rnorm(10000);ee=fff"
##       code4="TriplotGUI::TriplotGUI_shiny()"
##  code5="plan(multisession);a %<-% {Sys.sleep(1);a <- 1};b %<-% {Sys.sleep(1);b <- 2}"

time_evaluation<-function(code,
                          how_many_times=1,
    memory_eval=c("object_size")){

  time_eval=c("system_time",
              "nanotime",
              "microbenchmark",
              "Rprof")
result_list<-list()
  cat("\nThe first time is excluded from average run times \n")
  if(!any(time_eval%in%c("system_time",
                        "nanotime",
                        "microbenchmark",
                        "Rprof"))){
    stop("This method is not included")
  }
if("system_time"%in%time_eval){
  cat("\nsystem.time()\n" )
  systime_record<-c(0,0,0)
  for(i in 1:(how_many_times+1)){
    ## is systime addable?
    ## There is the effect of adding 2 together vs {}

    if(i==1){
      systime_record_firstime<-eval(parse(text=paste("system.time({",
                                                      code,
                                                      "})")))[1:3]
      systime_matrix<-systime_record_firstime

    }else{
      systime_tmp<-eval(parse(text=paste("system.time({",
                                         code,
                                         "})")))[1:3]
      systime_record<-systime_record+systime_tmp
      systime_matrix<-rbind(systime_matrix,systime_tmp)

    }
  }

  rownames(systime_matrix)<-c(1:(how_many_times+1))
  library(ggplot2)
  systime_plot<-ggplot()+
    geom_point(aes(x=1:(how_many_times+1),
                   y=systime_matrix[,3]))+
    scale_x_continuous(name="Running times")+
    ylab("system.time time")
  systime_record_avg<-systime_record/how_many_times
#  result_list$systime_plot<-systime_plot
  result_list$systime_firstime<-systime_record_firstime
  result_list$systime_average<-systime_record_avg

}
  if("nanotime"%in%time_eval){
    cat("\nget_nanotime()\n" )
  ############# get nano time
  library(microbenchmark)
  nanotime_record<-double(how_many_times+1)
  for(i in 1:(how_many_times+1)){

    eval(parse(text=paste("nanotime_start<-get_nanotime();",
                          code,";","nanotime_end<-get_nanotime()")))

    nanotime_record[i] <- -(nanotime_start-nanotime_end)
  }

  library(ggplot2)
  nanotime_plot<-ggplot()+
    geom_point(aes(x=1:(how_many_times+1),
                   y=nanotime_record/1000000000))+
    scale_x_continuous(name="Running times")+
    ylab("get_nanotime time")

  nanotime_record_firstime<-nanotime_record[1]/1000000000
  nanotime_record_avg<-mean(nanotime_record[2:(how_many_times+1)])/1000000000


  #result_list$nanotime_plot<-nanotime_plot
  result_list$nanotime_firstime<-nanotime_record_firstime
  result_list$nanotime_average<-nanotime_record_avg

}
  if ("microbenchmark"%in%time_eval){
  library(microbenchmark)
  library(ggplot2)
    cat("\nmicrobenchmark()" )

    #code_original<-code
  #  code<-change_name(code,";",",")
  #code<-gsub(" ","",code)
  time=how_many_times+1
 # tryCatch(  {
   microbench_time<-eval(parse(text=paste0("microbenchmark({",code,"}",
                          ",times=",time,
                          ")")))
 # },error=function(e){})

   time_temp<-microbench_time$time/1000000000
   microbench_time_firstime<-time_temp[1]
   microbench_time_avg<-mean(time_temp[2:(how_many_times+1)])
   microbench_time_plot<-ggplot()+
     geom_point(aes(x=1:(how_many_times+1),
                    y=time_temp))+
     scale_x_continuous(name="Running times")+
     ylab("microbenchmark time")
   #result_list$microbench_plot<-microbench_time_plot
   result_list$microbench_firstime<-microbench_time_firstime
   result_list$microbench_average<-microbench_time_avg

   ###################
}
  if ("Rprof"%in%time_eval){
    library(profr)
    cat("\nRprof()\n")
### why show up so much

    eval(parse(text=paste("Rprof('code.out', interval = 0.001);",
                          code,
                          ";#Rprof_output1<-summaryRprof(filename = 'code.out');
                          Rprof(NULL);
                          Rprof_output <- parse_rprof('code.out')"

      )))
    file.remove('code.out')

result_list$Rprof_output<-Rprof_output
if(nrow(Rprof_output)!=0){
  result_list$Rprof_time<-Rprof_output[nrow(Rprof_output),"end"]-Rprof_output[1,"start"]}
  }
#  if ("profvis"%in%time_eval){
#    library(profvis)
#    a<-profvis({
#      eval(parse(text=paste(code)))
#    })
  a<-systime_matrix[2:(how_many_times+1),3]
  b<-nanotime_record[2:(how_many_times+1)]/1000000000
  d<-time_temp[2:(how_many_times+1)]
  result_list$systime_vector<-systime_matrix[1:(how_many_times+1),3]
  result_list$nanotime_vector<-nanotime_record[1:(how_many_times+1)]/1000000000
  result_list$microbench_vector<-time_temp[1:(how_many_times+1)]
  method<-c(rep("system.time",how_many_times),rep("get_nanotime",how_many_times),rep("microbenchmark",how_many_times))
  time<-c(a,b,d)
  data=data.frame(time,method)
histplot<-
  ggplot(data=data,
         mapping=aes(x=time))+
    geom_density(aes(fill = method),
                 alpha = 0.5)+ ##the transparency
  xlab("time/s")
  result_list$dotplot<-ggpubr::ggarrange(systime_plot,nanotime_plot,microbench_time_plot,nrow=3)
  result_list$histplot<-histplot
return(result_list)
}
