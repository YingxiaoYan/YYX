#' evaluate time and memory
#' @param beforeloop_code code chunk
#' @param maincode code chunk
#' @param afterloop_code code chunk
#' @param parallel_option none future or do parallel
#' @export

#beforeloop_code="qwe=vector()"
#maincode="for(i in 1:50){set.seed(1000);qwe[i]=rnorm(100000000)[1];cat('\nloop',i)}"
#afterloop_code="cat(qwe)"
#
parallel_loop<-function(beforeloop_code,
                        maincode,
                        afterloop_code,
                        parallel_option=c("none","doParallel","future")){
  result_list<-list()
if( parallel_option=="doParallel"){
  library(doParallel)
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores) # Init cluster
  eval(parse(text=paste("time1=proc.time();",beforeloop_code)))
  eval(parse(text=paste(maincode)))

  eval(parse(text=paste(afterloop_code,";time2=proc.time()")))
  estimated_time_parallel<-time2-time1
  estimated_time_parallel
  stopCluster(cl)
  return(estimated_time_parallel)
}
  if( parallel_option=="future"){
  library(future)
  plan(multisession)
  eval(parse(text=paste("time1=proc.time();",beforeloop_code)))
  eval(parse(text=paste(maincode)))

  eval(parse(text=paste(afterloop_code,";time2=proc.time()")))
  estimated_time_future<-time2-time1
  estimated_time_future
  return(estimated_time_future)
  a %<-% {
    Sys.sleep(3)
    a <- 1
  }
  b %<-% {
    Sys.sleep(3)
    b <- 2
  }

  a + b
  }
  if( parallel_option=="none"){
    eval(parse(text=paste("time1=proc.time();",beforeloop_code)))
    find_identical_strings(maincode,specific="){")
    eval(parse(text=paste(maincode)))

    eval(parse(text=paste(afterloop_code,";time2=proc.time()")))
    estimated_time_sequential<-time2-time1
    estimated_time_sequential
    return(estimated_time_sequential)
  }
}
