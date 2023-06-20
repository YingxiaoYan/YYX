#' evaluate memory useage of object and function
#' @param code
#' @export
#code0="rnorm(1000000)"
#code1="a=rnorm(1000000)"
#code2="a=rnorm(1000000);b=rnorm(1000000)"
#code3="fff=rnorm(10000);ee=fff"
#code4="TriplotGUI::TriplotGUI_shiny()"
#code5="plan(multisession);a %<-% {Sys.sleep(1);a <- 1};b %<-% {Sys.sleep(1);b <- 2}"

memory_evaluation<-function(code){
  result_list<-list()
cat("Do not name any of your variable as mmmmm")
code
mem_change<-pryr::mem_change(eval(parse(text=paste("mmmmm<-gc()[2,1];",code,"mmmmm<-mmmmm-gc()[2,1]"))))
if(mmmmm<0){
  cat("\nincrease memoty usage by",mem_change,"\n")
}
if(mmmmm>0){
  cat("\nreduce memoty usage by",mem_change,"\n")
}
if(mmmmm==0){cat("\nMemory usage is not changed\n")}
return(result_list)
}
gc(verbose=T)

