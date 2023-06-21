#' evaluate memory useage of object and function Note that you do not do anything with the global environemnt.
#' running the code without running it
#' @param code code chunk
#' @param option "mem_change","peakRAM"
#' @export
#code0="rnorm(1000000)"
#code1="a=rnorm(1000000)"
#code2="a=rnorm(1000000);b=rnorm(1000000)"
#code3="fff=rnorm(10000);ee=fff"
#code4="TriplotGUI::TriplotGUI_shiny()"
#code5="plan(multisession);a %<-% {Sys.sleep(1);a <- 1};b %<-% {Sys.sleep(1);b <- 2}"

memory_evaluation<-function(code,
                            option=c("mem_change","peakRAM")){
  library(pryr)
  library(peakRAM)
  result_list<-list()
## cat("Do not name any of your variable as mmmmm")
maincode<-code
if(missing(option)){option="mem_change"}
if(!any(option%in%c("mem_change","peakRAM"))){
  stop("option not supported")}
if(option=="mem_change"){
cat("1.By gc(verbose=T), memory usage before running your code: \n")
invisible(gc(verbose=T))

cat("\n2.By pryr::mem_used(), before running your code, your assumed memory usege is",mem_used()/1000000, "Mb\n")

memchange<-pryr::mem_change(eval(parse(text=paste(
  "mmmmm<-gc()[2,1];",maincode,";mmmmm<-mmmmm-gc()[2,1]"))))

if(mmmmm<0){
  cat("\n3. By pryr::mem_change(), you occupied ",memchange/1000000,"more Mb after your code \n")
}
if(mmmmm>0){
  cat("\n3.By pryr::mem_change(), you freed ",memchange/1000000,"Mb after your code \n")
}
if(mmmmm==0){cat("\n 3.By pryr::mem_change(), Memory usage is not changed \n")}

cat("\n4.By gc(verbose=T), memory usage before running your code: \n")
invisible(gc(verbose=T))

cat("\n5.By pryr::mem_used(), after running your code, your assumed memory useage is",mem_used()/1000000, "Mb\n")
cat("\n")
result_list$memchange<-memchange
result_list$gcchange<- -mmmmm


}
if(option=="peakRAM"){
  cat("\n1.By gc(verbose=T), memory usage before running your code: \n")
  invisible(gc(verbose=T))
  ramchange<-peakRAM(eval(parse(text=paste(
    "{mmmmm<-gc()[2,1];",maincode,";mmmmm<-mmmmm-gc()[2,1]}"))))
  if(ramchange[2]>=0){
  cat("\n2.By peakRAM(), after runing your code, your will use",ramchange[1,3],"Mb ram to save your object. \n Your peak ram use is",ramchange[1,4],"Mb.\n")
  }else{
    cat("\n2.By peakRAM(), after runing your code, your will release",-ramchange[1,3],"Mb ram. \n Your peak ram use is",ramchange[1,4],"Mb.\n")
  }

  cat("\n3.By gc(verbose=T), memory usage before running your code: \n")
  invisible(gc(verbose=T))
  cat("\n")

result_list$ram_occupied<-ramchange[3]
result_list$ram_used<-ramchange[4]
}
return(result_list)


}


