#' build a new character variabel
#' @param char character variable
#'
#'
#' @export
str_paste0<-function(char){
  newchar<-c()
  for(i in 1:length(char)){
    newchar<-paste0(newchar,char[i])
  }
  return(newchar)
}
