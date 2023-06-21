#' A string vector
#'@param x a string vector
#'@param n how long
#'@param specific specific strings
#'@export
find_identical_strings<-function(x,
                                 n=2,
                                 specific=NULL){
  char_sep=strsplit(x,"")[[1]]


  if(!is.null(specific)){
    specific_char_sep=strsplit(specific,"")[[1]]
    n=length(specific_char_sep)

    new_string_matrix<-matrix("0",length(char_sep)+1-n,n)
    for(j in 1:ncol(new_string_matrix)){
      new_string_matrix[,j]<-char_sep[j:(nrow(new_string_matrix)+j-1)]
    }
    new_string_matrix_save<-new_string_matrix


    position_list_n<-list()

    for(i in 1:nrow(new_string_matrix)) {
      position_list_n[[i]]<-vector()
    }

    for(i in 1:nrow(new_string_matrix)) {
      position_list_n[[i]]<-c(position_list_n[[i]],i)
      if(i!=nrow(new_string_matrix)){
        for(j in 1:(nrow(new_string_matrix)-i)) {
          if(identical(new_string_matrix[i,],new_string_matrix[i+j,])){
            position_list_n[[i]]<-c(position_list_n[[i]],(i+j-1)+1)
          }
        }
      }

    }
    all_names_n<-apply (new_string_matrix,1,str_paste0)
    names(position_list_n)<- all_names_n

    unique_names_n<-unique(all_names_n)
    nameposition<-vector(length=length(unique_names_n))
    for(i in 1:length(unique_names_n)){
      nameposition[i]<-which(all_names_n==unique_names_n[i])[1]
    }
    position_list_n<-position_list_n[nameposition]

    specific_char_sep_restr<-str_paste0(specific_char_sep)
      if(specific_char_sep_restr%in%names(position_list_n)){
        position_list_n<-position_list_n[specific_char_sep_restr]
      }else{position_list_n=NULL
        cat("no match strings")}

  }



  if(!is.null(n)&is.null(specific)){

    ## base on n separate the string to small length matrix
    new_string_matrix<-matrix("0",length(char_sep)+1-n,n)
  for(j in 1:ncol(new_string_matrix)){
      new_string_matrix[,j]<-char_sep[j:(nrow(new_string_matrix)+j-1)]
  }
    new_string_matrix_save<-new_string_matrix


    position_list_n<-list()

    for(i in 1:nrow(new_string_matrix)) {
      position_list_n[[i]]<-vector()
    }

    for(i in 1:nrow(new_string_matrix)) {
      position_list_n[[i]]<-c(position_list_n[[i]],i)
      if(i!=nrow(new_string_matrix)){
        for(j in 1:(nrow(new_string_matrix)-i)) {
          if(identical(new_string_matrix[i,],new_string_matrix[i+j,])){
            position_list_n[[i]]<-c(position_list_n[[i]],(i+j-1)+1)
          }
        }
      }

    }
    all_names_n<-apply (new_string_matrix,1,str_paste0)
    names(position_list_n)<- all_names_n

    unique_names_n<-unique(all_names_n)
    nameposition<-vector(length=length(unique_names_n))
    for(i in 1:length(unique_names_n)){
      nameposition[i]<-which(all_names_n==unique_names_n[i])[1]
    }
    position_list_n<-position_list_n[nameposition]

    remove_place<-c()
    for(i in 1:length(position_list_n)){
   if(length(position_list_n[[i]])==1){
     remove_place<-c(remove_place,i)
   }
    }
    remove_place_func <- function(place) {
      position_list_n[-place]
    }

    position_list_n<- remove_place_func(remove_place)

  }
if(length(position_list_n)==0){
  cat("There is no matched place")
}else{return(position_list_n)}

}
