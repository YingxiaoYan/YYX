#' Transform your loop text in for future
#' @param x code chunk where you wrote a for loop
#' @export
#'
#'
## maincode= " for( j  in 1:5 ){  set.seed(1000); qaz[,j]=qwed;qas[j,r]=qrfs;;qwe[[j]]=rnorm(100);qwr[j]=rnorm(1);cat('loop',j)}"
transform_loop_text_future<-function(x){
  cat(" ")

  cat("\nWhen saving output, your code should have format,e.x. a[i]<-1,not a<-c(a,1). \n")
  cat("Each line in your loop should have a ';'", "\n")
#a<-menu(c("Yes","No"),
#        graphics = F,
#        "Do you want to stop now and reformat your code a bit?")
#if(a==1){stop("Change your code first.")}
maincode=x
##maincode=change_name(maincode," ","")
char_sep=strsplit(maincode,"")[[1]]

position=find_identical_strings(maincode,specific="{")$`{`[1]
char_front<-char_sep[1:(position)]
char_behind<-char_sep[(position+1):(length(char_sep))]
char_front0<-str_paste0(char_front)
char_behind0<-str_paste0(char_behind)

position_in<-find_identical_strings(maincode,specific=" in ")$` in `[1]
char_infront<-char_sep[1:(position_in-1)]
char_infront0<-str_paste0(char_infront)
char_infront0<-change_name(char_infront0," ","")

char_loopsign<-strsplit(char_infront0,"")[[1]][-c(1:4)]


## maincode= " for( j  in 1:5 ){  set.seed(1000); qaz[,j]=qwed;qas[j,r]=qrfs;;qwe[[j]]=rnorm(100);qwr[j]=rnorm(1);cat('loop',j)}"
char_loopnumber<-char_front[(position_in+4):(length(char_front)-1)]
char_loopnumber<-str_paste0(char_loopnumber)
char_loopnumber<-change_name(char_loopnumber," ","")
char_loopnumber<-strsplit(char_loopnumber,"")[[1]]
char_loopnumber<-char_loopnumber[1:(length(char_loopnumber)-1)]
char_loopnumber<-str_paste0(char_loopnumber)
char_loopnumber<-as.character(eval(parse(text=paste(char_loopnumber))))
char_loopnumber<-paste(char_loopnumber,collapse="','")
char_loopnumber<-paste0("'",char_loopnumber,"'")


ijz_whatever_squarebracket<-str_paste0(c("[",char_loopsign,"]"))
ijz_whatever_squarebracket_db<-str_paste0(c("[[",char_loopsign,"]]"))
ijz_whatever_matrix_row<-str_paste0(c("[",char_loopsign,","))
ijz_whatever_matrix_column<-str_paste0(c(",",char_loopsign,"]"))
char_behind_adjust<-char_behind0

bad_position_2<-find_identical_strings(char_behind_adjust,specific=ijz_whatever_squarebracket_db)[[1]]
bad_position_24<-bad_position_2+4
bad_position_2_vec<-vector()

if(!is.null(bad_position_2)){
  for(i in 1:length(bad_position_2)){
    bad_position_2_vec<-c(bad_position_2_vec,bad_position_2[i]:bad_position_24[i])
  }
}else{bad_position_2_vec<-c()}

## get bad position 1
bad_position_1<-find_identical_strings(char_behind_adjust,specific=ijz_whatever_squarebracket)[[1]]
bad_position_13<-bad_position_1+2
bad_position_1_vec<-vector()


if(!is.null(bad_position_1)){
  for(i in 1:length(bad_position_1)){
    bad_position_1_vec<-c(bad_position_1_vec,bad_position_1[i]:bad_position_13[i])
  }
}else{bad_position_1_vec<-c()}


bad_position_vec<-c(bad_position_1_vec,bad_position_2_vec)
bad_position_vec<-unique(bad_position_vec)
######

if(length(bad_position_vec)!=0){
  char_behind_adjust<-strsplit(char_behind_adjust,"")[[1]]
  char_behind_adjust<-char_behind_adjust[-bad_position_vec]
}
char_behind_adjust<-str_paste0(char_behind_adjust)

##get bad position matrix and column
bad_position_column<-find_identical_strings(char_behind_adjust,
                                            specific=ijz_whatever_matrix_column)[[1]]
bad_position_column_3<-bad_position_column+2
bad_position_column_vec<-vector()

if(!is.null(bad_position_column)){
  for(i in 1:length(bad_position_column)){
    bad_position_column_vec<-c(bad_position_column_vec,bad_position_column[i]+1)
  }
}else{bad_position_column_vec<-c()}

if(length(bad_position_column_vec)!=0){
  char_behind_adjust<-strsplit(char_behind_adjust,"")[[1]]
  char_behind_adjust<-char_behind_adjust[-bad_position_column_vec]
}
char_behind_adjust<-str_paste0(char_behind_adjust)

bad_position_row<-find_identical_strings(char_behind_adjust,specific=ijz_whatever_matrix_row)[[1]]
bad_position_row_3<-bad_position_row+2
bad_position_row_vec<-vector()

if(!is.null(bad_position_row)){
  for(i in 1:length(bad_position_row)){
    bad_position_row_vec<-c(bad_position_row_vec,bad_position_row[i]+1)
  }
}else{bad_position_row_vec<-c()}




if(length(bad_position_row_vec)!=0){
  char_behind_adjust<-strsplit(char_behind_adjust,"")[[1]]
  char_behind_adjust<-char_behind_adjust[-bad_position_row_vec]
}
char_behind_adjust<-str_paste0(char_behind_adjust)



#char_behind_adjust<-str_split(char_behind_adjust,"")[[1]]




char_fu_start<-"new_env <- new.env();"

char_brand_new<-paste(char_fu_start,"\n",
      char_infront0," in c(",char_loopnumber,")){",
      "new_env[[",char_loopsign,"]] %<-%{" ,char_behind_adjust,";return(list(items))};","\n","new_env <- as.list(new_env);")
char_brand_new<-str_paste0(char_brand_new)
char_brand_new<-change_name(char_brand_new,";;",";")
cat("\nThe future code looks like: \n",char_brand_new,"\n"
)

cat("\nNote that in your code you will get only one output from each loop. Please adjust your code to put thing you want in the list, replacing items in the end.\n"
)


}

