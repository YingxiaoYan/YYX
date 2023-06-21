#' Transform your loop text in for doparallel format
#' @param x code chunk where you wrote a for loop
#' @export
#'
#'
transform_loop_text_doParallel<-function(x){
  cat("\nWhen saving output, your code should have format,e.x. a[i]<-1,not a<-c(a,1). \n")
  cat("Each line in your loop should have a ';'", "\n")
######
  ##detect ythe c(,)

######
#  a<-menu(c("Yes","No"),
#          graphics = F,
#          "Do you want to stop now and reformat your code a bit?")
#  if(a==1){stop("Change your code first.")}
maincode=x
  char_sep=strsplit(maincode,"")[[1]]

  position=find_identical_strings(maincode,specific="{")$`{`[1]

  char_front<-char_sep[1:position]
  char_behind<-char_sep[(position+1):(length(char_sep)-1)]
  char_front0<-str_paste0(char_front)
  char_behind0<-str_paste0(char_behind)

  position_for_front<-find_identical_strings(maincode,specific="for")$`for`[1]

  position_for_behind<-find_identical_strings(maincode,specific="(")$`(`[1]

  char_for<-char_front[position_for_front:position_for_behind]
  char_for<-str_paste0(char_for)
  char_for<-change_name(char_for,"for","foreach")

  position_infor_front<-find_identical_strings(maincode,specific="(")$`(`[1]

  position_infor_behind<-find_identical_strings(maincode,specific="{")$`{`[1]


  char_infor<-char_front[(position_infor_front+1):(position_infor_behind-1)]
  char_infor<-str_paste0(char_infor)
  char_infor<-change_name(char_infor," in ", " = ")
  char_doPar<-"%dopar%"
  char_infor_tight<-change_name(char_infor," ", "")


  char_front_new<-str_paste0(c(char_for,char_infor, char_doPar,"{"))
  char_new<-str_paste0(c("result<-",char_front_new,char_behind0,";return(list("))##"items))}"
  char_new<-change_name(char_new," ","")
  ijz_whatever<-str_split(char_infor_tight,"")[[1]][1]
  ijz_whatever_squarebracket<-str_paste0(c("[",ijz_whatever,"]"))
  ijz_whatever_squarebracket_db<-str_paste0(c("[[",ijz_whatever,"]]"))
  ijz_whatever_matrix_row<-str_paste0(c("[",ijz_whatever,","))
  ijz_whatever_matrix_column<-str_paste0(c(",",ijz_whatever,"]"))


  ## get bad position 2

  bad_position_2<-find_identical_strings(char_new,specific=ijz_whatever_squarebracket_db)[[1]]


  bad_position_24<-bad_position_2+4
  bad_position_2_vec<-vector()

  if(!is.null(bad_position_2)){
  for(i in 1:length(bad_position_2)){
    bad_position_2_vec<-c(bad_position_2_vec,bad_position_2[i]:bad_position_24[i])
  }
  }else{bad_position_2_vec<-c()}

  ## get bad position 1
  invisible(
  bad_position_1<-find_identical_strings(char_new,specific=ijz_whatever_squarebracket)[[1]]

  )
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
    char_new<-strsplit(char_new,"")[[1]]
  char_new<-char_new[-bad_position_vec]
  }
  char_new<-str_paste0(char_new)

  ##get bad position matrix and column

  bad_position_column<-find_identical_strings(char_new,
                                              specific=ijz_whatever_matrix_column)[[1]]

  bad_position_column_3<-bad_position_column+2
  bad_position_column_vec<-vector()

  if(!is.null(bad_position_column)){
    for(i in 1:length(bad_position_column)){
      bad_position_column_vec<-c(bad_position_column_vec,bad_position_column[i]+1)
    }
  }else{bad_position_column_vec<-c()}

  if(length(bad_position_column_vec)!=0){
    char_new<-strsplit(char_new,"")[[1]]
    char_new<-char_new[-bad_position_column_vec]
  }
  char_new<-str_paste0(char_new)

  bad_position_row<-find_identical_strings(char_new,specific=ijz_whatever_matrix_row)[[1]]

   bad_position_row_3<-bad_position_row+2
  bad_position_row_vec<-vector()

  if(!is.null(bad_position_row)){
    for(i in 1:length(bad_position_row)){
      bad_position_row_vec<-c(bad_position_row_vec,bad_position_row[i]+1)
    }
  }else{bad_position_row_vec<-c()}




  if(length(bad_position_row_vec)!=0){
    char_new<-strsplit(char_new,"")[[1]]
    char_new<-char_new[-bad_position_row_vec]
  }
  char_new<-str_paste0(char_new)


  str_paste0(char_new)



  ### the we need to isolate items between ; and the bad postions and aim to return it in th elist
  ### item should be in the format that is "qwe=qwe"

  char_new<-str_paste0(c(char_new,"items","))}"))
  char_new<-change_name(char_new,";;",";")
  "%doVersion%" <- get("%dopar%")
  cat("\n\nYour new foreach loop by doparallel is \n",char_new, "\n",
      "\nNote that in your code you will get only one output from each loop. Please adjust your code to put thing you want in the list, replacing items in the end.\n
       "
  )

#return(char_new)
}
