#' evaluate time and memory
#' @param beforeloop_code code chunk
#' @param maincode code chunk
#' @param afterloop_code code chunk
#' @param parallel_option none future or do parallel
#' @export

#beforeloop_code="qwe=vector();qwr<-vector()"
#maincode=" for( j  in 1:5 ){  set.seed(1000); qwe[j]=rnorm(100);qwr[j]=rnorm(1);cat('\nloop',j)  }"
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
  registerDoParallel(cl)

  char_sep=strsplit(maincode,"")[[1]]

  position=find_identical_strings(maincode,specific="{")$`{`[1]

  char_front<-char_sep[1:position]
  char_behind<-char_sep[(position+1):(length(char_sep))]
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


  char_front_new<-str_paste0(c(char_for,char_infor, char_doPar,"{"))
  char_new<-str_paste0(c(char_front_new,char_behind0))
  cat("Your new foreach loop by doparallel is ",char_new, "\n",
      "Note that in your code you will get only one output from each loop.
       Please format your code to put all items in a list in each loop and have return(the_list) in the end.
       The for

       "
      )
   b<-menu(c("Yes","No"),
           graphics = F,
           "Do you want to stop now and reformat your code a bit?")
  if(b==1){stop("Change your code first.")}
  eval(parse(text=paste("time1=proc.time();",beforeloop_code)))
  eval(parse(text=paste("result=",char_new)))

  eval(parse(text=paste(afterloop_code,";time2=proc.time()")))
  estimated_time_parallel<-time2-time1
  estimated_time_parallel
  stopCluster(cl)
  return(estimated_time_parallel)
}
  if( parallel_option=="future"){
  library(future)
  plan(multisession)

  #donkey<-new.env()
  char_sep=strsplit(maincode,"")[[1]]
  position=find_identical_strings(maincode,specific="{")$`{`[1]

  char_front<-char_sep[1:position]
  char_behind<-char_sep[(position+1):(length(char_sep)-1)]
  char_front0<-str_paste0(char_front)
  char_behind0<-str_paste0(char_behind)

  position_length=find_identical_strings(maincode,specific=" in ")$` in `[1]
  char_length<-char_front[(position_length+4):(length(char_front)-2)]
  char_length0<-str_paste0(char_length)
  char_name<-eval(parse(text=paste("as.character(",char_length0,")")))

  eval(parse(text=paste("time1=proc.time();",beforeloop_code)))
  rty<-new.env()

  for(j in char_name){
    #proc.time()
  # rty[[i]]%<-%
  #    {
       #  set.seed(1000)
        #qwe[i]<-rnorm(10000)[1]
        #cat('\nloop',i)
        eval(parse(text=paste(char_behind0)))
        overall<-list()
        listname<-c()
        for(i in 1:(length(ls()))+1){
          if(!identical(ls()[i],"overall")&!identical(ls()[i],"i")&!identical(ls()[i],"listname")){
          overall[[i]]<-get(ls()[i])
          listname<-c(listname,ls()[i])


         # names(overall[[i]])<-eval(parse(text=paste("as.character(",ls()[i],")")))
        #   names(overall[[i]])<-as.character(eval(parse(text=paste("ls()[i]"))))
        #  names(overall[[i]])<-as.character(ls()[i])
              }
        }


        overall_new<-list()
        k=0
        for(i in 1:length(overall))
        if(!is.null(overall[[i]])){
          k=k+1
          overall_new[[k]]<-overall[[i]]
        }
        names(overall_new)<-listname
        overall_new
    #  }
  }




  }



  }
v<-as.list(rty)
Reduce(c,v)
  eval(parse(text=paste(afterloop_code,";time2=proc.time()")))
  estimated_time_future<-time2-time1
  estimated_time_future
  return(estimated_time_future)

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
