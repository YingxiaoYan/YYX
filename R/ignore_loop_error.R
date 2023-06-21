#' This is for ignore loop errors (caused by extreme scenario of resampling or something else).
#' You should run the code and know for sure the error is not a "hard error"
#' @param rep  loop number
#' @param empty_name  chunk of code, building empty object for saving results
#'                                     the code should be written in "..
#'                                                                  .. " With lines separate
#' @param some_operation_before chunk of code, do some manipualtion before the loop starts
#' @param main_function chunk of code, the main function to compute the result
#'                                     the code should be written in "..
#'                                                                  .. " With lines separate
#' @param some_operation_after chunk of code, do some manipualtion before the loop starts
#' @param record_values chunk of code, to save the result
#'                                     the code should be written in "..
#'                                                                  .. " With lines separate
#' @param output chunk of code, A list format the variables names you want to save as output "", should be something in the recorded values
#' @return result
#' @export
#
# Note that in the "...", if there is a charater you need to specifiy, use ' '
# Note that you must guarantee that the

##################################################################################################
## @example
##################################################################################################
###example
#library(MUVR)
#library(StatTools)
#rep=2
#trick=5
#dataX=Xotu
#dataY=Yotu
#empty_name<-"vector_1<-vector()
#            vector_2<-vector()"
#some_operation_before<-"dataYPerm = StatTools::sampling_from_distribution(dataY)"
#main_function<-"perm=MUVR(X=dataX,Y=dataYPerm,method='RF',
#                DA=T, modReturn = T, nRep =trick, nOuter=3,varRatio=0.3)
#               if(rep==sample(c(1:2),1)){trick=trick-1; stop('deliberatly give error')}"
#record_values<-"vector_1<- c(vector_1,perm$ber['min'])
#vector_2<- c(vector_2,perm$miss['min'])"
#output<-"list(vector_1,vector_2)"
#sss<-ignore_loop_error(rep,
#                 some_operation_before,
#                 empty_name,
#                 main_function,
#                 record_values,
#                NULL,
#                 output)

ignore_loop_error<-function(rep,
                            empty_name,
                            some_operation_before=NULL,
                            main_function,
                            record_values,
                            some_operation_after=NULL,
                            output){


  result<-list()
  pp<-1
  p<-1
  ########################################
  #### empty name
  eval(parse(text=paste(empty_name)))
 #eval(parse(text=paste(empty_name)))
  ###################################################33
 # print(environment())
  while(pp<=rep){
    p<-pp
    cat("/loop",pp,"of",rep)
  #  print(environment())
    eval(parse(text=paste(some_operation_before,";print(environment())")))

    tryCatch(  {#pp<-p
    ###################################################################
    ##main function
    pp<- p
   # print(environment())
    eval(parse(text=paste(main_function)))
    ################################################################
    cat(perm$ber['min'])
    pp<- p+1
    ######################################################################
    ## recording
    },error=function(e){})




    if(pp==p+1){

      eval(parse(text=paste(record_values)))
    }
    cat(vector_1,"\n")
    cat(vector_2,"\n")

    ####################################################################
  }
cat(vector_1,"\n")
cat(vector_2,"\n")
#print(environment())
  for(i in 1:length(eval(parse(text=paste(output))))){
 #   print(environment())
    result[[i]]<-eval(parse(text=paste(output)))[[i]]
  }
  text=paste0("c('",substr(output, start=6, stop=nchar(output)-1),"')")
  text=change_name(text,",","','")
  if(!is.null(names(eval(parse(text=paste(output)))))){
  names(result)<-eval(parse(text=text))
  }

  return(result)
}

