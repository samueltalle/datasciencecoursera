
rankhospital <- function(state, outcome1, num = "best") {

  ## Read outcome data
  var_state<-state
  Var_outcome<-outcome1
  var_num<-num
  
  if(!var_state %in% c(unique(outcome$State))){ 
    stop("Invalid State")
    
  } 
  if (!Var_outcome %in% c("heart attack","heart failure","pneumonia")) {
      stop("Invalid Outcome")  
  }
  
      #create subset of dataframe with variables we need to work on
  if(Var_outcome=="heart attack"){
    x<-data.frame("Hospital Name"=outcome[,2], "State"=outcome[,7],"Hospital 30-Day Death (Mortality) Rates from Heart Attack"=outcome[,11],stringsAsFactors=FALSE)
    y<-x[x$State==state,]
    y<-y[complete.cases(y),]
    y <- y[order(y$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,y$Hospital.Name),]  
  }
      
  if(Var_outcome=="heart failure"){
    x<-data.frame("Hospital Name"=outcome[,2], "State"=outcome[,7],"Hospital 30-Day Death (Mortality) Rates from Heart Failure"=outcome[,17],stringsAsFactors=FALSE)
    y<-x[x$State==state,]
    y<-y[complete.cases(y),]
    y <- y[order(y$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,y$Hospital.Name),]
  }
      
  if(Var_outcome=="pneumonia"){
    x<-data.frame("Hospital Name"=outcome[,2], "State"=outcome[,7],"Hospital 30-Day Death (Mortality) Rates from Pneumonia"=outcome[,23],stringsAsFactors=FALSE)
    y<-x[x$State==state,]
    y<-y[complete.cases(y),]
    y <- y[order(y$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia ,y$Hospital.Name),]
  }
      
  if(var_num=="best"){
    result<-y[1,1]
  }else{
    if(var_num=="worst"){
      result<-y[nrow(y),1]
    } else{
      result<-y[var_num,1]    
      }  
    }
      result
    }
  

