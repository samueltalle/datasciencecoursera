rankall <- function(outcome1, num = "best") {
  
  
  ## Read outcome data 
  Var_outcome<-outcome1
  var_num<-num
  
  
  ## initialisation of variables
  hospital<-NULL
  var_hospital<-NULL
  state<-NULL
  
  if (!Var_outcome %in% c("heart attack","heart failure","pneumonia")) {
    stop("Invalid Outcome")
  }
  
  
  #loop for each state to determine hospital name 
  for(n in c(unique(outcome$State))){
    
    #create subset of dataframe with variables we need to work with
    if(Var_outcome=="heart attack"){
      x<-data.frame("Hospital Name"=outcome[,2], "State"=outcome[,7],"Hospital 30-Day Death (Mortality) Rates from Heart Attack"=outcome[,11],stringsAsFactors=FALSE)
      y<-x[x$State==n,]
      y<-y[complete.cases(y),]
      y <- y[order(y$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,y$Hospital.Name),]  
    }
    
    if(Var_outcome=="heart failure"){
      x<-data.frame("Hospital Name"=outcome[,2], "State"=outcome[,7],"Hospital 30-Day Death (Mortality) Rates from Heart Failure"=outcome[,17],stringsAsFactors=FALSE)
      y<-x[x$State==n,]
      y<-y[complete.cases(y),]
      y <- y[order(y$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,y$Hospital.Name),]
    }
    
    if(Var_outcome=="pneumonia"){
      x<-data.frame("Hospital Name"=outcome[,2], "State"=outcome[,7],"Hospital 30-Day Death (Mortality) Rates from Pneumonia"=outcome[,23],stringsAsFactors=FALSE)
      y<-x[x$State==n,]
      y<-y[complete.cases(y),]
      y <- y[order(y$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia ,y$Hospital.Name),]
    }
    
    #retrieve the Name of hospistal
    if(var_num=="best"){
      var_hospital<-y[1,1]
    }else{
      if(var_num=="worst"){
        var_hospital<-y[nrow(y),1]
      } else{
        var_hospital<-y[var_num,1]    
      }  
    }
    
    # Add names of hospital to vector
    hospital<-c(hospital,var_hospital)
    # add name state to vector
    state<-c(state,n)
  }
  
  #create dataframe for result and order the dataframe by state
  result<-data.frame("Hospital"=hospital,"State"=state)
  result[order(result$State),]
  
}
