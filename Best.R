
best <- function(state, outcome1) {
  ## Read outcome data
  var_state<-state
  Var_outcome<-outcome1
  
  #transform  rates as numeric. Important for the first running of program after loading dataset
  outcome[, 11] <- as.numeric(outcome[, 11])
  outcome[, 17] <- as.numeric(outcome[, 17])
  outcome[, 23] <- as.numeric(outcome[, 23])
  
  
  ## Check if state and outcome are valid
  if(!var_state %in% c(unique(outcome$State))){ 
    stop("Invalid State")
  } 
    
  if (!Var_outcome %in% c("heart attack","heart failure","pneumonia")) {
    stop("Invalid Outcome")  
  }
  
  #records of specific state
  subset_outcome<-outcome[outcome$State==var_state,] 
      
  # minimun rates in different cases for the specific State
  min_heart_attack_rates<-min(subset_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.rm = TRUE) #best value of heart attack rates
  min_heart_failure_rates<-min(subset_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,na.rm = TRUE) #best value of heart attack rates
  min_pneumonia_rates<-min(subset_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,na.rm = TRUE) #best value of heart attack rates

  #Best hospital of specific state
  if(Var_outcome=="heart attack"){
    best_hospital<-subset_outcome[subset_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==min_heart_attack_rates ,2]   
  }else{
  if(Var_outcome=="heart failure"){
    best_hospital<-subset_outcome[subset_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==min_heart_failure_rates ,2]
  }else{
    best_hospital<-subset_outcome[subset_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==min_pneumonia_rates ,2]  
  }  
  }
  
  #shows the result
  best_hospital[complete.cases(best_hospital)] 
  
  } 
  




