# this file contains a function best which takes two arguments, validate them and return 
# a charcter vector with the name of the best hospital
# arguments are state abbrv. and outcome name
best <- function(state,outcome){
  outcome_data <-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  valid_states <-c(outcome_data$State)
  st_validation <-0
  for (i in valid_states)
  {
    if(i==state)
    {
      st_validation <-1
      break
    }
  }
  if(st_validation==0)
    stop(" invalid state")
  
  if (outcome == "heart attack" )
    outcome_data <- cbind(outcome_data$State,outcome_data$Hospital.Name,as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
  else if (outcome == "heart failure")
    outcome_data <- cbind(outcome_data$State,outcome_data$Hospital.Name,outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  else if (outcome == "pneumonia")
    outcome_data <- cbind(outcome_data$State,outcome_data$Hospital.Name,outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  else
    stop("invalid outcome")
  
  outcome_data_set<-outcome_data[(outcome_data[,1] == state),]
  numeric_data <-outcome_data_set
  numeric_data[,3] =as.numeric(outcome_data_set[,3])*1
  numeric_data<-numeric_data[!is.na(numeric_data[,3]),]
  numbers=as.numeric(numeric_data[,3])
  
  numeric_data<-numeric_data[numeric_data[,3] == min(numbers),]
  
  print(numeric_data)

  
  
  
  
}