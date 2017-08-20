# this file contains a function best which takes two arguments, validate them and return 
# a charcter vector with the name of the best hospital
# arguments are state abbrv. and outcome name
rankhospital <- function(state,outcome,num){
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
  { outcome_data <- data.frame(St = outcome_data$State,hos =outcome_data$Hospital.Name,res =as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    #rates<-cbind(as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
  }
 else if (outcome == "heart failure")
  
   outcome_data <- data.frame(St = outcome_data$State, hos = outcome_data$Hospital.Name,res = as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
  else if (outcome == "pneumonia")
    outcome_data <- data.frame(St = outcome_data$State,hos = outcome_data$Hospital.Name,res = as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  else
    stop("invalid outcome")
  
  outcome_data_set<-outcome_data[(outcome_data[,1] == state) & !is.na(outcome_data[,3]),]
  
  ordered_outcome <-outcome_data_set[order(outcome_data_set$res,outcome_data_set$hos),]
  
  rank_length <-dim(ordered_outcome)
  
  if(num=="best")
  {
    num<-1
  }
  else if (num=="worst")
    num<-rank_length[1]
  else if(num > rank_length[1])
  {
    print("NA")
    stop()
  }
  
  
  ordered_outcome_with_rank <-data.frame(ordered_outcome,rank=1:rank_length[1])
 
  print(ordered_outcome_with_rank$hos[ordered_outcome_with_rank$rank == num])
  
  
  
  
  
  
}