# this file contains a function best which takes two arguments, validate them and return 
# a charcter vector with the name of the best hospital
# arguments are state abbrv. and outcome name
rankall <- function(outcome,num){
  outcome_data <-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  valid_states <-c(outcome_data$State)
  
  
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
  
  outcome_data_set<-outcome_data[!is.na(outcome_data[,3]),]
  
  outcome_split <-split(outcome_data_set,outcome_data_set$St)
  
  ordered_outcome_split <-lapply(outcome_split,function(df,numx=num){
    
    ordered_df <- df[order(df$res,df$hos),]
    rank_length <-dim(ordered_df)
    df_ranked <-data.frame(ordered_df,rank=1:rank_length[1])
    
    if(numx=="best")
    {
    data.frame(df_ranked$St[df_ranked$rank==1],df_ranked$hos[df_ranked$rank==1])
    }
    else if (numx=="worst")
  {
    data.frame(df_ranked$St[df_ranked$rank==1],df_ranked$hos[df_ranked$rank==rank_length[1]])
  }
  else if(numx > rank_length[1])
  {
    data.frame(df_ranked$St[df_ranked$rank==1],NA)
    
  }
    else
    {
      data.frame(df_ranked$St[df_ranked$rank==1],df_ranked$hos[df_ranked$rank==numx])
    }
  
    
    
    })
  xxx <-data.frame(ordered_outcome_split)
  curr_dim <-(dim(xxx))
  rowws <-curr_dim[2]/2
 # dim(xxx)<-c(rowws,2)
  
 
  
 xxx <-data.frame(matrix(unlist(t(xxx)), byrow=T, rowws, 2))
 names(xxx) <- c("state","hospital")
  
  return(xxx)
  
 
  
  
  
  
  
}