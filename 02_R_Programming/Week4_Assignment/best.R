best <- function(state, outcome){
        ## Read outcome data
        df <- read.csv('./outcome-of-care-measures.csv',
                       na.strings = "Not Available", stringsAsFactors = FALSE)
        
        ## Check that state and outocme are valid
        if (!(state %in% unique(df$State))){
                return("invalid state")
        }
        else if (!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){
                return("invalid outcome")
        }
        else{
                ## Return hospital name in that state with lowest 30-day death
                if(outcome=='heart attack'){
                        output <- na.omit(df[, c(2, 7, 11)])
                        output <- output[output$State == state,]
                        return(output[order(output[,3], output[,1]),][1,1])
                }
                else if(outcome=='heart failure'){
                        output <- na.omit(df[, c(2, 7, 17)])
                        output <- output[output$State == state,]
                        return(output[order(output[,3], output[,1]),][1,1])
                }
                else{
                        output <- na.omit(df[, c(2, 7, 23)])
                        output <- output[output$State == state,]
                        return(output[order(output[,3], output[,1]),][1,1])
                }
        }
        
}