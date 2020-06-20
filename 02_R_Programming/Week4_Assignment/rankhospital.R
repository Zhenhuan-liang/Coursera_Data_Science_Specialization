rankhospital <- function(state, outcome, num='best'){
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
                        output <- output[order(output[,3], output[,1]),]
                }
                else if(outcome=='heart failure'){
                        output <- na.omit(df[, c(2, 7, 17)])
                        output <- output[output$State == state,]
                        output <- output[order(output[,3], output[,1]),]
                }
                else{
                        output <- na.omit(df[, c(2, 7, 23)])
                        output <- output[output$State == state,]
                        output <- output[order(output[,3], output[,1]),]
                }
        }
        
        # rank
        if (is.numeric(num)){
                if (num > nrow(output)){return(NA)}
                else {return(output[num, 1])}
        }
        else{
                if (num=='best'){output[1, 1]}
                else {return(output[nrow(output), 1])}
        }
        
}