rankall <- function(outcome, num='best'){
        ## Read outcome data
        df <- read.csv('./outcome-of-care-measures.csv',
                       na.strings = "Not Available", stringsAsFactors = FALSE)
        
        ## Check that state and outocme are valid
        if (!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){
                return("invalid outcome")
        }
        else{
                ## Return hospital name in that state with lowest 30-day death
                if(outcome=='heart attack'){
                        output <- na.omit(df[, c(2, 7, 11)])
                        output <- output[order(output[,3], output[,1]),]
                }
                else if(outcome=='heart failure'){
                        output <- na.omit(df[, c(2, 7, 17)])
                        output <- output[order(output[,3], output[,1]),]
                }
                else{
                        output <- na.omit(df[, c(2, 7, 23)])
                        output <- output[order(output[,3], output[,1]),]
                }
        }
        
        # rank
        s <- split(output,output[,2])
        if (is.numeric(num)){
                ranked <- data.frame(sapply(s, function(x) x[num,1]))
                ranked <- cbind(ranked, row.names(ranked))
                names(ranked) <- c('hospital', 'state')
                return(ranked)
        }
        else{
                if (num=='best'){
                        ranked <- data.frame(sapply(s, function(x) x[1,1]))
                        ranked <- cbind(ranked, row.names(ranked))
                        names(ranked) <- c('hospital', 'state')
                        return(ranked)
                }
                else {
                        ranked <- data.frame(sapply(s, function(x) x[nrow(x),1]))
                        ranked <- cbind(ranked, row.names(ranked))
                        names(ranked) <- c('hospital', 'state')
                        return(ranked)
                }
        }
        
}