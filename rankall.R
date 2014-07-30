rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomes <- read.csv("data/outcome-of-care-measures.csv",
                         colClasses="character")
    
    ## Check that outcome is valid
    validOutcomes <- c("heart attack", 
                       "heart failure",
                       "pneumonia")
    if (!is.character(outcome) ||
            !(outcome %in% validOutcomes)) {
        stop("invalid outcome")        
    }
    
    #State name column
    stateCol <- 7
    
    # Hospital name column
    nameCol <- 2
    
    # Respective columns for each outcome
    outcomeColNums <- c(11,17,23) 
        
    outcomeCol <- outcomeColNums[match(outcome,validOutcomes)]
    
    outcomes[,outcomeCol] <- as.numeric(outcomes[,outcomeCol])
    
    selectedOutcome <- outcomes[c(stateCol, nameCol, outcomeCol)]
    selectedOutcome <- selectedOutcome[complete.cases(selectedOutcome),]
    selectedOutcome <- selectedOutcome[order(selectedOutcome[,1],
                                             selectedOutcome[,3],
                                             selectedOutcome[,2]),]
    names(selectedOutcome) <- c("state","hospital", outcome)
    outcomesByState <- split(selectedOutcome, selectedOutcome$state)
    
    # Initialize the data frame
    result <- data.frame(hospital=character(),
                         state=character(),
                         stringsAsFactors = FALSE)
    
    # Loop through the states
    for(s in names(outcomesByState)) {
        stateOutcomes <- outcomesByState[[s]]
        maxRank <- nrow(stateOutcomes)
        
        val <- 0
        if (num=="best") val <- 1
        else if (num=="worst") val <- maxRank
        else val <- num
        
        newRow <- data.frame(hospital=NA,
                             state=s,
                             stringsAsFactors = FALSE)
        if (val<=maxRank) {
            newRow <- data.frame(hospital=stateOutcomes$hospital[val],
                                 state=s,
                                 stringsAsFactors = FALSE)
        }
        result <- rbind(result, newRow)
    }
    result
}