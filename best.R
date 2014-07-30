best <- function(state, outcome) {
    ## Read outcome data
    outcomes <- read.csv("data/outcome-of-care-measures.csv",
                     colClasses="character")
    
    ## Check that state and outcome are valid
    if(!is.character(state) ||
       !(state %in% state.abb)) {
        stop("invalid state")
    }
    
    validOutcomes <- c("heart attack", 
                       "heart failure",
                       "pneumonia")
    # Hospital name column
    nameCol <- 2
    
    # Respective columns for each outcome
    outcomeColNums <- c(11,17,23) 
    
    if (!is.character(outcome) ||
        !(outcome %in% validOutcomes)) {
        stop("invalid outcome")        
    }
    
    outcomesByState <- split(outcomes, outcomes$State)    
    stateOutcomes <- outcomesByState[[state]]
    
    outcomeCol <- outcomeColNums[match(outcome,validOutcomes)]
    
    stateOutcomes[,outcomeCol] <- as.numeric(stateOutcomes[,outcomeCol])
    
    selectedOutcome <- stateOutcomes[c(nameCol,outcomeCol)]
    selectedOutcome <- selectedOutcome[complete.cases(selectedOutcome),]
    selectedOutcome <- selectedOutcome[order(selectedOutcome[,2],selectedOutcome[,1]),]
    selectedOutcome[1,1]
    
}