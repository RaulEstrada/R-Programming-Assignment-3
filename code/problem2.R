## Returns a character vector with the name of the hospital that has the best 
## (i.e. lowest) 30-day mortality for the specified outcome in that state
best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("./data/outcome-of-care-measures.csv", stringsAsFactors = FALSE)
    
    ## Checking that state and outcome are valid
    if (is.na(state)) {
        stop("Invalid state")
    }
    if (is.na(outcome) || !(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    
    ## Return hospital with lowest 30-day mortality rate
    if (outcome == "heart attack") {
        column <- 11
    } else if (outcome == "heart failure") {
        column <- 17
    } else {
        column <- 23
    }
    data <- data[data$State == state,]
    data[,column] <- as.double(data[,column])
    data <- data[!is.na(data[,column]),]
    lowestNumber <- min(data[column])
    targetRows <- data[data[column] == lowestNumber, ]
    targetRows[1,2]
}