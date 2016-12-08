## Function that takes three arguments: the 2-character abbreviated name of a 
## state, an outcome, and the ranking of a hospital in that state for that 
## outcome. It reads the data file and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument.
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("./data/outcome-of-care-measures.csv", stringsAsFactors = FALSE)
    
    ## Check that state and outcome are valid
    ## Checking that state and outcome are valid
    if (is.na(state)) {
        stop("Invalid state")
    }
    if (is.na(outcome) || !(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank
    data <- data[data$State == state,]
    if (outcome == "heart attack") {
        column <- 11
    } else if (outcome == "heart failure") {
        column <- 17
    } else {
        column <- 23
    }
    data[,column] <- as.double(data[,column])
    data <- data[!is.na(data[,column]),]
    data <- data[order(data[,column], data$Hospital.Name),]
    if (num == "best") {
        data[1, 2]
    } else if (num == "worst") {
        data[nrow(data), 2]
    } else if (num > nrow(data)) {
        NA
    } else {
        data[num, 2]
    }
}