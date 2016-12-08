rankedHospital <- function(data, num) {
    if (num == "best" && length(data) > 0) {
        data[1]
    } else if (num == "worst" && length(data) > 0) {
        data[length(data)]
    } else if (num > length(data) || length(data) == 0) {
        NA
    } else {
        data[num]
    }
}

## takes two arguments: an outcome name (outcome) and a hospital ranking
## (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("./data/outcome-of-care-measures.csv", stringsAsFactors = FALSE)
    
    ## Check that state and outcome are valid
    if (is.na(outcome) || !(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    states <- sort(unique(as.character(data$State)))

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
    hosp <- tapply(data$Hospital.Name, as.factor(data$State), rankedHospital, num)

    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    df <- as.data.frame(hosp)
    df <- cbind(df, states)
    colnames(df) <- c("Hospital", "State")
    df
}

