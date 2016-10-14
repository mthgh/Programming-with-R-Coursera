
rankall <- function(outcome, num = "best") {
    source("rankhospital.R")
    
    outcome_list <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    if (! outcome %in% names(outcome_list)) return(message("invalid outcome"))
    
    if (!is.numeric(num) & num != "best" & num != "worst") return(message("invalid number"))
    
    om <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    all_states <- unique(om$State)
    sts_ranking <- data.frame(hospital=character(), state=character(), stringsAsFactors=F)
    for(st in all_states) {
        h_name <- rankhospital(st, outcome, num)
        sts_ranking[nrow(sts_ranking)+1, ] <- c(h_name, st)
    }
    return(sts_ranking[order(sts_ranking$state),])
}