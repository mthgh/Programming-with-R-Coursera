best <- function(state, outcome) {
    outcome_list <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    if (! outcome %in% names(outcome_list)) return(message("invalid outcome")) 
    
    om <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    all_states <- unique(om$State)
    if (! state %in% all_states) return(message("invalid state"))
    
    col_ex <- outcome_list[[outcome]]
    
    st_oc <- om[om$State==state, c(col_ex, 2)]
    st_oc[, 1] = suppressWarnings(as.numeric(st_oc[,1]))
    best_rate <- min(st_oc[,1], na.rm = T)
    best_h <- sort(st_oc[st_oc[,1]==best_rate, 2])
    return(best_h[1])
}