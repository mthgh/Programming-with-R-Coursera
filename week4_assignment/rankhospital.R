rankhospital <- function(state, outcome, num = "best") {
    outcome_list <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    if (! outcome %in% names(outcome_list)) return(message("invalid outcome"))
    
    if (!is.numeric(num) & num != "best" & num != "worst") return(message("invalid number"))
    
    om <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    all_states <- unique(om$State)
    if (! state %in% all_states) return(message("invalid state"))
    
    col_ex <- outcome_list[[outcome]]
    
    st_oc <- om[om$State==state, c(col_ex, 2)]
    st_oc[, 1] = suppressWarnings(as.numeric(st_oc[,1]))
    st_oc <- st_oc[order(st_oc[,1], st_oc[,2], na.last = NA), ]
    
    if (num == "best") return(st_oc[1,2])
    else if (num == "worst") return(st_oc[nrow(st_oc),2])
    return(st_oc[num,2])
}