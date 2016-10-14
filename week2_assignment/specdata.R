pollutantmean <- function(directory, pollutant, id = 1:332) {
    if (any(id < 1) | any(id > 332)) {
        return("Not valid id")
    }
    
    if (pollutant != "sulfate" & pollutant != "nitrate") {
        return("Not valid pollutant")
    }
    
    filename <- paste0(sprintf(("%03d"), id), ".csv")
    filePath <- file.path(directory, filename)
    
    data <- data.frame()
    for (file in filePath) {
        if (!file.exists(file)) return("Can not find file in directory")
        data <- rbind(data, read.csv(file))
    }
    
    data_use <- data[complete.cases(data[, pollutant]), pollutant]
    mean(data_use)
}

complete <- function(directory, id = 1:332) {
    if (any(id < 1) | any(id > 332)) {
        return("Not valid id")
    }
    
    filename <- paste0(sprintf(("%03d"), id), ".csv")
    filePath <- file.path(directory, filename)
    
    nobs <- numeric(0)
    for (i in seq_along(filePath)) {
        if (!file.exists(filePath[i])) return("Can not find file in directory")
        data <- read.csv(filePath[i])
        nobs[i] <- sum(complete.cases(data))
    }
    
    data.frame(id, nobs)
}

corr <- function(directory, threshold = 0) {
    data_nobs <- complete(directory)
    if (class(data_nobs) != "data.frame") {
        return("Not valid directory")
    }
    
    data_ids <- data_nobs[data_nobs$nobs > threshold,"id"]
    if (length(data_ids)==0) {
        return(numeric(0))
    }
    
    filename <- paste0(sprintf(("%03d"), data_ids), ".csv")
    filePath <- file.path(directory, filename)
    
    correlations = numeric(0)
    for (i in seq_along(filePath)) {
        data <- read.csv(filePath[i])
        correlations[i] <- cor(data$sulfate, data$nitrate, use="complete.obs")
    }
    return(correlations)
}