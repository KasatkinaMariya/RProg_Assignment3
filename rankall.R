rankall <- function(outcome, num = "best") {
  requestedRank <- num
  requestedDisease <- outcome
  
  mortalityRateColumnNameEnd <- switch(requestedDisease,
                                       "heart attack" = "Heart.Attack",
                                       "heart failure" = "Heart.Failure",
                                       "pneumonia" = "Pneumonia",
                                       stop("invalid outcome"))
  mortalityRateColumnName <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",
                                   mortalityRateColumnNameEnd,
                                   sep="")
  interestingColumns <- c("Hospital.Name",
                          "State",
                          mortalityRateColumnName)    
  
  allData <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available")    
  relevantData <- allData[!is.na(allData[[mortalityRateColumnName]]),interestingColumns]
  splittedByState <- split(relevantData,relevantData$State)
  
  bestHospitalInState <- sapply(splittedByState, function (x) {
    state <- as.character(x$State[1])
    hospitalNamesByMortalityRate <- x[order(x[[mortalityRateColumnName]],
                                            x$Hospital.Name),
                                      ]$Hospital.Name
    
    if (is.numeric(requestedRank))
      hospital <- hospitalNamesByMortalityRate[requestedRank]
    else
      hospital <- switch(requestedRank,
                         "best"  = head(hospitalNamesByMortalityRate, n=1),
                         "worst" = tail(hospitalNamesByMortalityRate, n=1))
    
    c(as.character(hospital),state)
  })
  
  data.frame(hospital=bestHospitalInState[1,],
             state=bestHospitalInState[2,])
}