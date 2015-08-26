rankhospital <- function(state, outcome, num = "best") {
  valeur_rank <- 0
  ## Read outcome data
  ## Check that state and outcome are valid
  my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  try(if(match(state, my_data$State, nomatch = -1) == -1) stop("Error with the state"))
  possible <- c("heart attack", "heart failure", "pneumonia")
  try(if(match(outcome, possible, nomatch = -1) == -1) stop("Error with the outcome"))
  if(outcome == "heart attack")
    outcome <- "Heart.Attack"
  else if (outcome == "heart failure")
    outcome <- "Heart.Failure"
  else
    outcome <- "Pneumonia"
  v <- vector()
  colone <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep="")
  my_data <- my_data[my_data$State == state,] # Selection de l'état
  my_data <- my_data[(my_data[,colone]) != "Not Available",]
  my_data <- my_data[order(as.numeric(my_data[,colone]), my_data$Hospital.Name), ]
  if(num == "best"||num<=0)
    num <- 1
  else if(num == "worst")
    num <- length(my_data$Hospital.Name)
  my_data[num, "Hospital.Name"]
  ## Return hospital name in that state with the given rank 30-day death rate
}