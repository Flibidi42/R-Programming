best <- function(state, outcome) {
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
  v <- as.numeric(my_data[,colone])# Extraction des taux
  my_data <- my_data[v == min(v),] # Extraction des minima
  noms <- my_data$Hospital.Name # Extraction des noms
  noms <- sort(noms)
  noms[1]
}