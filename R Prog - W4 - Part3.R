rankall <- function(outcome, num = "best") {
  my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data_modif <- my_data
  noms <- vector()
  etats <- vector()
  possible_s <- my_data$State
  possible_s <- unique(possible_s)
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
  for(s in possible_s){
    data_modif <- data_modif[data_modif$State == s,] # Selection de l'état
    data_modif <- data_modif[(data_modif[,colone]) != "Not Available",]
    data_modif <- data_modif[order(as.numeric(data_modif[,colone]), data_modif$Hospital.Name), ]
    if(num == "best"||num<=0)
      nume <- 1
    else if(num == "worst")
      nume <- nrow(data_modif)
    else
      nume <- num
    etats <- c(etats, s)
    noms <- c(noms, data_modif[nume, "Hospital.Name"])
    data_modif <- my_data
  }
  final <- data.frame(noms, etats)
  colnames(final) <- c("Noms", "Etats")
  final <- final[order(final$Etats, final$Noms), ]
  final
}