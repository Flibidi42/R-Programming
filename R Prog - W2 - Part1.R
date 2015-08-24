pollutantmean <- function(directory, pollutant = " ", id = 1:332) {
  total <- 0
  for (k in id) {
    if (k < 10)
      fichier <- paste(directory, "/00", k, ".csv", sep = "")
    else if (k < 100)
      fichier <- paste(directory, "/0", k, ".csv", sep = "")
    else
      fichier <- paste(directory, "/", k, ".csv", sep = "")
    my_data <- read.csv(file = fichier)
    if (pollutant == "sulfate") {
      total <- total + mean(my_data$sulfate[!is.na(my_data$sulfate)])
    }
    else{
      total <- total + mean(my_data$nitrate[!is.na(my_data$nitrate)])
    }
  }
  total <- total / (length(id))
  total
}