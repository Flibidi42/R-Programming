complete <- function(directory, id = 1:332) {
  for (k in id) {
    if (k < 10)
      fichier <- paste(directory, "/00", k, ".csv", sep = "")
    else if (k < 100)
      fichier <- paste(directory, "/0", k, ".csv", sep = "")
    else
      fichier <- paste(directory, "/", k, ".csv", sep = "")
    my_data <- read.csv(file = fichier)
  
}