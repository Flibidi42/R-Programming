complete <- function(directory, id = 1:332) {
  vecteur_id <- vector()
  vecteur_compte <- vector()
  for (k in id) {
    vecteur_id <- c(vecteur_id, k)
    if (k < 10)
      fichier <- paste("../", directory, "/00", k, ".csv", sep = "")
    else if (k < 100)
      fichier <- paste("../", directory, "/0", k, ".csv", sep = "")
    else
      fichier <- paste("../", directory, "/", k, ".csv", sep = "")
    my_data <- read.csv(file = fichier)
    vecteur_compte <- c(vecteur_compte, length(my_data$ID[!is.na(my_data$sulfate)&!is.na(my_data$nitrate)]))
  }
  sortie <- data.frame(vecteur_id, vecteur_compte)
  colnames(sortie) <- c("ID", "Nb lignes")
  sortie
}