corr <- function(directory, threshold = 0) {
  vecteur_sortie <- vector()
  seuils <- complete("specdata")
  for(k in 1:332){
    if(seuils$Nblignes[seuils$ID == k] > threshold){
      if (k < 10)
        fichier <- paste("../", directory, "/00", k, ".csv", sep = "")
      else if (k < 100)
        fichier <- paste("../", directory, "/0", k, ".csv", sep = "")
      else
        fichier <- paste("../", directory, "/", k, ".csv", sep = "")
      my_data <- read.csv(file = fichier)
      vecteur_sortie <- c(vecteur_sortie, cor(my_data$nitrate, my_data$sulfate, use = "pairwise.complete.obs"))
    }
  }
  round(vecteur_sortie, 3)
}