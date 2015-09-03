poprava <- function(xts_mat){
  # Funkcija sprejme xts matriko, ki nam predstavlja spremembe fundamentalnega podatka glede 
  # na prejsnji trgovalni dan. V taki xts matriki se tako nahajajo samo nicle, ko ni bilo spremembe 
  # glede na prejsni trgovalni dan, in stevilo razlicno od 0, ko dejansko je bila sprememba.
  #
  # Funkcija tako vrne popravljeno xts matriko: 
  # 1. Nicle nadomesti z zadnjo spremembo fundamentalnega podatka.
  # 2. Ce te vrednosti, ki predstavlja spremembo, ni pred nizom nicel, pa nadomesti te nicle z NA.
  
  n1 <- ncol(xts_mat)
  n2 <- nrow(xts_mat)
  for (i in 1:n1){
    vec <- xts_mat[, i]
    ind <- which(vec != 0)
    val <- vec[ind]
    n3 <- length(ind)
    if (n3 > 1){
      for (j in 1:(n3-1)){
        a <- ind[j]
        b <- ind[j+1]
        part <- vec[a:(b-1)]
        part[part == 0] <- val[j]
        vec[a:(b-1)] <- part
        if (j == (n3-1)){
          part <- vec[b:n2]
          part[part == 0] <- val[j+1]
          vec[b:n2] <- part
        }
      }
    }
    else if (n3 == 1){
      b <- ind
      part <- vec[b:n2]
      part[part == 0] <- val
      vec[b:n2] <- part
    }
    vec[vec == 0] <- NA
    xts_mat[, i] <- vec
  }
  return(xts_mat)
}  


  