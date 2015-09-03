razvrscanjeFF_C <- function(xts, xts1, xts2){
# Funkcija je poenostavljena verzija funkcije "razvrscanjeQ", s to razliko,
# da za vsak dan razvrsti delnice v zgolj 6 portfeljev. Za podrobnejsi opis 
# te funkcije si tako oglej datoteko "razvrscanjeQ.R".
  
  len_r <- nrow(xts)
  razvrsti <- xts
  razvrsti[, ] <- NA
  for (j in 1:len_r){
    m1 <- xts1[j, ]
    m1[m1 == 0] <- NA
    m2 <- xts2[j, ]
    m2[m2 == 0] <- NA
    m3 <- xts[j, ]
    m3[m3 == 0] <- NA
    b <- razvrsti[1:3,]
    b <- rbind(m1,m2,m3)
    b <- b[, order(b[3,], na.last = NA)]
    b <- b[, order(b[2,], na.last = NA)]
    b <- b[, order(b[1,], na.last = NA)]
    len <- ncol(b)
    n1 <- floor(len/2)
    b[1, ] <- rep(c(1,4), c(n1, (len - n1)))
    
    b <- b[, order(b[1,], b[2,])]
    n2 <- floor(n1*0.3)
    n3 <- ceiling(n1*0.7)
    n4 <- floor(n1*1.3)
    n5 <- ceiling(n1*1.7)
    b[2, ] <- rep(c(0,1,2,0,1,2), c(n2, (n3-n2), (n1-n3), (n4-n1), (n5-n4), (len-n5)))
    
    b <- b[, order(b[1,], b[2,], b[3,])]
    b[3, ] <- rep(0, len)
    b_new <- t(apply(b, 2, sum))
    b_new <- t(b_new[, order(colnames(b_new))])
    razvrsti[j, colnames(b_new)] <- b_new
  }
  return(razvrsti)
}
