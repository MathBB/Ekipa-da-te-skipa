razvrscanjeQ <- function(xts, xts1, xts2, xts3){
# Funkcija sprejme xts matriko xts, po kateri se uravnavamo glede na razplozljive 
# datume in delnice (najbolje je, da je ta xts matrika xts matrika z dnevnimi cenami 
# oz. donosi delnic) in xts matrike xts1, xts2 ter xts3, ki so xts matrike s  
# fundamentalnimi podatki, po katerih želimo razvrstiti delnice v 18 portfeljev. 

# Funkcija vrne xts matriko z vrednostmi od 1 do 18, ta pa za vsak posamezen 
# trgovalni dan predstavlja portfelj, v kateremu se nahaja delnica na ta dan.
  
  len_r <- nrow(xts)
  
  # Priprava prostora v RAM-u za xts matriko, ki nam jo vrne funkcija.
  razvrsti <- xts
  razvrsti[, ] <- NA
  
  for (j in 1:len_r){
    m1 <- xts1[j, ]
    m1[m1 == 0] <- NA
    m2 <- xts2[j, ]
    m2[m2 == 0] <- NA
    m3 <- xts3[j, ]
    m3[m3 == 0] <- NA
    m4 <- xts[j, ]
    m4[m4 == 0] <- NA
     
    # Za xts matriko b najprej pripravimo prostor v RAM-u.
    b <- razvrsti[1:4,]  
    
    # Za vsak dan zdruzimo vrstice iz xts matrik v novo xts matriko b.
    b <- rbind(m1,m2,m3,m4)
    
    # Znebimo se delnic (stolpcev), ki nimajo na voljo vseh potrebnih podatkov.
    b <- b[, order(b[4,], na.last = NA)]
    b <- b[, order(b[3,], na.last = NA)]
    b <- b[, order(b[2,], na.last = NA)]
    
    # Razdelimo delnice v dva portfelja v razmerju 50:50 glede na fundamentalen podatek, 
    # ki je shranjen v matriki xts1. Meja med tema dvema portfeljema je tako delnica, ki 
    # ima fundamentalni podatek, ki je srednja vrednost. Delnicam iz prvega portfelja iz 
    # te delitve v prvi vrstici xts matrike b pripišemo 1, delnicam iz drugega portfelja
    # pa 10.
    b <- b[, order(b[1,], na.last = NA)]
    len <- ncol(b)
    n1 <- floor(len/2)
    b[1, ] <- rep(c(1,10), c(n1, (len - n1)))
    
    # Tako delnice iz prvega portfelja kot tudi delnice iz drugega portfelja vsakega zase 
    # razdelimo na nove tri portfelje v razmerju 30:70:30 glede na fundamentalen podatek,
    # ki je shranjen v matriki xts2. Sedaj imamo 6 portfeljev. Obema prvima na novo ustvarjena 
    # portfelja v drugi vrstici matrike b pripisemo 0, obema drugima 3, obema tretjima pa 6.  
    b <- b[, order(b[1,], b[2,])]
    n2 <- floor(n1*0.3)
    n3 <- ceiling(n1*0.7)
    n4 <- floor(n1*1.3)
    n5 <- ceiling(n1*1.7)
    b[2, ] <- rep(c(0,3,6,0,3,6), c(n2, (n3-n2), (n1-n3), (n4-n1), (n5-n4), (len-n5)))
    
    # Podobno kot zgoraj ponovno vsak portfelj zase razdelimo še na nove tri portfelje 
    # (30:40:30) tokrat glede na fundamentalen podatek, ki je shranjen v matriki xts3. 
    # Tako dobimo 18 portfeljev. Vsem sestim prvim na novo ustvarjenim portfeljem v
    # tretji vrstici matrike b pripišemo 0, vsem sestim drugim 1, vsem sestim tretjim
    # po vrsti pa 2. 
    b <- b[, order(b[1,], b[2,], b[3,])]
    n6 <- floor(n2*0.3)
    n7 <- ceiling(n2*0.7)
    n8 <- floor((n3-n2)*0.3)
    n9 <- ceiling((n3-n2)*0.7)
    n10 <- floor((n1-n3)*0.3)
    n11 <- ceiling((n1-n3)*0.7)
    n12 <- floor((n4-n1)*0.3)
    n13 <- ceiling((n4-n1)*0.7)
    n14 <- floor((n5-n4)*0.3)
    n15 <- ceiling((n5-n4)*0.7)
    n16 <- floor((len-n5)*0.3)
    n17 <- ceiling((len-n5)*0.7)
    b[3, ] <- rep(rep(0:2, 6), c(n6, (n7-n6), (n2-n7), n8, (n9-n8), (n3-n2-n9), n10, (n11-n10), (n1-n3-n11),
                                 n12, (n13-n12), (n4-n1-n13), n14, (n15-n14), (n5-n4-n15), n16, (n17-n16), (len-n5-n17)))
    
    # Vsem delnicam v cetrti vrstici pripisemo 0 ter sestejemo vrednosti po stolpcih matrike b.
    # Na novo dobljeni vektor transponiramo in ga shranimo v j-to vrstico xts matrike, ki nam jo
    # bo po koncani for zanki vrnila funkcija.
    b[4, ] <- rep(0, len)
    b_new <- t(apply(b, 2, sum))
    b_new <- t(b_new[, order(colnames(b_new))])
    razvrsti[j, colnames(b_new)] <- b_new
  }
  return(razvrsti)
}
