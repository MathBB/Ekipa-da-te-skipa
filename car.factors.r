

car.factors <- function(cene, st_delnic, knjig_vred, zacD, konD){
# Funkcija sprejme xts matrike z dnevnimi podatki o cenah delnic, stevilu delnic v 
# obtoku in knjigovodski vrednosti podjetja, ter zacetni in koncni datum obdobja.
# Funkcija pa vrne dnevne podatke o faktorjih faktorskega modela Carhart, 
# in sicer iz obdobja, ki smo ga dolocili z zacetnim in koncnim datumom.
  
  # Faktorski model Carhart je nadgradnja fak. modela Fama-French s faktorjem WML, 
  # zato faktorje MKT, SMB in HML pridobimo iz ze implementirane funkcije ff.factors. 
  fact <- ff.factors(cene, st_delnic, knjig_vred, zacD, konD)
  stocks <- colnames(cene)
  dates <- paste(zacD, konD, sep = "/")
  
  # Da se ne pojavljajo NA-ji, za izracun letnih donosov vzamemo 12 (polnih) mesecev (12*31=372) 
  # daljse obdobje od prvotnega obdobja (dates).
  dates1 <- paste((as.Date(zacD) - 372), konD, sep = "/") 
  
  # Da se ne pojavljajo NA-ji, za racunanje donosov vzamemo 1 teden daljse obdobje od prvotnega obdobja.
  dates2 <- paste((as.Date(zacD) - 7), konD, sep = "/")
  
  # Racunanje enodnevnih donosov, zato za n vzamemo 1.
  rates1D <- ROC(cene[dates2,], n = 1, type = "discrete")
  rates1D <- rates1D[dates,]  
  
  
  ### Izracun Market Equity-a
  
  shares <- st_delnic[dates,] 
  MarkEquity <- shares*cene[dates,]
  
  
  ### Izracun 12-mesecnih/1-letnih donosov
  
  # Za n vzamemo 252 = povprecje trgovalnih dni v letu.
  return_12m <- ROC(cene[dates1,], n = 252, type = "discrete")
  return_12m <- return_12m[dates,]

  
  # Ravrscanje delnic glede na Market Equity in 12-mesecni donos v portfelje od 1 do 6
  # s funkcijo "razvrscanjeFF_C" (opis funkcije si oglej v datoteki "razvrscanjeFF_C.R").
  portfoliosC_wml <- razvrscanjeFF_C(rates1D, MarkEquity, return_12m)


  # Racunanje donosov posameznega portfelja
  
  # Vsaka od matrik od P1 do P6 vsebuje samo vrednosti NA ali 1.
  # Matrika P1 se nanasa na portfelj 1, P2 na portfelj 2, ..., P6 pa na portfelj 6.
  # NA pomeni, da na posamezen dan posamezna delnica ni v tistem portfelju, 1 pa pomeni, da je.
  P1 <- P2 <- P3 <- P4 <- P5 <- P6 <- portfoliosC_wml
  
  P1[P1 != 1] <- P2[P2 != 2] <- P3[P3 != 3] <- P4[P4 != 4] <- P5[P5 != 5] <- P6[P6 != 6] <- NA
  P1[P1 == 1] <- P2[P2 == 2] <- P3[P3 == 3] <- P4[P4 == 4] <- P5[P5 == 5] <- P6[P6 == 6] <- 1
  
  rates1C <- apply(rates1D*P1, 1, function(x) mean(x, na.rm = T))
  rates2C <- apply(rates1D*P2, 1, function(x) mean(x, na.rm = T))
  rates3C <- apply(rates1D*P3, 1, function(x) mean(x, na.rm = T))
  rates4C <- apply(rates1D*P4, 1, function(x) mean(x, na.rm = T))
  rates5C <- apply(rates1D*P5, 1, function(x) mean(x, na.rm = T))
  rates6C <- apply(rates1D*P6, 1, function(x) mean(x, na.rm = T))

  mean_ratesC <- cbind(rates1C, rates2C, rates3C, rates4C, rates5C, rates6C)
  colnames(mean_ratesC) <- paste("Portf_", as.character(1:6), sep = "")

  
  # Racunanje faktorja WML
  winnC <- apply(subset(mean_ratesC, select = c(3,6)), 1, mean)
  lossC <- apply(subset(mean_ratesC, select = c(1,4)), 1, mean)

  WML <- winnC - lossC
  WML <- as.xts(WML, order.by = as.Date(index(fact[, 1])))

  factorsC <- cbind(fact[, 1], fact[, 2], fact[, 3], WML, fact[, 4])
  colnames(factorsC) <- c("MKT", "SMB", "HML", "WML", "risk.free_r")
  
  return(factorsC)
}

faktorjiCar <- car.factors(prices, shares, bookval, "2014-06-30", last(datumi))

#PODATKI
prices1 <- prices["2014-06-30 ::"]
#prices <- prices[,apply(!is.na(prices), 2, all)]
#load("D:\\R_\\OPB\\Cene.RData")
#load("D:\\R_\\OPB\\faktorjiCar.RData")
#faktorjiCar <- as.data.frame(faktorjiCar)
faktorjiCar <- faktorjiCar[-1,]
colnames(faktorjiCar) = c("MKT", "SMB", "HML", "WML", "risk_free_r")
ClosePrices <- prices1
#ClosePrices <- as.xts(ClosePrices, order.by = as.Date(row.names(ClosePrices)))
ClosePrices <- ClosePrices[, !(colnames(ClosePrices) %in% c("GBIM","GOOG","LBRDA","KBIO","PSG","XOOM"))]
#ClosePrices <- ClosePrices[paste0("::",last(rownames(faktorjiCar)))]
#faktorjiCar <- as.xts(faktorjiCar, order.by = as.Date(row.names(faktorjiCar)))