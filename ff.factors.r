require("xts")
require("zoo")
require("quantmod")
require("TTR")



# Za risk-free ob. mero vzamemo donose enomesecnih zakladnih menic
getSymbols("DGS1MO", src = "FRED")


ff.factors <- function(cene, st_delnic, knjig_vred, zacD, konD){
# Funkcija sprejme xts matrike z dnevnimi podatki o cenah delnic, stevilu delnic v 
# obtoku in knjigovodski vrednosti podjetja, ter zacetni in koncni datum obdobja.
# Funkcija pa vrne dnevne podatke o faktorjih faktorskega modela Fama-French, 
# in sicer iz obdobja, ki smo ga dolocili z zacetnim in koncnim datumom.  
  
  stocks <- colnames(cene)
  dates <- paste(zacD, konD, sep = "/") 
  
  # Da se ne pojavljajo NA-ji, za racunanje donosov vzamemo 1 teden daljse obdobje od prvotnega obdobja (dates).
  dates2 <- paste((as.Date(zacD) - 7), konD, sep = "/")
  
  # Racunanje enodnevnih donosov, zato za n vzamemo 1.
  rates1D <- ROC(cene[dates2,], n = 1, type = "discrete")
  rates1D <- rates1D[dates,]  
  
  
  ##### Izracun faktorja Market Excess Return (MKT) = trzna premija za tveganje

  dates <- index(rates1D)
  r_free <- DGS1MO[dates,]/100
  r_free[r_free == NA] <- 0
  r_free[c("2014-10-13","2014-11-11"),] <- 0.0003
  
  # Za povprecen donos trga lahko vzamemo kar povprecen donos po vseh razpolozljivih delnicah.
  avg_rates1D <- apply(rates1D, 1, function(x) mean(x, na.rm = T))
  MktExcess_r <- avg_rates1D - r_free


  ### Izracun Market Equity-a in nato Book-to-Market (B/M)

  shares <- st_delnic[dates,]
  MarkEquity <- shares*cene[dates,]

  BookEquity <- knjig_vred[dates,]
  BtoM <- BookEquity/MarkEquity


  # Ravrscanje delnic glede na Market Equity in Book-to-Market v portfelje od 1 do 6
  # s funkcijo "razvrscanjeFF_C" (opis funkcije si oglej v datoteki "razvrscanjeFF_C.R").
  portfoliosFF <- razvrscanjeFF_C(rates1D, MarkEquity, BtoM)


  # Racunanje donosov posameznega portfelja
  
  # Vsaka od matrik od P1 do P6 vsebuje samo vrednosti NA ali 1.
  # Matrika P1 se nanasa na portfelj 1, P2 na portfelj 2, ..., P6 pa na portfelj 6.
  # NA pomeni, da na posamezen dan posamezna delnica ni v tistem portfelju, 1 pa pomeni, da je.
  P1 <- P2 <- P3 <- P4 <- P5 <- P6 <- portfoliosFF
  
  P1[P1 != 1] <- P2[P2 != 2] <- P3[P3 != 3] <- P4[P4 != 4] <- P5[P5 != 5] <- P6[P6 != 6] <- NA
  P1[P1 == 1] <- P2[P2 == 2] <- P3[P3 == 3] <- P4[P4 == 4] <- P5[P5 == 5] <- P6[P6 == 6] <- 1

  rates1FF <- apply(rates1D*P1, 1, function(x) mean(x, na.rm = T))
  rates2FF <- apply(rates1D*P2, 1, function(x) mean(x, na.rm = T))
  rates3FF <- apply(rates1D*P3, 1, function(x) mean(x, na.rm = T))
  rates4FF <- apply(rates1D*P4, 1, function(x) mean(x, na.rm = T))
  rates5FF <- apply(rates1D*P5, 1, function(x) mean(x, na.rm = T))
  rates6FF <- apply(rates1D*P6, 1, function(x) mean(x, na.rm = T))

  mean_ratesFF <- cbind(rates1FF, rates2FF, rates3FF, rates4FF, rates5FF, rates6FF)
  colnames(mean_ratesFF) <- paste("Portf_", as.character(1:6), sep = "")
  
  
  # Racunanje faktorjev
  smallFF <- apply(subset(mean_ratesFF, select = 1:3), 1, mean)
  bigFF <- apply(subset(mean_ratesFF, select = 4:6), 1, mean)
  lowFF <- apply(subset(mean_ratesFF, select = c(1,4)), 1, mean)
  highFF <- apply(subset(mean_ratesFF, select = c(3,6)), 1, mean)
   
  SMB <- smallFF - bigFF
  HML <- highFF - lowFF
  SMB <- as.xts(SMB, order.by = as.Date(index(MktExcess_r)))
  HML <- as.xts(HML, order.by = as.Date(index(MktExcess_r)))
  
  factorsFF <- cbind(MktExcess_r, SMB, HML, r_free)
  colnames(factorsFF) <- c("MKT", "SMB", "HML", "risk.free_r")
  
  return(factorsFF)
}

