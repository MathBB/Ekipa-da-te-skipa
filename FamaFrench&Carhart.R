setwd("C:/Users/Matija Vodišek/Documents/Faculty/PROJEKT/Faktorski modeli")
.libPaths("C:/RStudio")

require("xts")
require("zoo")
require("quantmod")
require("dplyr")
require("TTR")

source("poprava.R")
source("razdeli.r")
source("razvrscanje2.R")

adjCene <- data.frame(readRDS("adjustedCene.RDS"))
dates <- rownames(adjCene)
dates <- dates[759:7436]  # Zožitev na dneve (od 1988-01-04 do 2014-06-30), ki so nam na razpolago v tabeli totAssets
years <- 3
num <- years*252
dates1 <- tail(dates, (num + 1))
dates_1ymore <- tail(dates, (num + 252))
stocks <- colnames(adjCene)
adjCene_1ymore <- adjCene[dates_1ymore, ]
adjCene <- adjCene[dates1,]  

rates1D <- (tail(adjCene, -1)/head(adjCene, -1)) - 1
dates2 <- rownames(rates1D)

##### Market Excess Return (MKT)

#getSymbols("^RUT", from = "2010-07-09", to = "2014-06-30", src = "yahoo")
#rates_russell <- as.xts(dailyReturn(RUT[, 6]))
#rates_russell <- tail(rates_russell[dates,], -1)
#dates1 <- index(rates_russell)

avg_rates1D <- apply(rates1D, 1, function(x) mean(x, na.rm = T))

r_free <- as.xts(read.table("DTB4WK.txt", header = TRUE, row.names = 1))
r_free <- r_free[dates2,]
for (i in 1:length(dates2)){
  if (r_free[i,] == "."){
    r_free[i,] <- r_free[(i-1),]}}

r_free_ed <- data.frame(r_free)

r_free <- as.numeric(coredata(r_free))/100
r_free_ed[, 1] <- r_free

MktExcess_r <- avg_rates1D - r_free_ed


##### Book-to-Market (B/M)

shares <- data.frame(readRDS("SharesOutstanding.RDS"))
shares <- shares[dates1, stocks]
MarkEquity <- tail(shares*adjCene, -1)
MarkEquity[MarkEquity == 0] <- NA

BookEquity <- data.frame(readRDS("ShareholdersEquity.RDS"))
BookEquity <- tail(BookEquity[dates1,], -1)
BookEquity[BookEquity == 0] <- NA
BtoM <- BookEquity/MarkEquity


##### 12-month return

return_12m <- ROC(adjCene_1ymore, n = 252, type = "discrete")
return_12m <- tail(return_12m, num)


# Ravrscanje delnic v posamezen portfelj (od 1 do 6)
portfoliosFF <- razvrscanje2(MarkEquity, BtoM, dates2)

portfoliosC_wml <- razvrscanje2(MarkEquity, return_12m, dates2)


# Racunanje donosov posameznega portfelja
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


# Racunanje faktorjev
smallFF <- apply(subset(mean_ratesFF, select = 1:3), 1, mean)
bigFF <- apply(subset(mean_ratesFF, select = 4:6), 1, mean)
lowFF <- apply(subset(mean_ratesFF, select = c(1,4)), 1, mean)
highFF <- apply(subset(mean_ratesFF, select = c(3,6)), 1, mean)

winnC <- apply(subset(mean_ratesC, select = c(3,6)), 1, mean)
lossC <- apply(subset(mean_ratesC, select = c(1,4)), 1, mean)

SMB <- smallFF - bigFF
HML <- highFF - lowFF

WML <- winnC - lossC

factorsFF <- cbind(MktExcess_r, SMB, HML, r_free_ed)
colnames(factorsFF) <- c("MKT", "SMB", "HML", "risk-free_r")

save(factorsFF, file = "factorsFF.RData")


factorsC <- cbind(MktExcess_r, SMB, HML, WML, r_free_ed)
colnames(factorsC) <- c("MKT", "SMB", "HML", "WML", "risk-free_r")

save(factorsC, file = "factorsC.RData")


