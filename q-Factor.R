setwd("C:/Users/Matija Vodišek/Documents/Faculty/PROJEKT/Faktorski modeli")
.libPaths("C:/RStudio")

require("xts")
require("zoo")
require("quantmod")
require("dplyr")

source("poprava.R")
source("razdeli.r")
source("razvrscanje1.R")

adjCene <- data.frame(readRDS("adjustedCene.RDS"))
dates <- rownames(adjCene)
dates <- dates[759:7436]  # Zožitev na dneve (od 1988-01-04 do 2014-06-30), ki so nam na razpolago v tabeli totAssets
num1 <- 4*21  # Vzamemo še 4 mesece več za računanje 1 kvartal odloženega BookEquity-a 
years <- 3
num2 <- years*252
dates1 <- tail(dates, (num1 + num2))
dates2 <- tail(dates, (num2 + 1))
stocks <- colnames(adjCene)
adjCene2 <- adjCene[dates2,]  

rates1D <- (tail(adjCene2, -1)/head(adjCene2, -1)) - 1
dates3 <- rownames(rates1D)

##### Market Excess Return (MKT)

#getSymbols("^RUT", from = "2010-07-09", to = "2014-06-30", src = "yahoo")
#rates_russell <- as.xts(dailyReturn(RUT[, 6]))
#rates_russell <- tail(rates_russell[dates,], -1)
#dates1 <- index(rates_russell)

avg_rates1D <- apply(rates1D, 1, function(x) mean(x, na.rm = T))

r_free <- as.xts(read.table("DTB4WK.txt", header = TRUE, row.names = 1))
r_free <- r_free[dates3,]
for (i in 1:length(dates3)){
  if (r_free[i,] == "."){
    r_free[i,] <- r_free[(i-1),]}}

r_free_ed <- data.frame(r_free)

r_free <- as.numeric(coredata(r_free))/100
r_free_ed[, 1] <- r_free

MktExcess_r <- avg_rates1D - r_free_ed


##### Market Equity

shares <- data.frame(readRDS("SharesOutstanding.RDS"))
shares <- shares[dates2, stocks]
MarkEquity <- tail(shares*adjCene2, -1)
MarkEquity[MarkEquity == 0] <- NA


##### Investments (difference in Assets)

totAssets <- data.frame(readRDS("TotalAssets.RDS"))
totAssets <- totAssets[dates1,]
totAssets[totAssets == 0] <- NA
invest <- (tail(totAssets, -1) - head(totAssets, -1))/head(totAssets, -1)

# Poprava xts matrike invest s funkcijo "poprava"
invest <- poprava(invest)

invest <- tail(invest, num2)


##### Return-On-Equity (ROE)

IncomeBEI <- data.frame(readRDS("IncomeBeforeExtraordinariesDiscontinuedOperations.RDS"))
IncomeBEI <- IncomeBEI[dates1,]
IncomeBEI[IncomeBEI == 0] <- NA

BookEquity <- data.frame(readRDS("ShareholdersEquity.RDS"))
BookEquity <- BookEquity[dates1,]
BookEquity[BookEquity == 0] <- NA
changeBookEq <- (tail(BookEquity, -1) - head(BookEquity, -1))/head(BookEquity, -1)
changeBookEq <- poprava(changeBookEq)
BookEquity_1Qlagged <- (tail(BookEquity, -1))*(1/(1 + changeBookEq))
ROE <- tail(IncomeBEI, -1)/BookEquity_1Qlagged
ROE <- tail(ROE, num2)


# Ravrscanje delnic v posamezen portfelj (od 1 do 18)
portfolios <- razvrscanje1(MarkEquity, invest, ROE, dates3)

# Racunanje donosov posameznega portfelja
P1 <- P2 <- P3 <- P4 <- P5 <- P6 <- P7 <- P8 <- P9 <- portfolios
P10 <- P11 <- P12 <- P13 <- P14 <- P15 <- P16 <- P17 <- P18 <- portfolios

P1[P1 != 1] <- P2[P2 != 2] <- P3[P3 != 3] <- P4[P4 != 4] <- P5[P5 != 5] <- P6[P6 != 6] <- NA
P7[P7 != 7] <- P8[P8 != 8] <- P9[P9 != 9] <- P10[P10 != 10] <- P11[P11 != 11] <- P12[P12 != 12] <- NA
P13[P13 != 13] <- P14[P14 != 14] <- P15[P15 != 15] <- P16[P16 != 16] <- P17[P17 != 17] <- P18[P18 != 18] <- NA

P1[P1 == 1] <- P2[P2 == 2] <- P3[P3 == 3] <- P4[P4 == 4] <- P5[P5 == 5] <- P6[P6 == 6] <- 1
P7[P7 == 7] <- P8[P8 == 8] <- P9[P9 == 9] <- P10[P10 == 10] <- P11[P11 == 11] <- P12[P12 == 12] <- 1
P13[P13 == 13] <- P14[P14 == 14] <- P15[P15 == 15] <- P16[P16 == 16] <- P17[P17 == 17] <- P18[P18 == 18] <- 1


rates1 <- apply(rates1D*P1, 1, function(x) mean(x, na.rm = T))
rates2 <- apply(rates1D*P2, 1, function(x) mean(x, na.rm = T))
rates3 <- apply(rates1D*P3, 1, function(x) mean(x, na.rm = T))
rates4 <- apply(rates1D*P4, 1, function(x) mean(x, na.rm = T))
rates5 <- apply(rates1D*P5, 1, function(x) mean(x, na.rm = T))
rates6 <- apply(rates1D*P6, 1, function(x) mean(x, na.rm = T))
rates7 <- apply(rates1D*P7, 1, function(x) mean(x, na.rm = T))
rates8 <- apply(rates1D*P8, 1, function(x) mean(x, na.rm = T))
rates9 <- apply(rates1D*P9, 1, function(x) mean(x, na.rm = T))
rates10 <- apply(rates1D*P10, 1, function(x) mean(x, na.rm = T))
rates11 <- apply(rates1D*P11, 1, function(x) mean(x, na.rm = T))
rates12 <- apply(rates1D*P12, 1, function(x) mean(x, na.rm = T))
rates13 <- apply(rates1D*P13, 1, function(x) mean(x, na.rm = T))
rates14 <- apply(rates1D*P14, 1, function(x) mean(x, na.rm = T))
rates15 <- apply(rates1D*P15, 1, function(x) mean(x, na.rm = T))
rates16 <- apply(rates1D*P16, 1, function(x) mean(x, na.rm = T))
rates17 <- apply(rates1D*P17, 1, function(x) mean(x, na.rm = T))
rates18 <- apply(rates1D*P18, 1, function(x) mean(x, na.rm = T))
mean_rates <- cbind(rates1, rates2, rates3, rates4, rates5, rates6, rates7, rates8, rates9,
                    rates10, rates11, rates12, rates13, rates14, rates15, rates16, rates17, rates18)
colnames(mean_rates) <- paste("Portf_", as.character(1:18), sep = "")


# Racunanje faktorjev
small <- apply(subset(mean_rates, select = 1:9), 1, mean)
big <- apply(subset(mean_rates, select = 10:18), 1, mean)
low_I <- apply(subset(mean_rates, select = c(1:3, 10:12)), 1, mean)
high_I <- apply(subset(mean_rates, select = c(7:9, 16:18)), 1, mean)
high_ROE <- apply(subset(mean_rates, select = seq(3, 18, 3)), 1, mean)
low_ROE <- apply(subset(mean_rates, select = seq(1, 16, 3)), 1, mean)

r_ME <- small - big
r_I <- low_I - high_I
r_ROE <- high_ROE - low_ROE

factors <- cbind(MktExcess_r, r_ME, r_I, r_ROE, r_free_ed)
colnames(factors) <- c("MKT", "r_ME", "r_I", "r_ROE", "risk-free_r")

save(factors, file = "factorsQ.RData")

