#.libPaths("C:/RStudio")
require("RSQLite")
require("xts")


# Vse stevilke zapisemo v obicajni obliki in ne v eksponentni.
options(scipen = 2)

library("RPostgreSQL")

# Preimenuj datoteko v  auth.R in vnesi podatke za priklop na bazo
db = 'seminarska_ahacp'
host = 'baza.fmf.uni-lj.si'
user = 'ahacp'
password = 'cygrj26y'

drv <- dbDriver("PostgreSQL")
baza <- dbConnect(drv, dbname = db, host = host,
                  user = user, password = password)

# a <- dbGetQuery(conn, "SELECT * FROM neki_se_igramigram ;")

# Rezultate poizvedbe SQL shranimo v nek objekt
data1 = dbSendQuery(baza, "SELECT datum FROM cene GROUP BY datum ORDER BY datum ASC")
# Spremenimo v data.frame
datumi = fetch(data1, n = -1)
# Počistimo in zapremo povezavo do baze
dbClearResult(data1)


data2 = dbSendQuery(baza, "SELECT ticker FROM podjetja GROUP BY ticker")
tickerji = fetch(data2, n = -1)
dbClearResult(data2)

# Naredimo poizvedbo, s katero ustvarimo vektor, v katerem so shranjeni datumi,
# ki predstavljajo konec cetrtletja.
data3 = dbSendQuery(baza, "SELECT datum FROM fundamentalni GROUP BY datum ORDER BY datum ASC")
datumiQ = fetch(data3, n = -1)
dbClearResult(data3)

# Ustvarimo xts matriko, v katero bomo shranjevali cene delnic.
prices <- data.frame(matrix(NA, nrow = nrow(datumi), ncol = length(tickerji[,1])))
rownames(prices) <- datumi[, 1]
colnames(prices) <- tickerji[, 1]
prices <- as.xts(prices, order.by = as.Date(rownames(prices)))

dnevi <- index(prices)
delnice <- colnames(prices)

# Za vsako delnico vektor cen shranimo v nov stolpec xts matrike s cenami.
# Spotoma vrednosti pretvorimo se v stevilke.
for (i in delnice){
  data = dbSendQuery(baza, paste("SELECT datum, adjprice FROM cene WHERE ticker = '", i, "'", " ORDER BY datum ASC", sep = ""))  
  ticker = fetch(data, n = -1)
  dbClearResult(data)
  ticker = head(ticker, -1)
  ticker <- as.xts(ticker[,2], order.by = as.Date(ticker[,1]))
  dnevi_t <- index(ticker)
  prices[dnevi_t, i] <- as.numeric(ticker[dnevi_t])}

prices <- head(prices, -1)
#prices <- prices[,apply(!is.na(prices), 2, all)]

dnevi <- index(prices)
delnice <- colnames(prices)

# Ustvarimo se xts matrike, v katere bomo shranjevali fundamentalne podatke.
assets <- as.data.frame(matrix(0, nrow = length(dnevi), ncol = length(delnice)))
rownames(assets) <- dnevi
colnames(assets) <- delnice
income <- shares <- assets <- bookval <- as.xts(assets, order.by = as.Date(rownames(assets)))

# Enako kot xts matriko s cenami delnic napolnimo tudi xts matrike s preostalimi fundamentalnimi podatki.
# Tudi tu spotoma vrednosti pretvorimo se v stevilke, poleg tega pa se vse NA-je v numericne NA-je.
for (i in delnice){
  data = dbSendQuery(baza, paste("SELECT datum,NetIncome,SharesOutstand,TotalAssets,ShareholdEquity 
                                 FROM fundamentalni WHERE ticker = '", i, "'", " ORDER BY datum ASC", sep = ""))
  ticker = fetch(data, n = -1)
  dbClearResult(data)
  ticker <- as.xts(ticker[,2:5], order.by = as.Date(ticker[,1]))
  ticker[ticker == 0] <- NA
  ticker[ticker == "NA"] <- NA
  dnevi_t <- index(ticker)
  income[dnevi_t, i] <- as.numeric(ticker[dnevi_t, 1])
  shares[dnevi_t, i] <- as.numeric(ticker[dnevi_t, 2])
  assets[dnevi_t, i] <- as.numeric(ticker[dnevi_t, 3])
  bookval[dnevi_t, i] <- as.numeric(ticker[dnevi_t, 4])}

# S funkcijo "poprava" skopiramo posamezen kvartalni podatek pod vsak datum,
# ki se nahaja znotraj tega posameznega kvartala.
income <- poprava(income)
shares <- poprava(shares)
assets <- poprava(assets)
bookval <- poprava(bookval)

# xts matrikam odrežemo vrstice, kjer se ni fundamentalnih podatkov (kjer so sami NA-ji).
obd <- paste(datumiQ[1,], "::", sep = "")

income <- income[obd]
shares <- shares[obd]
assets <- assets[obd]
bookval <- bookval[obd]





