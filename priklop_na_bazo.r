setwd("C:/Users/Matija Vodišek/Documents/Faculty/PROJEKT/Faktorski modeli")
.libPaths("C:/RStudio")

require("RSQLite")

dbdat = "grizem.db"
sqlite = dbDriver("SQLite")

# Assign the connection string to a connection object
baza = dbConnect(sqlite, dbdat)

# Rezultate poizvedbe SQL shranimo v nek objekt
datumi = dbSendQuery(baza, "SELECT datum FROM cene GROUP BY datum")
# Spremenimo v data.frame
data1 = fetch(datumi, n = -1)
# Počistimo in zapremo povezavo do baze
dbClearResult(datumi)

tickerji = dbSendQuery(baza, "SELECT ticker FROM podjetja GROUP BY ticker")
data2 = fetch(tickerji, n = -1)
dbClearResult(tickerji)

adjPrices <- data.frame(matrix(, nrow = nrow(data1), ncol = nrow(data2)))
rownames(adjPrices) <- data1[, 1]
colnames(adjPrices) <- data2[, 1]

dnevi <- rownames(adjPrices)
len1 <- length(dnevi)
delnice <- colnames(adjPrices)

for (i in delnice){
  ticker = dbSendQuery(baza, paste("SELECT adjPrice FROM cene WHERE ticker = '", i, "'", " ORDER BY datum ASC", sep = ""))  
  data = fetch(ticker, n = -1)
  dbClearResult(ticker)
  len <- nrow(data)
  adjPrices[(len1-len+1):len1, i] <- data}

save(adjPrices, file = "Cene.RData")





