
#import sqlite3
import psycopg2, psycopg2.extensions, psycopg2.extras
import auth

psycopg2.extensions.register_type(psycopg2.extensions.UNICODE) # se znebimo problemov s sumniki
##sestavljanje baze ################################

print(auth.db )
## priklop na postgres ###
baza = psycopg2.connect(database=auth.db, host=auth.host, user=auth.user, password=auth.password)

## Definiramo ime baze za priklop na sqlite (izbira po zelji)
#BAZA = "baza28_8.db"

## Naredimo povezavo z bazo. Funkcija sqlite3.connect vrne objekt, ki hrani podatke o povezavi z bazo.
#baza = sqlite3.connect(BAZA)
##(----------------konec priklopa ------------------)



cur = baza.cursor()

cur.execute(''' DROP TABLE  fundamentalni''')
cur.execute(''' DROP TABLE  cene''')
cur.execute(''' DROP TABLE  podjetja''')
cur.execute(''' DROP TABLE  sektor''')


cur.execute('''CREATE TABLE IF NOT EXISTS sektor (
  ime_sektorja TEXT PRIMARY KEY)''')


cur.execute('''CREATE TABLE IF NOT EXISTS podjetja (
  ticker    TEXT PRIMARY KEY,
  ime   TEXT,
  IPOyear INTEGER,
  sektor TEXT,
  FOREIGN KEY(sektor) REFERENCES sektor(ime_sektorja))
''')


## tiker in datum skupaj bosta primarna kljuca!!
cur.execute('''CREATE TABLE IF NOT EXISTS cene (
    ticker TEXT,
    datum DATE,
    volume INTEGER,
    adjPrice FLOAT(24),
    FOREIGN KEY(ticker) REFERENCES podjetja(ticker),
    PRIMARY KEY(ticker,datum))
''')


cur.execute('''CREATE TABLE IF NOT EXISTS fundamentalni (
    ticker TEXT,
    datum DATE,
    NetIncome TEXT,
    SharesOutstand TEXT,
    TotalAssets TEXT,
    ShareholdEquity TEXT,
    FOREIGN KEY(ticker) REFERENCES podjetja(ticker),
    PRIMARY KEY(ticker,datum))
''')


baza.commit()
cur.close()
baza.close()

