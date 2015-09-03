#postavitev baze za opb

import os
import sqlite3        # KnjiĂ„â€šĂ˘â‚¬ĹľÄ‚â€žĂ˘â‚¬Â¦Ă„â€šĂ˘â‚¬ĹľÄ‚â€žĂ„Äľnica za delo z bazo
import csv            # KnjiĂ„â€šĂ˘â‚¬ĹľÄ‚â€žĂ˘â‚¬Â¦Ă„â€šĂ˘â‚¬ĹľÄ‚â€žĂ„Äľnica za delo s CSV datotekami
import urllib.request # KnjiĂ„â€šĂ˘â‚¬ĹľÄ‚â€žĂ˘â‚¬Â¦Ă„â€šĂ˘â‚¬ĹľÄ‚â€žĂ„Äľnica za delo s spletom
import re             # KnjiĂ„â€šĂ˘â‚¬ĹľÄ‚â€žĂ˘â‚¬Â¦Ă„â€šĂ˘â‚¬ĹľÄ‚â€žĂ„Äľnica za delo z regularnimi izrazi
import numpy
import time
import datetime
from time import time,sleep

from bs4 import BeautifulSoup

#takole naredimo novo mapo mogoce
#os.mkdir('novaMapa')
<<<<<<< HEAD
=======

"""
Osnovna uporaba psycopg2
========================

Uporaba je prakticno enaka uporabi SQLite-a; bistveno drugacna je le
inicializacija, opisana spodaj.
"""

# Podobno kot bi se priklopili na sqlite:
#   import sqlite3
#   cur = sqlite3.connect('imenik.db', isolation_level=None)
# se priklopimo tudi na postgres (le da bolj na dolgo vklopimo se nekaj prakticnih razsiritev):

import psycopg2, psycopg2.extensions, psycopg2.extras
psycopg2.extensions.register_type(psycopg2.extensions.UNICODE) # se znebimo problemov s sumniki
conn = psycopg2.connect(database='banka', host='audrey.fmf.uni-lj.si', user='student', password='telebajsek')
conn.set_isolation_level(psycopg2.extensions.ISOLATION_LEVEL_AUTOCOMMIT) # onemogocimo transakcije
cur = conn.cursor(cursor_factory=psycopg2.extras.DictCursor) 

# Nadaljni ukazi so enaki ne glede na to, ali "cur" prihaja iz sqlite ali psycopg2 knjiznice:

# POZOR! Majhna razlika je le v nacinu posredovanja parametrov.
# V SQLite prostor za parametre oznacimo z "?":
#  cur.execute("SELECT * FROM transakcija WHERE znesek > ?", [x])
# V psycopg jih oznacimo z "%s", vendar potem NE uporabimo operatorja "%". 
#  cur.execute("SELECT * FROM transakcija WHERE znesek > %s" % [x])  # NAROBE!
# cur.execute("SELECT * FROM transakcija WHERE znesek > %s", [x])  # Pravilno



>>>>>>> fec1b36c6aa05b095323ee5521fad8eec54bdbef
### PARSANJE PODATKOV
#namesto aapl bodo prisli drugi tikerji
##URL = "http://www.marketwatch.com/investing/stock/aapl/financials/income/quarter"
##spletna_stran = urllib.request.urlopen(URL)
###neki od parsanja
###http://blog.miguelgrinberg.com/post/easy-web-scraping-with-python
###http://www.pythoncentral.io/introduction-to-sqlite-in-python/
###http://pandas.pydata.org/
###http://www.nasdaq.com/symbol/aapl/financials?query=income-statement
###https://scottishsnow.wordpress.com/2014/08/14/writing-to-a-database-r-and-sqlite/
###http://www.google.com/finance?q=NYSE%3AAAP&fstype=ii&ei=8BNRVdGRKYjisgHG-ICICQ
###http://finance.yahoo.com/q/hp?s=AAPL&a=11&b=12&c=1980&d=01&e=8&f=2015&g=d

######






###################################### sqlite NACIN BAZE

#### pogledam ce je baza v datoteki
# Datoteka, v kateri je baza
BAZA = "pozense.db"
# Naredimo povezavo z bazo. Funkcija sqlite3.connect vrne objekt, ki hrani podatke o povezavi z bazo.
baza = sqlite3.connect(BAZA)

baza.execute('''CREATE TABLE IF NOT EXISTS sektor (
  ime_sektorja TEXT PRIMARY KEY)''')

baza.execute('''CREATE TABLE IF NOT EXISTS podjetja (
  ticker    TEXT PRIMARY KEY,
  ime   TEXT,
  IPOyear INTEGER,
  sektor TEXT,
  FOREIGN KEY(sektor) REFERENCES sektor(ime_sektorja))
''')
## tiker in datum skupaj bosta primarna kljuca!!
baza.execute('''CREATE TABLE IF NOT EXISTS cene (
    ticker TEXT,
    datum DATE,
    volume INTEGER,
    adjPrice FLOAT(3,2),
    FOREIGN KEY(ticker) REFERENCES podjetja(ticker),
    PRIMARY KEY(ticker,datum))
''')
baza.execute('''CREATE TABLE IF NOT EXISTS fundamentalni (
    ticker TEXT,
    datum DATE,
    NetIncome FLOAT,
    SharesOutstand FLOAT,
    TotalAssets FLOAT,
    ShareholdEquity FLOAT,
    FOREIGN KEY(ticker) REFERENCES podjetja(ticker),
    PRIMARY KEY(ticker,datum))
''')

### UPDATE BAZE

#pogleda uskaljenost datumov v bazi in danasnjega datuma
lokalniCas=time.localtime()
datumDanes = str(lokalniCas[0]) + '-' + str(lokalniCas[1]) +'-'+str(lokalniCas[2])

datumi = (baza.execute("SELECT DISTINCT datum FROM cene ORDER BY datum DESC;")).fetchall()
datumVBazi = datumi[1][0] #drugi element seznama datumov prvi v tuplu

#datum fundamentalnih podatkov
dat = (baza.execute("SELECT DISTINCT datum FROM fundamentalni ORDER BY  datum DESC;")).fetchall()
datVBazFund = [dat[i][0] for i in range(len(dat))] #datumi fundamentalnih podatkov v bazi
               ###### za datume
soup = BeautifulSoup(urllib.request.urlopen('http://www.marketwatch.com/investing/stock/{0}/financials/income/quarter'.format('AAPL')).read())
neki = soup('table',{'class':'crDataTable'})[0].thead('th')
datMW = [] #datumi, ki jih ponuja MW
pretMes =  {'Mar':'03','Jun':'06','Sep':'09','Dec':'12'}
for row in range(1,len(neki)-1):
    razdelim = (neki[row].contents)[0].split('-')
    datMW.append(razdelim[2] + '-' + pretMes[razdelim[1]] + '-' + razdelim[0])


if datumDanes > datumVBazi:

    ###posodobi cene v bazi od datumaVBazi do datumaPreizkus
    ## ce datumVBazi ne obstaja po tem nastaviš nek poljuben
    ##obsotjeco bazo posodobim
    tikerjiUpdate = (baza.execute("SELECT ticker  FROM podjetja")).fetchall()
    # a= meseci (oktober 09 zacne z 0 !!) b = dnevi c= leto   (a,b,c) zacetni datum
    # (d,e,f) koncni datum
    zacDatum = datumVBazi.split(sep='-')
    a = str(int(zacDatum[1])-1)
    koncDate = datumDanes.split(sep='-')
    d = str(int(koncDate[1])-1)
    ##tikerjiUpdate so oblike [('nekej',),.. (...)]
    for i in tikerjiUpdate:
        try:
            #ce zapisem cez vec vrstic mori, zato ena dolga vrstica
            delnicaSoup = BeautifulSoup((urllib.request.urlopen("""http://real-chart.finance.yahoo.com/table.csv?s={0}&a={1}&b={2}&c={3}&d={4}&e={5}&f={6}&g=d&ignore=.csv""".format(i[0],a,zacDatum[2],zacDatum[0],d,koncDate[2],koncDate[0]))).read())
            besedilo = delnicaSoup.getText() #potegnem tekst ven
            besediloSez= besedilo.splitlines()
            st = 0
            for row in besediloSez:
                if st==0: row = next(besediloSez)
                else:
                    vrstica = row.split(sep=',')
                    baza.execute("""INSERT INTO cene VALUES (?,?,?,?)""",(i[0],vrstica[0],vrstica[5],vrstica[6]))
        except:
            print("neka neznana izjema se je pojavila ")
    baza.commit()

for objava in range(len(datMW)):
    if not(datMW[objava] in datVBazFund):
        ###začneš posodabljati fundamentalen podatke
        ### datum ki manjka ti pove stolpec ki ga bo potrebno posodobiti


    else:pass
     #nic je vse v redu





















###branje podatkov iz csv datotekic
def poisciTikerjeZpodatki(csvfile='incomeglavni.csv'):
    """poisce tikerje pri katerih so podatki, praznim tikerjem se izogne
    """
    #tikerji, ki imajo podatke!
    polniTikerji =[]
    with open(csvfile,'r') as cash:
        preberem = csv.reader(cash,delimiter=',',quotechar ='"')
        for row in preberem:
            if len(row)==1:
                while len(row)==1:
                    rowNazaj = row
                    #da ne grem predalec naprej z iteratorjem
                    try:
                        row = next(preberem)
                    except StopIteration:
                        print("pr koncu smo")
                        break
                polniTikerji.append(rowNazaj[0])
                #print("izven while",rowNazaj,row)
            else:
                pass
                #print(row[1],row[2])
    return polniTikerji

#pogledam za vse tri podatke kateri tikerji imajo podatke
cashTiker = poisciTikerjeZpodatki('cash-flow.csv')
balanceTiker = poisciTikerjeZpodatki('incomeglavni.csv')
incomeTiker = poisciTikerjeZpodatki('balance-sheet.csv')

###naredim minimalni seznam tikerjev (presek) s podatki za v bazo.
stevc =0
seznam = []
for i in balanceTiker:
        if i in cashTiker and i in incomeTiker:
            seznam.append(i)
        else:
            print(i,'ni v balance',stevc)

#na hitro preveril ce sem dobil presek
##for i in seznam:
##    if i in cashTiker and i in incomeTiker and i in balanceTiker:
##        pass
##    else:
##        print(i, 'ni v seznamu')

##iz seznama vseh, ki imajo podatke, fundamentalne, bom potegnil cene.
##http://finance.yahoo.com/q/hp?s=AAPL&a=00&b=10&c=2012&d=05&e=4&f=2015&g=d&ignore=.csv
##navadnemu url dodamo &ignore=.csv da ve da ni to html tabelca ni res
##url naslov http://real-chart.finance.yahoo.com/table.csv?s=AAPL&a=00&b=10&c=2012&d=05&e=4&f=2015&g=d&ignore=.csv


#samo pogledam kateri so vsi sektorji, ce bodo svoja tabela v sqlu
delavniTikrji = []
sektorji = []
st =0
with open('AmexNasdaqNyseList.csv','r') as sector:
    preberem = csv.reader(sector,delimiter=',',quotechar ='"')
    for row in preberem:
        #pogledam kter tiker ima sektor in podatke ter sektor ni na :)
        if not(row[6] in sektorji) and row[6] != "n/a":
            sektorji.append(row[6])
        if  row[6] != "n/a" and row[0] in seznam:
            delavniTikrji.append(row[0])
        else:
            pass

delnicaSoup = BeautifulSoup((urllib.request.urlopen('http://real-chart.finance.yahoo.com/table.csv?s=AAPL&a=00&b=10&c=2012&d=05&e=4&f=2015&g=d&ignore=.csv')).read())
b = delnicaSoup.find('p')
############## cene, potegne cene iz yahoo finance in jih shrani v nek podirektorij.
test = delavniTikrji
st=0
for i in test:
    try:
        delnicaSoup = BeautifulSoup((urllib.request.urlopen('http://real-chart.finance.yahoo.com/table.csv?s={0}&a=00&b=10&c=2012&d=05&e=4&f=2015&g=d&ignore=.csv'.format(i))).read())
        besedilo = delnicaSoup.getText() #potegnem tekst ven
        besediloSez= besedilo.splitlines()
        #pazit, da ko odpres je zraven newline....

        #relativno glede na pozicijo shranimo kt 'folder/subfolder/file.neki
        with open('cene_nova/{0}.csv'.format(i),'w',newline='') as file:
            pisem = csv.writer(file,delimiter=',',quotechar='"',quoting=csv.QUOTE_ALL)
            if st%30==0 and st !=0:
                sleep(20)
            #for row in delnicaSoup.find('p'):
            for row in besediloSez:
                vrstica = row.split(sep=',')
                pisem.writerow(vrstica)
#                    print(type(row),row,vrstica)

        st+=1
    except:
        print('manjka', st)
        st+=1



proba=[]
tikerji = []
with open('NASDAQlist.csv','r') as cash:
    preberem = csv.reader(cash,delimiter=',',quotechar ='"')
    for row in preberem:
        if row[4] =='n/a' or row[4]=='IPOyear':
            pass
        else:
            proba.append(int(row[4]))
            tikerji.append(row[1])

##naredim en csv z borzami, amex,nasdaq,nyse pa tikerji imajo MW notation

borze = ['AMEXlist.csv','NASDAQlist.csv','NYSElist.csv']
with open('AmexNasdaqNyseList.csv','w',newline='') as ANN:

    prvaVrsta = True
    for i in borze:
        with open(i,'r') as podjetja:
            preberem = csv.reader(podjetja,delimiter=',',quotechar ='"')
            #tko da samo enkrat prvo vrstico napisem
            if prvaVrsta==False:
                    row = next(preberem)
            for row in preberem:
                #postimat tikerje na MW notation
                if '/' in row[0] and '^'in row[0]:
                    row[0] = row[0].replace('^','.P')
                    row[0] = row[0].replace('/','.')
                elif '/' in row[0]: row[0] = row[0].replace('/','.')
                elif '^'in row[0]:  row[0] = row[0].replace('^','.P')
                elif ' ' in row[0]: row[0] = row[0].replace(' ','')#odstranim mutirane tikereje z presledki
                else: pass
                if not(row[0] in seznam):
                    print(row[0],'ni v seznamu')
                pisem = csv.writer(ANN,delimiter=',',quotechar='"',quoting=csv.QUOTE_ALL)
                pisem.writerow(row)
        prvaVrsta=False

#samo pogledam kateri so vsi sektorji, ce bodo svoja tabela v sqlu


with open('cash-flow.csv','r') as cash:
    preberem = csv.reader(cash,delimiter=',',quotechar ='"')

    for row in preberem:
        if i > 20: break
        else:
            if len(row)>1:
                for j in range(1,len(row)): #od 1 da tikerjev ne pobases
                    if row[j][-1] == ')' and row[j][-2]=='M' or row[j][-1]=='M':
                        row[j]= float(row[j][1:-2])*10**6
                    elif row[j][-1] == ')' and row[j][-2]=='B' or row[j][-1]=='B':
                        row[j]= float(row[j][1:-2])*10**9
                    elif row[j][-1] == ')' and row[j][-2]=='K' or row[j][-1]=='K':
                        row[j]= float(row[j][1:-2])*10**3
                    else:
                        pass
                a = type(row[1])
                print(row[0],row[1],a)
                i+=1
            else:
                print(row)

##preberemo samo tiste tikerje iz fundamentalnih podatkov,ki imajo podatke
##berem NetIncome in Basic ... iz income.csv
seznamPolnihDelnic =[]
NetIncome = []
with open('incomeglavni.csv','r') as cash:
    preberem = csv.reader(cash,delimiter=',',quotechar ='"')
    vmes = []
    for row in preberem:
        if row[0]=='feature': date= [row[1],row[2],row[3],row[4],row[5]]
        if len(row)<=1:
            vmes=[]
            while len(row)<=1:
                tiker = row[0]
                row= next(preberem)
            seznamPolnihDelnic.append(tiker)
        if ' Net Income' ==row[0]:
            #popucamo malo podatke vmes
            for j in range(1,len(row)): #od 1 da tikerjev ne pobases
                if row[j][-1] == ')' and row[j][-2]=='M' or row[j][-1]=='M':
                    row[j]= float(row[j][1:-2])*10**6
                elif row[j][-1] == ')' and row[j][-2]=='B' or row[j][-1]=='B':
                    row[j]= float(row[j][1:-2])*10**9
                elif row[j][-1] == ')' and row[j][-2]=='K' or row[j][-1]=='K':
                    row[j]= float(row[j][1:-2])*10**3
                elif row[j][-1] == ')' and row[j][0] =='(':
                    #se enkrat prevert ce je treba z 10^3 mnozit
                    row[j]= float((row[j][1:-2]).replace(',','.'))*10**3
                elif ',' in row[j]:
                    #vejice označujejo tisočice pri njih
                    row[j] = float((row[j]).replace(',','.'))*10**3
                else:
                    pass
            prvi = [tiker,date[0],row[1]]
            drugi = [tiker,date[1],row[2]]
            treti = [tiker,date[2],row[3]]
            cetrti = [tiker,date[3],row[4]]
            peti = [tiker,date[4],row[5]]
                #v NetIncome nafilam robo
                #v

        if 'Basic Shares Outstanding' == row[0]:

            for j in range(1,len(row)): #od 1 da tikerjev ne pobases
                if row[j][-1] == ')' and row[j][-2]=='M' or row[j][-1]=='M':
                    row[j]= float(row[j][1:-2])*10**6
                elif row[j][-1] == ')' and row[j][-2]=='B' or row[j][-1]=='B':
                    row[j]= float(row[j][1:-2])*10**9
                elif row[j][-1] == ')' and row[j][-2]=='K' or row[j][-1]=='K':
                    row[j]= float(row[j][1:-2])*10**3
                elif row[j][-1] == ')' and row[j][0] =='(':
                    #senkrat prevert ce je treba z 10^3 mnozit
                    row[j]= float((row[j][1:-2]).replace(',','.'))*10**3
                elif ',' in row[j]:
                    #ker vejice označujejo tisočice pri njih zgleda
                    row[j] = float((row[j]).replace(',','.'))*10**3
                else:
                    pass
            NetIncome.append(prvi   +[row[1]])
            NetIncome.append(drugi  +[row[2]])
            NetIncome.append(treti  +[row[3]])
            NetIncome.append(cetrti +[row[4]])
            NetIncome.append(peti   +[row[5]])

##preberemo samo tiste tikerje iz fundamentalnih podatkov,ki imajo podatke
##berem TotalAssets  in Total Shareholders Equity ... iz income.csv
seznamPolnihDelnicB =[]
TotalAssets = []
with open('balance-sheetglavni.csv','r') as cash:
    preberem = csv.reader(cash,delimiter=',',quotechar ='"')
    vmes = []
    for row in preberem:
        if row[0]=='feature': date= [row[1],row[2],row[3],row[4],row[5]]
        if len(row)<=1:
            vmes=[]
            while len(row)<=1:
                tiker = row[0]
                row= next(preberem)
            seznamPolnihDelnicB.append(tiker)
        if ' Total Assets' ==row[0]:
            #popucamo malo podatke vmes
            for j in range(1,len(row)): #od 1 da tikerjev ne pobases
                if row[j][-1] == ')' and row[j][-2]=='M' or row[j][-1]=='M':
                    row[j]= float(row[j][1:-2])*10**6
                elif row[j][-1] == ')' and row[j][-2]=='B' or row[j][-1]=='B':
                    row[j]= float(row[j][1:-2])*10**9
                elif row[j][-1] == ')' and row[j][-2]=='K' or row[j][-1]=='K':
                    row[j]= float(row[j][1:-2])*10**3
                elif row[j][-1] == ')' and row[j][0] =='(':
                    #se enkrat prevert ce je treba z 10^3 mnozit
                    row[j]= float((row[j][1:-2]).replace(',','.'))*10**3
                elif ',' in row[j]:
                    #vejice označujejo tisočice pri njih
                    row[j] = float((row[j]).replace(',','.'))*10**3
                else:
                    pass
            prvi = [tiker,date[0],row[1]]
            drugi = [tiker,date[1],row[2]]
            treti = [tiker,date[2],row[3]]
            cetrti = [tiker,date[3],row[4]]
            peti = [tiker,date[4],row[5]]
                #v TotalAssets nafilam robo
                #v

        if " Total Shareholders' Equity" == row[0]:

            for j in range(1,len(row)): #od 1 da tikerjev ne pobases
                if row[j][-1] == ')' and row[j][-2]=='M' or row[j][-1]=='M':
                    row[j]= float(row[j][1:-2])*10**6
                elif row[j][-1] == ')' and row[j][-2]=='B' or row[j][-1]=='B':
                    row[j]= float(row[j][1:-2])*10**9
                elif row[j][-1] == ')' and row[j][-2]=='K' or row[j][-1]=='K':
                    row[j]= float(row[j][1:-2])*10**3
                elif row[j][-1] == ')' and row[j][0] =='(':
                    #senkrat prevert ce je treba z 10^3 mnozit
                    row[j]= float((row[j][1:-2]).replace(',','.'))*10**3
                elif ',' in row[j]:
                    #ker vejice označujejo tisočice pri njih zgleda
                    row[j] = float((row[j]).replace(',','.'))*10**3
                else:
                    pass
            TotalAssets.append(prvi   +[row[1]])
            TotalAssets.append(drugi  +[row[2]])
            TotalAssets.append(treti  +[row[3]])
            TotalAssets.append(cetrti +[row[4]])
            TotalAssets.append(peti   +[row[5]])






#### ena ceska baza primer

conn = sqlite3.connect('test.db')
print("Opened database successfully")

conn.execute('''CREATE TABLE COMPANY
       (ID INT PRIMARY KEY     NOT NULL,
       NAME           TEXT    NOT NULL,
       AGE            INT     NOT NULL,
       ADDRESS        CHAR(50),
       SALARY         REAL);''')

conn.execute("INSERT INTO COMPANY (ID,NAME,AGE,ADDRESS,SALARY) \
      VALUES (1, 'Paul', 32, 'California', 20000.00 )");

conn.execute("INSERT INTO COMPANY (ID,NAME,AGE,ADDRESS,SALARY) \
      VALUES (2, 'Allen', 25, 'Texas', 15000.00 )");

conn.execute("INSERT INTO COMPANY (ID,NAME,AGE,ADDRESS,SALARY) \
      VALUES (3, 'Teddy', 23, 'Norway', 20000.00 )");

conn.execute("INSERT INTO COMPANY (ID,NAME,AGE,ADDRESS,SALARY) \
      VALUES (4, 'Mark', 25, 'Rich-Mond ', 65000.00 )");

conn.commit()
print("Records created successfully")
cursor = conn.execute("SELECT id, name, address, salary  from COMPANY")
for row in cursor:
   print( "ID = ", row[0])
   print( "NAME = ", row[1])
   print( "ADDRESS = ", row[2])
   print( "SALARY = ", row[3], "\n")
conn.close()







### Nastavitve

# Datoteka, v kateri je baza
BAZA = "pozense.db"

### Program

### 1. NAREDIMO BAZO, Ä‚â€žĂ˘â‚¬ĹˇÄ‚ËĂ˘â€šÂ¬ÄąÄľÄ‚â€žĂ„â€¦Ă„Ä…Ă‹â€ˇE JE Ă„â€šĂ˘â‚¬ĹľÄ‚â€žĂ˘â‚¬Â¦Ă„â€šĂ˘â‚¬ĹˇÄ‚â€šĂ‚Â E NIMAMO

# Naredimo povezavo z bazo. Funkcija sqlite3.connect vrne objekt,
# ki hrani podatke o povezavi z bazo.
baza = sqlite3.connect(BAZA)

#nafilam sektorje v tabelco
baza.execute('''DROP TABLE sektor''')
a=baza.execute('''SELECT IME_SEKTORJA FROM sektor''')


baza.execute('''CREATE TABLE IF NOT EXISTS sektor (
  ime_sektorja TEXT PRIMARY KEY)''')

baza.execute('''CREATE TABLE IF NOT EXISTS podjetja (
  ticker    TEXT PRIMARY KEY,
  ime   TEXT,
  IPOyear INTEGER,
  sektor TEXT,
  FOREIGN KEY(sektor) REFERENCES sektor(ime_sektorja))
''')
## tiker in datum skupaj bosta primarna kluca!!
baza.execute('''CREATE TABLE IF NOT EXISTS cene (
    ticker TEXT,
    datum DATE,
    volume INTEGER,
    adjPrice FLOAT(3,2),
    FOREIGN KEY(ticker) REFERENCES podjetja(ticker),
    PRIMARY KEY(ticker,datum))
''')

baza.execute('''CREATE TABLE IF NOT EXISTS fundamentalni (
    ticker TEXT,
    datum DATE,
    NetIncome FLOAT,
    SharesOutstand FLOAT,
    TotalAssets FLOAT,
    ShareholdEquity FLOAT,
    FOREIGN KEY(ticker) REFERENCES podjetja(ticker),
    PRIMARY KEY(ticker,datum))
''')

for i in range(1,len(sektorji)):
    #pri (sektorji[i],) obvezno vejca, da ve da je to tupl, k cene pizdarija!!!
    baza.execute("""INSERT INTO sektor VALUES (?)""",(sektorji[i],))

curzor =baza.cursor()
#naredim seznam vseh tikerjev, ki jih imam v mapi cash
#vstavimo vsakega desetega v bazo
imenaPolnihTikerjev = os.listdir(path="cene")
#testni tikerji, ki gredo v bazo
zaVBazo=[]
for i in range(0,len(imenaPolnihTikerjev),100):
    zaVBazo.append(imenaPolnihTikerjev[i].strip('.csv'))
#polnem tabelo company

with open('AmexNasdaqNyseList.csv','r') as delnice:
    preberem = csv.reader(delnice,delimiter=',',quotechar ='"')
    for row in preberem:
        if row[0] in zaVBazo:
            baza.execute("""INSERT INTO podjetja  VALUES (?,?,?,?)""",(row[0],row[1],row[4],row[5]))
        else:
            pass

for i in zaVBazo:


    with open('cene/{0}.csv'.format(i),'r',newline='\n') as price:
        berem = csv.reader(price,delimiter=',',quotechar ='"')
        ticker = i
        for row in berem:
            print(row[0],type(row),len(row))
            #baza.execute("""INSERT INTO cene VALUES (?,?,?,?)""",(i,row[0],row[5],row[6]))

with open('nova_mapa/AAU.csv','r',newline='') as price:
    #besedil= price.read()
    berem=csv.reader(price,delimiter=',',quotechar ='"',quoting=csv.QUOTE_ALL)
    for row in berem:
        print(row[0])

#vstavljam net income in basic share outstand
for i in vbazo:
    baza.execute('''INSERT INTO fundamentalni
    (ticker,datum,NetIncome,SharesOutstand) VALUES (?,?,?,?)''',(i[0],i[1],i[2],i[3])
    )
#potem pa vse te podatke updatam za stolpce, kjer je potrebno.

for i in vbazo2:
    baza.execute('''UPDATE fundamentalni SET TotalAssets='{2}',ShareholdEquity='{3}'
    WHERE ticker ='{0}' AND datum='{1}';'''.format(i[0],i[1],i[2],i[3]) )
#tezava ker je bil AND z malo ali pa ker ni bilo ; na koncu
#ce ni ticker =' ' jih ne najed
baza.commit()
baza.close()

curzor.close()



for i in range(0,len(imenaPolnihTikerjev),100):


baza.execute("""    """)

baza.commit()

baza.close()



for i in len(podsez):
    podsez[i]= podsez[i].strip(char='.csv')






# Naredimo tabelo druzba, ce je se ni
baza.execute('''CREATE TABLE IF NOT EXISTS druzba (
  tiker TEXT PRIMARY KEY,
  name TEXT,
  sektor SECONDARY KEY
)''')

# Naredimo tabelo promet, Ä‚â€žĂ˘â‚¬ĹˇÄ‚ËĂ˘â€šÂ¬ÄąÄľÄ‚â€žĂ„â€¦Ä‚â€šĂ‚Â¤e je Ă„â€šĂ˘â‚¬ĹľÄ‚â€žĂ˘â‚¬Â¦Ă„â€šĂ˘â‚¬Ä…Ä‚ËĂ˘â€šÂ¬Ă‹â€ˇe ni
baza.execute('''CREATE TABLE IF NOT EXISTS promet (
   symbol TEXT REFERENCES delnica (symbol),
   datetime TEXT,
   open REAL,
   high REAL,
   low REAL,
   close REAL
)''')

# naredimo tabelo borza, ce je se ni
baza.execute('''CREATE TABLE IF NOT EXISTS borza (
  kratica TEXT PRIMARY KEY,
  name TEXT
)''')
#generiram ukaz za tabelo ki bo imela n stolpcev za vsak tiker
#\n dojame kot novo vrstivo, to se vidi le v printu

sezTikerjev= ["A","b","C"]
tikrji ="CREATE TABEL IF NOT EXIST cene ("
st=0
for i in sezTikerjev:
    st+=1
    tikrji +="\n "+i+" REAL" + "," if st <= len(sezTikerjev)-1 else "\n "+i+" REAL"+ "\n)"

#naredimo tabelo cene ce je se ni
baza.execute('''CREATE TABLE IF NOT EXISTS cene (
  name TEXT,
  sektor SECONDARY KEY
)'''

a=["a","b","c"]

print(" nek tekst, i se ponavlja")
for i in a:print("      "+i)

a=["a","b","c"]
b=  for i in a: i

vbazo=[]
for i in NetIncome:
    if i[0] in zaVBazo:
        vbazo.append(i)

vbazo2=[]
for i in TotalAssets:
    if i[0] in zaVBazo:
        vbazo2.append(i)




