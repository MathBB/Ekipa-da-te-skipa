#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      Caha
#
# Created:     20.08.2015
# Copyright:   (c) Caha 2015
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import auth
import psycopg2,psycopg2.extensions, psycopg2.extras
import OPBfunkcije
###importas v primeru da potrebujes bazo
import ustvariBazo
### (------------ ce imamo samo datoteko --------)
##        #naredimo direktorij za podatke, ki jih bom sem downloadali.
##        #ce ta direktorij in podatki ze ne obstajajo.
##        os.makedirs(os.getcwd() +'/podatki/cene_nova')


if 'podatki' in OPBfunkcije.os.listdir():
    print('mapa s podatki je v datoteki')
    pass
else:
    OPBfunkcije.os.makedirs(OPBfunkcije.os.getcwd() + '/podatki/cene_nova')
    print('pripravi mape')






vsiTickerji = OPBfunkcije.VsitickerIzBorz()

testniTickerji = OPBfunkcije.zmansatTickerje(vsiTickerji,50)


zanicFinanci=OPBfunkcije.zajemPodatkov(testniTickerji)

#da ne vlecemo vedno cen dolj Pazit na direktorji ce je v drugi mapi!!!!!!!!!!
vsebina = OPBfunkcije.os.listdir(path='podatki/cene_nova/')
if len(vsebina)==0:
    tikrjiCen=OPBfunkcije.zajemCen(testniTickerji)
else:
    tikrjiCen=[]
    for i in vsebina:
        tikrjiCen.append(i.strip('.csv'))





###(----podatki ki niso cisto popolni----)
##iscemo nov seznam tickerej, oz nov presek, ki bo dober za dat podatke v bazo.


###testni tikerji so vsi ipo pred 2014 in imajo podatke o sektorjih( rabimo za bazo)
##    ko zajamemo podatek, nekateri nimajo informacij o financnih izkazih
##    nekateri pa nimajo podatkov o cenah. Lahko zajamemo samo take, ki imajo podatke o cenah
##    in take ki imajo podatke, lahko pa vse to reguliramo z select stavki.


## 1 za zajem podatkov rabimo presek tikerjev ki imajo podatke

delovnaMapa=OPBfunkcije.os.getcwd()
#uredi po abecedi
podmape = OPBfunkcije.os.listdir(delovnaMapa+'/podatki')


tikrjiIncome = OPBfunkcije.poisciTikerjeZpodatki('podatki/income16_8.csv')
tikrjiBalance = OPBfunkcije.poisciTikerjeZpodatki('podatki/balance-sheet16_8.csv')





presek = OPBfunkcije.presek(tikrjiIncome,tikrjiBalance)
presek1 = OPBfunkcije.presek(tikrjiIncome,tikrjiCen)

tikrjiVbazo = OPBfunkcije.presek(presek,presek1)
###naredl bom presek vseh stirih podatkov med cash,balance,cene,income tikerji


income = OPBfunkcije.netIncome('podatki/income16_8.csv')
totalAssets = OPBfunkcije.totalAssets('podatki/balance-sheet16_8.csv')


########## polnim bazo
BAZA = "baza28_8.db"


#polnimo sektorje in cene ter podjetja
sektorji=OPBfunkcije.polniSektorje(tikrjiVbazo,BAZA)

OPBfunkcije.polniFundamentalne(totalAssets,income,tikrjiVbazo,BAZA)

#odvisno od baze ki jo uporabljamo
#baza =   OPBfunkcije.sqlite3.connect(BAZA)

baza = psycopg2.connect(database=auth.db, host=auth.host, user=auth.user, password=auth.password)

(baza.execute('''select * from podjetja''')).fetchall()


(baza.execute('''select * from sektor''')).fetchall()


(baza.execute('''select * from cene where volume < 10000 ''')).fetchall()


(baza.execute('''select * from fundamentalni where NetIncome < 100000''')).fetchall()

baza.close()
