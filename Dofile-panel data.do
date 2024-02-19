clear all
use "C:\Users\USER\Documents\MASTER ECONOMICS AND PUBLIC POLICY\TESIS\PAST TESIS\GOVERNMENT EXPENDITURE\DATOS\STATA\new.dta"
xtset countrynum year

gen lngini=ln(gini)
****DOFILE DISSERTATION MSC.ECONOMICS AND PUBLIC POLICY*************************
********************************************************************************
///UNIVERSITY OF YORK 2019 DIANA CONTRERAS ARIAS


*********************************************************************************
******************SUMMARY STATISTICS*********************************************
*********************************************************************************
*********************************************************************************

summarize gini sh se sp st gdp pgdp elder aged popgro unem2 unem trade eduy rem urb pprim psec
summarize gini sh se sp st gdp pgdp elder aged popgro unem2 unem trade eduy rem urb pprim psec,detail

********************GRAPH INITIAL************************************************
xtline gini
xtline gini, overlay
bysort countrynum: egen gini_mean=mean(gini)
twoway scatter gini countrynum, msymbol(circle_hollow)|| connected gini_mean countrynum, msymbol (diamond)||,xlabel(1 "CR" 2 "ESA" 3 "HON" 4 "PAN" 5 "RD")
twoway scatter gini se, mlabel (country) || lfit gini st ,clstyle(p2)

xi: regress gini st i.countrynum
predict ginihat
separate gini, by(countrynum)
separate ginihat, by(countrynum)
twoway connected ginihat1-ginihat7 st, msymbol(none diamond_hollow triangle_hollow square_hollo w + circle_hollow x) msize(medium) mcolor(black black black black black black black) || lfit gini st, clwidth(thick) clcolor(black)

***1)test unit root
xtwest loghex loggdp, lags(1 3) leads(0 3) lrwindow(3) constant trend 
***2) test cointegration test
xtwest lngini elder, constant trend lags(1)


*********************************************************************************
*********************************************************************************
****************************REGRESIONS*******************************************
*********************************************************************************
//4AGO-TEST COINTEGRACION
xtwest lngini sh,constant trend lags(1) westerlund
xtwest lngini rem,constant trend lags(1) westerlund
//TEST DE UNIT ROOT
xtunitroot ips sp, trend lags(1)
//opcion1-modelo
xi: xtabond lngini sh se sp trade elder rem urb unem eduy  i.countrynum , maxldep(2)vce(robust) ///para GMM
xtreg lngini sh se sp trade elder rem urb unem eduy, fe robust ///par FIXED

//MODELO2
reg lngini sh se sp popgro eduy rem aged gdp
xtreg lngini sh se sp popgro eduy rem aged gdp, fe
xi: xtabond lngini sh se sp popgro eduy rem aged gdp i.countrynum, vce(robust)
xi: xtgls lngini sh se sp popgro eduy rem aged gdp i.countrynum

//MODELO3

//MODELO 4-REGRESION LINEAL-LINEAL
reg gini sh se sp popgro eduy rem aged gdp
xtreg gini sh se sp popgro eduy rem aged gdp, fe
xi: xtabond gini sh se sp popgro eduy rem aged gdp i.countrynum, vce(robust)
xi: xtgls gini sh se sp popgro eduy rem aged gdp i.countrynum

//MODELO 5-posibles
xi: xtabond gini sh se sp eduy rem aged lnpgdp urb unem i.countrynum, vce(robust)
xi: xtabond gini sh se sp rem aged lnpgdp urb trade i.countrynum, vce(robust)

gen lnpgdp=ln(pgdp)

//MODEO ELEJIDO Y PROBADO CON TESTS DE AUT(2)
xi: xtabond gini sh se sp eduy rem aged lnpgdp urb unem, vce(robust)
estat abond




//22 julio:MODELO 1

reg GINI SHEA SEDU SPRO GDP POPGRO PPRIM
xtreg GINI SHEA SEDU SPRO GDP POPGRO PPRIM, fe
estimate store fixed
xtreg GINI SHEA SEDU SPRO GDP POPGRO PPRIM, re
estimate store random
hausman fixed random
//test del 23 julio: DEBO CONSIDERAR EFECTOS FIJOS EN EL TIEMPO
predict resid, residual
jb resid
histogram resid, kdensity normal
xtreg GINI SHEA SEDU SPRO GDP POPGRO PPRIM, fe
xttest2
xtreg GINI SHEA SEDU SPRO GDP POPGRO PPRIM, fe
xtcsd, pesaran abs
xtserial GINI SHEA SEDU SPRO GDP POPGRO PPRIM
xtreg GINI SHEA SEDU SPRO GDP POPGRO PPRIM i.Year, fe
testparm i.Year

**Para corregir tema de heteroskedasticidad
xi: xtgls GINI SHEA SEDU SPRO GDP POPGRO PPRIM i.countrynum

//para aplicar GMM
xtabond2 GINI SHEA SEDU SPRO GDP POPGRO PPRIM, gmm (GINI SHEA SEDU SPRO) nolevel small
//Arellano-Bond First-Differenced GMM
xi: xtabond GINI SHEA SEDU SPRO GDP POPGRO PPRIM i.countrynum , maxldep(2)vce(robust)
**twosteps GMM
xtabond2 GINI SHEA SEDU SPRO GDP POPGRO PPRIM, gmm (GINI SHEA SEDU SPRO) twostep small

//MODELO 2: el mejor hasta ahora por signos en edu y salud(disminuyen desigualdad)
reg GINI SHEA SEDU SPRO OPE ELDER REM URB UNEM EDUY
xtreg GINI SHEA SEDU SPRO OPE ELDER REM URB UNEM EDUY, fe
estimate store fixed
xtreg GINI SHEA SEDU SPRO OPE ELDER REM URB UNEM EDUY, re
estimate store random
hausman fixed random
xtreg GINI SHEA SEDU SPRO OPE ELDER REM URB UNEM EDUY, fe
xttest3
xtreg GINI SHEA SEDU SPRO OPE ELDER REM URB UNEM EDUY, fe robust
xi: xtgls GINI SHEA SEDU SPRO OPE ELDER REM URB UNEM EDUY i.countrynum
xi: xtabond GINI SHEA SEDU SPRO OPE ELDER REM URB UNEM EDUY i.countrynum , maxldep(2)vce(robust)


reg lngini lnshea lnspro lnsedu lnelder lnagdp lnpgdp lnarem lnunem lnself lnope lneduy lnprima lnsec lnrem lnurb
sktest lngini lnshea lnspro lnsedu lnelder lnagdp lnpgdp lnarem lnunem lnself lnope lneduy lnprima lnsec lnrem lnurb
reg lngini lnshea lnspro lnsedu lnelder lnagdp lnpgdp lnarem lnunem lnself lnope lneduy lnprima lnsec lnrem lnurb
drop if lnspro<3

//28 julio: 
**para primer modelo
xtreg gini sh se d.sp ope aged rem d.urb unem eduy, fe robust
xi: xtabond gini sh se d.sp ope aged rem d.urb unem eduy i.countrynum , maxldep(3)vce(robust)

