/*
----------------------------------------------------------------------------------------------------------------------------------------------------------------
Replication files of the appendix for the paper "How to Get Coal Country to Vote for Climate Policy: The Effect of a `Just Transition Agreement' on Spanish Election Results". 
----------------------------------------------------------------------------------------------------------------------------------------------------------------
*/

*Version 17.1 // in case Stata's syntax changes in future versions

*********************************************************************************************
******************************************Table C.1: Summary Statistics***********************
*********************************************************************************************

use  "$/replication_just_transition/2008.dta", clear
append using "$/replication_just_transition/2011.dta"
append using "$/replication_just_transition/2015.dta"
append using "$/replication_just_transition/2016.dta"
append using "$/replication_just_transition/2019.dta"

gen time = (year> 2016) & !missing(year)
gen coalmine2=.
replace coalmine2=1 if coalmine==1
replace coalmine2=0 if coalmine==0 & province1==6
replace coalmine2=0 if coalmine==0 & province1==29
replace coalmine2=0 if coalmine==0 & province1==46
keep if province1==6 | province1==29 | province1==46
egen wanted = total(inrange(year, 2008, 2019)), by(municipality1)
keep if wanted==5

gen treated2 =(coalmine2==1) & !missing(year)
gen did2= time*treated2

merge m:m municipality province using "$/replication_just_transition/unemploymentgrowth.dta" 
drop _merge
merge m:m municipality province using "$/replication_just_transition/education.dta" 
drop _merge
merge m:m municipality province using "$/replication_just_transition/age_population.dta" 
drop _merge
merge m:m municipality using "$/replication_just_transition/miners.dta"

replace miners_share=0 if miners_share==.
gen visibility=0
replace visibility=1 if province=="Asturias" 

estpost sum psoe time treated2 log no_education growthunemployment men_50_rs immigration_rs miners_share visibility
esttab, cells("count mean sd min max")


*********************************************************************************************
******************************************Table C.2: Placebo Test***********************
*********************************************************************************************

use  "$/replication_just_transition/2008.dta", clear
append using "$/replication_just_transition/2011.dta"
append using "$/replication_just_transition/2015.dta"
append using "$/replication_just_transition/2016.dta"
append using "$/replication_just_transition/2019.dta"

encode municipality, gen(municipality1)
encode province, gen(province1)
gen treated =(coalmine==1) & !missing(year)

gen time = (year> 2016) & !missing(year)
gen coalmine2=.
replace coalmine2=1 if coalmine==1
replace coalmine2=0 if coalmine==0 & province1==6
replace coalmine2=0 if coalmine==0 & province1==29
replace coalmine2=0 if coalmine==0 & province1==46
keep if province1==6 | province1==29 | province1==46
egen wanted = total(inrange(year, 2008, 2019)), by(municipality1)
keep if wanted==5

gen treated2 =(coalmine2==1) & !missing(year)
xtset province1
xtreg psoe year##treated, fe robust
outreg2 using Table1.xls, nolabel bracket bdec(3) replace


********************************************************************************************************************
******************************************Table C.3: Covariates adjustment via entropy balancing ******************
********************************************************************************************************************

use  "$/replication_just_transition/2008.dta", clear
append using "$/replication_just_transition/2011.dta"
append using "$/replication_just_transition/2015.dta"
append using "$/replication_just_transition/2016.dta"
append using "$/replication_just_transition/2019.dta"

encode municipality, gen(municipality1)
encode province, gen(province1)

gen time = (year> 2016) & !missing(year)
gen coalmine2=.
replace coalmine2=1 if coalmine==1
replace coalmine2=0 if coalmine==0 & province1==6
replace coalmine2=0 if coalmine==0 & province1==29
replace coalmine2=0 if coalmine==0 & province1==46
keep if province1==6 | province1==29 | province1==46
egen wanted = total(inrange(year, 2008, 2019)), by(municipality1)
keep if wanted==5

gen treated2 =(coalmine2==1) & !missing(year)
gen did2= time*treated2

merge m:m municipality province using "$/replication_just_transition/unemploymentgrowth.dta" 
drop _merge
merge m:m municipality province using "$/replication_just_transition/education.dta" 
drop _merge
merge m:m municipality province using "$/replication_just_transition/age_population.dta" 
drop _merge

gen log = log(totalpopulation)
sum men_50_plus, meanonly
gen men_50_rs = ( men_50_plus - r(min) ) / ( r(max)-r(min) ) * 100
sum immigration, meanonly
gen immigration_rs = ( immigration - r(min) ) / ( r(max)-r(min) ) * 100

ebalance treated log no_education growthunemployment men_50_rs immigration_rs, targets(1)

*To get the Diff
ttest log,by(treated)
ttest no_education,by(treated)
ttest growthunemployment,by(treated)
ttest men_50_rs, by(treated)
ttest immigration_rs, by(treated)


**************************************************************************************************************************************
******************************************Figure D.1: Distribution between a municipality's centroid and the closest coalmine
**************************************************************************************************************************************

use "$/replication_just_transition/votediff.dta", replace
rename municipalities municipality
gen muni = lower(municipality)
merge m:m muni using "$/replication_just_transition/coalmines.dta"
drop if _merge==2
drop if _merge==1
sort muni province
quietly by muni  province :  gen dup1 = cond(_N==1,0,_n)
sum dup1
sum if dup1==2
drop if dup1>0
egen municipality2 = group(muni province)
rename distance_closure distance_closure0
reshape long distance_closure distance_noclosure, i(municipality2) j(which)
by municipality2: egen min_closure = min(distance_closure)
sort municipality2 min_closure
quietly by municipality2 min_closure :  gen dup2 = cond(_N==1,0,_n)
sum dup2
drop if dup2>1
drop distance_closure which _merge dup2

gen distance2 =  min_closure/1000
gen distance3= (min_closure/1000)^2
gen log1 = log(distance2)

set scheme plotplain
hist distance2, xtitle (Distance to the closest coalmine (in km)) graphregion(color(white))
graph save figure1
hist log1, xtitle ((Log) Distance to the closest coalmine (in km)) graphregion(color(white))
graph save figure2
graph combine figure1.gph figure2.gph

****************************************************************************************
******************************************Table D.1: Distance to coalmines
****************************************************************************************

use "$/replication_just_transition/votediff.dta", replace
rename municipalities municipality
gen muni = lower(municipality)
merge m:m muni using "$/replication_just_transition/coalmines.dta"
drop if _merge==2
drop if _merge==1
sort muni province
quietly by muni  province :  gen dup1 = cond(_N==1,0,_n)
sum dup1
sum if dup1==2
drop if dup1>0
egen municipality2 = group(muni province)
rename distance_closure distance_closure0
reshape long distance_closure distance_noclosure, i(municipality2) j(which)
by municipality2: egen min_closure = min(distance_closure)
sort municipality2 min_closure
quietly by municipality2 min_closure :  gen dup2 = cond(_N==1,0,_n)
sum dup2
drop if dup2>1
drop distance_closure which _merge dup2
merge m:m muni using "$/replication_just_transition/unemployment.dta"
drop _merge
merge m:m muni province using "$/replication_just_transition/2015e.dta"
drop _merge
merge m:m muni province using "$/replication_just_transition/age.dta"
drop _merge
merge m:1 muni province using "$/replication_just_transition/education1.dta"

gen log = log(totalpopulation)
sum men_50_plus, meanonly
gen men_50_rs = ( men_50_plus - r(min) ) / ( r(max)-r(min) ) * 100
sum immigration, meanonly
gen immigration_rs = ( immigration - r(min) ) / ( r(max)-r(min) ) * 100

gen distance2 =  min_closure/1000
gen distance3= (min_closure/1000)^2
gen log1 = log(distance2)

encode province, gen(province1)
xtset province1
xtreg voteshare log1,fe 
outreg2 using Table1.xls, nolabel bracket bdec(3) addtext(Province FE, YES) replace
xtreg voteshare log1 psoe log no_education growthunemployment men_50_rs immigration_rs,fe 
outreg2 using Table1.xls, nolabel bracket bdec(3) addtext(Province FE, YES) append
drop if min_closure > 300000
xtreg voteshare log1 psoe log no_education growthunemployment men_50_rs immigration_rs,fe 
outreg2 using Table1.xls, nolabel bracket bdec(3) addtext(Province FE, YES) append

****************************************************************************************
******************************************Table E.1: With municipality fixed effects
****************************************************************************************

use  "$/replication_just_transition/2008.dta", clear
append using "$/replication_just_transition/2011.dta"
append using "$/replication_just_transition/2015.dta"
append using "$/replication_just_transition/2016.dta"
append using "$/replication_just_transition/2019.dta"

encode municipality, gen(municipality1)
encode province, gen(province1)

gen time = (year> 2016) & !missing(year)
gen coalmine2=.
replace coalmine2=1 if coalmine==1
replace coalmine2=0 if coalmine==0 & province1==6
replace coalmine2=0 if coalmine==0 & province1==29
replace coalmine2=0 if coalmine==0 & province1==46
keep if province1==6 | province1==29 | province1==46
egen wanted = total(inrange(year, 2008, 2019)), by(municipality1)
keep if wanted==5

gen treated2 =(coalmine2==1) & !missing(year)
gen did2= time*treated2
xtset municipality1
xtreg psoe time treated2 did2, fe robust
outreg2 using Table1.xls, nolabel bracket bdec(3) replace

*****************************************************************************************************************
******************************************Table E.2: With the Lagged Dependent Variable (and entropy balancing)
*****************************************************************************************************************

use "$/replication_just_transition/2019.dta", replace
merge m:m municipality province using "$/replication_just_transition/2016lagged.dta"

encode municipality, gen(municipality1)
encode province, gen(province1)

drop _merge
merge m:m municipality province using "$/replication_just_transition/unemployment2016.dta" 
drop _merge
merge m:m municipality province using "$/replication_just_transition/education.dta" 
drop _merge
merge m:m municipality province using "$/replication_just_transition/age_population.dta" 
drop _merge

gen log = log(totalpopulation)
sum men_50_plus, meanonly
gen men_50_rs = ( men_50_plus - r(min) ) / ( r(max)-r(min) ) * 100
sum immigration, meanonly
gen immigration_rs = ( immigration - r(min) ) / ( r(max)-r(min) ) * 100
keep if province1==6 | province1==29 | province1==46

ebalance coalmine log no_education unemployment16 men_50_rs immigration_rs psoe2016, targets(1)

reg psoe coalmine log no_education unemployment16 men_50_rs immigration_rs psoe2016 _webal i.province1 , vce (cluster municipality1)
outreg2 using Table2.xls, nolabel bracket bdec(3) replace

*****************************************************************************************************************
******************************************Table E.3: With the full sample of municipalities
*****************************************************************************************************************

use  "$/replication_just_transition/2008.dta", clear
append using "$/replication_just_transition/2011.dta"
append using "$/replication_just_transition/2015.dta"
append using "$/replication_just_transition/2016.dta"
append using "$/replication_just_transition/2019.dta"

encode municipality, gen(municipality1)
encode province, gen(province1)

egen wanted = total(inrange(year, 2008, 2019)), by(municipality1)
keep if wanted==5
sort municipality1 year 
quietly by municipality1 year :  gen dup2 = cond(_N==1,0,_n)

gen time = (year> 2016) & !missing(year)
gen treated =(coalmine==1) & !missing(year)
gen did= time*treated

merge m:m municipality province using "$/replication_just_transition/unemploymentgrowth.dta" 
drop _merge
merge m:m municipality province using "$/replication_just_transition/education.dta" 
drop _merge
merge m:m municipality province using "$/replication_just_transition/age_population.dta" 
drop _merge

gen log = log(totalpopulation)
sum men_50_plus, meanonly
gen men_50_rs = ( men_50_plus - r(min) ) / ( r(max)-r(min) ) * 100
sum immigration, meanonly
gen immigration_rs = ( immigration - r(min) ) / ( r(max)-r(min) ) * 100

ebalance treated log no_education growthunemployment men_50_rs immigration_rs, targets(1)

reg psoe time treated did, vce(cluster municipality1)
outreg2 using Table3.xls, nolabel bracket bdec(3) replace

reg psoe time treated did i.year i.province1, vce(cluster municipality1)
outreg2 using Table3.xls, nolabel bracket bdec(3) append

reg psoe time treated did i.year log no_education growthunemployment men_50_rs immigration_rs i.province1,  vce(cluster municipality1)
outreg2 using Table3.xls, nolabel bracket bdec(3) append

reg psoe i.time i.treated did log no_education growthunemployment men_50_rs immigration_rs _webal i.province1, vce (cluster municipality1)
outreg2 using Table3.xls, nolabel bracket bdec(3) addtext(Matched, YES) append


*****************************************************************************************************************
******************************************Table E.4: Placebo tests
*****************************************************************************************************************


use "$/replication_just_transition/2019_turnout_podemos.dta", replace
append using "$/replication_just_transition/2016_turnout_podemos.dta"
gen time = (year> 2016) & !missing(year)
gen coalmine2=.
replace coalmine2=1 if coalmine==1
replace coalmine2=0 if coalmine==0 & province1==6
replace coalmine2=0 if coalmine==0 & province1==29
replace coalmine2=0 if coalmine==0 & province1==46
drop if coalmine2==.

gen treated2 =(coalmine2==1) & !missing(year)
gen did2= time*treated2
gen treated =(coalmine==1) & !missing(year)
gen did= time*treated

encode municipality, gen(municipality1)
xtset municipality1

xtreg turnout time treated2 did2 , vce(cluster municipality1)
outreg2 using Table4.xls, nolabel bracket bdec(3) replace

xtreg podemos time treated2 did2 , vce(cluster municipality1)
outreg2 using Table4.xls, nolabel bracket bdec(3) append

*****************************************************************************************************************
******************************************Table E.5: Spatial models with the spatial weighting matrix W
*****************************************************************************************************************

use  "$/replication_just_transition/2008c.dta", clear
append using "$/replication_just_transition/2011.dta"
append using "$/replication_just_transition/2015.dta"
append using "$/replication_just_transition/2016.dta"
append using "$/replication_just_transition/2019.dta"
merge m:m  municipality  using "$/replication_just_transition/distance_closure_coalmines.dta"
encode province, gen(province1)
keep if province1==6 | province1==29 | province1==46
encode municipality, gen(municipality1)

xtset municipality1 year
egen id2 = group(municipality1 year)
drop distance_closure distance_closure1 distance_closure2 distance_closure3 distance_closure4 distance_closure5 distance_closure6 distance_closure7 distance_closure8 distance_closure9 distance_closure10 distance_closure11 distance_closure12 distance_closure13 distance_closure14 distance_closure15 distance_closure16 distance_closure17 distance_closure18 distance_closure19 distance_closure20 distance_closure21 distance_closure22 distance_closure23 distance_closure24 distance_closure25 distance_closure26 distance_closure27
rename municipality1 _ID 
by _ID: gen nyear=[_N]
keep if nyear == 5
egen wanted = total(inrange(year, 2008, 2019)), by(_ID)
tab wanted
xtset _ID year
egen id1 = group(_ID year)
spbalance
drop wanted 

spbalance
bysort _ID (year): assert Latitude == Latitude[1]
bysort _ID (year): assert Longitude == Longitude[1]
 xtset _ID year
rename _ID mun
 xtset mun year
spset mun, coord(Latitude Longitude)
spmatrix create idistance Idist  if year == 2019
spmatrix summarize Idist

gen time = (year> 2016) & !missing(year)
gen coalmine2=.
replace coalmine2=1 if coalmine==1
replace coalmine2=0 if coalmine==0 & province1==6
replace coalmine2=0 if coalmine==0 & province1==29
replace coalmine2=0 if coalmine==0 & province1==46
gen treated2 =(coalmine2==1) & !missing(year)
gen did2= time*treated2

spxtregress psoe time treated2 did2, fe dvarlag(Idist)
outreg2 using Table5.xls, nolabel bracket bdec(3) replace

spxtregress psoe time treated2 did2, fe dvarlag(Idist) errorlag(Idist)
outreg2 using Table5, nolabel bracket bdec(3) append

spxtregress psoe time treated2 did2, fe dvarlag(Idist) errorlag(Idist) ivarlag(Idist: time treated2 did2) 
outreg2 using Table5.xls, nolabel bracket bdec(3)  append

***********************************************************************************************************************************************************
******************************************Figure F.1: Marginal plot of the distance to the closest coalmine on the change in PSOE Vote Share (2016-2019)
***********************************************************************************************************************************************************

use "$/replication_just_transition/votediff.dta", replace
rename municipalities municipality
gen muni = lower(municipality)
merge m:m muni using "$/replication_just_transition/coalmines.dta"
drop if _merge==2
drop if _merge==1
sort muni province
quietly by muni  province :  gen dup1 = cond(_N==1,0,_n)
sum dup1
sum if dup1==2
drop if dup1>0
egen municipality2 = group(muni province)
rename distance_closure distance_closure0
reshape long distance_closure distance_noclosure, i(municipality2) j(which)
by municipality2: egen min_closure = min(distance_closure)
sort municipality2 min_closure
quietly by municipality2 min_closure :  gen dup2 = cond(_N==1,0,_n)
sum dup2
drop if dup2>1
drop distance_closure which _merge dup2

encode province, gen(province1)
xtset province1

xtreg voteshare distance2,fe 
margins, at(distance2 = (0.5(200)1775))
marginsplot, ytitle (Change in PSOE Vote Share (2016-2019)) xtitle (Distance to the closest coalmine (in km))

***********************************************************************************************************************************************************
******************************************Table F.1 : Decrease of PSOE vote share (2016-2019) as the distance to the closest coalmines increases
***********************************************************************************************************************************************************

use "$/replication_just_transition/votediff.dta", replace
rename municipalities municipality
gen muni = lower(municipality)
merge m:m muni using "$/replication_just_transition/coalmines.dta"
drop if _merge==2
drop if _merge==1
sort muni province
quietly by muni  province :  gen dup1 = cond(_N==1,0,_n)
sum dup1
sum if dup1==2
drop if dup1>0
egen municipality2 = group(muni province)
rename distance_closure distance_closure0
reshape long distance_closure distance_noclosure, i(municipality2) j(which)
by municipality2: egen min_closure = min(distance_closure)
sort municipality2 min_closure
quietly by municipality2 min_closure :  gen dup2 = cond(_N==1,0,_n)
sum dup2
drop if dup2>1
drop distance_closure which _merge dup2

encode province, gen(province1)
xtset province1

xtreg voteshare distance2,fe 
outreg2 using Table1.xls, nolabel bracket bdec(3) replace

