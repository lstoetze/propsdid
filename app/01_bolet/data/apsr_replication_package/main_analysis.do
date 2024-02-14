/*
----------------------------------------------------------------------------------------------------------------------------------------------------------------
Replication files of the main analysis for the paper "How to Get Coal Country to Vote for Climate Policy: The Effect of a `Just Transition Agreement' on Spanish Election Results". 
----------------------------------------------------------------------------------------------------------------------------------------------------------------
*/

*Version 17.1 // in case Stata's syntax changes in future versions

*********************************************************************************************
******************************************Figure 2: DiD graph***********************
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

set scheme plotplain
lgraph psoe year, by(treated) stat(mean) xline(2016) ylab(, nogrid) xlab(2008(2)2020) scheme(s2mono) legend(order(1 "Control Municipalities" 2 "Treated Municipalities")) ytitle (PSOE Vote Share (in %)) xtitle (Year) graphregion(color(white))


*********************************************************************************************
******************************************Table 1: Main results of DiD ***********************
*********************************************************************************************

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

ebalance treated2 log no_education growthunemployment men_50_rs immigration_rs, targets(1)

***Table 1
reg psoe time treated2 did2, vce(cluster municipality1)
outreg2 using Table1.xls, nolabel bracket bdec(3) replace

reg psoe time treated2 did2 i.year i.province1, vce(cluster municipality1)
outreg2 using Table1.xls, nolabel bracket bdec(3) append

reg psoe time treated2 did2 i.year log no_education growthunemployment men_50_rs immigration_rs i.province1,  vce(cluster municipality1)
outreg2 using Table1.xls, nolabel bracket bdec(3) append

reg psoe i.time i.treated2 did2 log no_education growthunemployment men_50_rs immigration_rs _webal i.province1, vce (cluster municipality1)
outreg2 using Table1.xls, nolabel bracket bdec(3) addtext(Matched, YES) append


*********************************************************************************************
******************************************Table 2: Mechanisms ***********************
*********************************************************************************************

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

merge m:m municipality using "$/miners.dta"
replace miners_share=0 if miners_share==.

gen visibility=0
replace visibility=1 if province=="Asturias" 

reg psoe c.miners_share##i.time i.province1 i.year if coalmine2==1, cl(municipality1) robust
outreg2 using Table2.xls, nolabel bracket bdec(3) replace

reg psoe c.miners_share##i.time log no_education growthunemployment men_50_rs immigration_rs i.province1 i.year if coalmine2==1, cl(municipality1) robust
outreg2 using Table2.xls, nolabel bracket bdec(3) append

reg psoe i.visibility##i.time i.year if coalmine2==1, cl(municipality1) robust
outreg2 using Table2.xls, nolabel bracket bdec(3) append

reg psoe i.visibility##i.time log no_education growthunemployment men_50_rs immigration_rs i.year if coalmine2==1, cl(municipality1) robust
outreg2 using Table2.xls, nolabel bracket bdec(3) append

