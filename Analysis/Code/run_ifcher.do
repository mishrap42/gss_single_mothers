/* Run_Ifcher.do */
* Replicates Ifcher (2013) analysis

*******************************************************************************
/* SETUP */
clear matrix
clear mata
clear all
set more off
clear programs


/* Set project directory (EACH USER SHOULD EDIT, ALL OTHER PATHS SHOULD WORK) */ 

if "`c(username)'"=="prakashmishra"{
	local PROJECTDIR "/Users/prakashmishra/Documents/JrSem1/Ben/gss_single_mothers/"
}
else if "`c(username)'"=="benlockwood" | "`c(username)'"=="benlo" {
	local PROJECTDIR "~/Dropbox/Research/github_repositories/github_PresentBiasDynamics" 
}

cd `PROJECTDIR'/Analysis/Code/
local INPUTDIR "`PROJECTDIR'/Analysis/Inputs"
local INTERDIR "`PROJECTDIR'/Analysis/Intermediate"
local OUTPUTDIR "`PROJECTDIR'/Analysis/Outputs"
local FIGUREDIR "`OUTPUTDIR'/figures"


/*

use "/Users/prakashmishra/Dropbox/gss_single_mothers/Analysis/Inputs/GSS7216_R2.DTA"
*******************************************************************************


keep year id wrkstat found marital oversamp martype form childs educ sex age babies preteen teens sample adults unrelat income work10 nathealy happy health life helpful trust satfam sathealt control wtss region widowed divorce spaneng

save "`INTERDIR'/gss_clean.dta", replace

*******************************************************************************

*/

/*
*ADJUST VARS AND NAMES
use "`INTERDIR'/gss_clean.dta"
rename form FORM
rename sample SAMPLE
replace happy=4-happy

* CLEAR INTERVIEWS CONDUCTED IN SPANISH
drop if spaneng == 2

* DROP BLACK OVERSAMPLES - https://gssdataexplorer.norc.org/pages/show?page=gss%2Fweighting
drop if SAMPLE == 4
drop if SAMPLE == 5
drop if SAMPLE == 7

* ADJUST FOR INFLATION
gen yr_inc=year-1
gen cpi=.
for X in num 1971/2015 \ Y in any 40.5 41.8 44.4 49.3 53.8 56.9 60.6 65.2 72.6 82.4 90.9 96.5 99.6 103.9 107.6 109.6 113.6 118.3 124 130.7 136.2 140.3 144.5 148.2 152.4 156.9 160.5 163 166.6 172.2 177.1 179.9 184 188.9 195.3 20.6 207.3 215.3 214.5 218.1 224.9 229.6 233 236.7 237 : replace cpi=Y if yr_inc==X
la var cpi "CPI-RS; with CPI-U for https://www.minneapolisfed.org/community/teaching-aids/cpi-calculator-information/consumer-price-index-and-inflation-rates-1913"
gen faminc=income/cpi*240
la var faminc "Last year's family income, 2016 dollars"

* MAKE ADJUSTMENT FOR SAMPLING INCONSISTENCIES USING STEVENSON AND WOLFERS 2008
gen married=marital==1
for any a1 a2: gen X=0
replace a1=1 if (year==1972 & marital==1) | (year==1980 & marital==1 & FORM==3) | (year==1987 & marital==1 & FORM==3)
replace a2=1 if (year==1972) | (year==1985) | (year==1986 & FORM==2) | (year==1987 & FORM==2) | (year==1987 & FORM==3)
xi: oprobit happy a1 a2 i.married*i.year [pw=wt] 
local b1=_b[a1]
local b2=_b[a2]
xi: oprobit happy i.year [pw=wt]
predict hap_hat_raw if happy~=., xb
la var hap_hat_raw "Uncorrected ordered probit index"
gen hap_hat_adj=hap_hat_raw-`b1'*a1-`b2'*a2
xi: reg hap_hat_adj i.year [pw=wt]
predict hap_hat if hap_hat_adj~=.
la var hap_hat "Corrected ordered probit index"
drop hap_hat_adj
table year, c(m hap_hat m hap_hat_raw )
egen t=tag(year)
reg hap_hat year if t==1
reg hap_hat_raw year if t==1

gen correction_factor=.
la var correction_factor "Reweighting to correct for series breaks"
levelsof happy, local(happylevel)
foreach l of local happylevel {
	gen hap`l'=1 if happy==`l'
	la var hap`l' "Happiness==`l': binary indicator"
	replace hap`l'=0 if happy~=. & happy~=`l'
	xi: reg hap`l' a1 a2 i.married*i.year [pw=wt]
	local b1=_b[a1]
	local b2=_b[a2]
	predict corrected if hap`l'==1
	replace correction_factor=(corrected-`b1'*a1-`b2'*a2)/corrected if hap`l'==1
	drop corrected
	xi: reg hap`l' i.year [pw=wt]
	predict hap`l'_hat_raw if happy~=., xb
	gen hap`l'_hat_adj=hap`l'_hat_raw-`b1'*a1-`b2'*a2
	xi: reg hap`l'_hat_adj i.year [pw=wt]
	predict hap`l'_hat if hap`l'_hat_adj~=.
	la var hap`l'_hat "Corrected proportion choosing `l'"
	table year [aw=wt], c(m hap`l' m hap`l'_hat_raw m hap`l'_hat_adj m hap`l'_hat)
}
gen wt_correction=wtss*correction_factor
la var wt_correction "Weight, adjusting for happiness series breaks"
table year [aw=wt_correction], c(m hap1_hat_adj m hap2_hat_adj m hap3_hat_adj n hap1_hat_adj n happy)
tab year happy [aw=wt_correction], row nofreq

* FOR CONSISTENCY REMOVE OBSERVATIONS AGED ABOVE 45 YEARS OF AGE
drop if age > 45
generate single_mother = 1
replace single_mother = 0 if sex == 1
replace single_mother = 0 if marital == 1
replace single_mother = 0 if childs == 0
replace single_mother = 0 if babies + preteen + teens < 1 
label define single_mother_label 1 "Single Mother - Ifcher" 0 "Not Single Mother"
label values single_mother single_mother_label
tab single_mother

save "`INTERDIR'/gss_ifcher_data.dta", replace
*/

* MEAN COMPARISONS
use "`INTERDIR'/gss_ifcher_data.dta"

generate women_except_single = 0
replace women_except_single = 1 if single_mother == 0 & sex == 2

generate single_childless = 0
replace single_childless = 1 if marital > 1 & babies + preteen + teens < 1 & sex == 2

generate married_mothers = 0
replace married_mothers = 1 if marital == 1 & babies + preteen + teens  > 0 & sex == 2

mean faminc, over(single_mother)

mean happy, over(single_mother)

ttest happy, by(single_mother)

