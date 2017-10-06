cd "C:\Users\jwolfers\Documents\Justin@Wharton\Happiness and inequality"

clear
set mem 1000m
set maxvar 10000
set matsize 1000
set more off
use "04697-0001-Data.dta" 

gen wt=WTSSALL if SAMPLE~=4 & SAMPLE~=5 & SAMPLE~=7 & SPANINT~=2

gen year=YEAR
gen happy=4-HAPPY
egen t_year=tag(year) 

* Create an appropriate income series
gen INCOME73=INCOME if YEAR>=1973 & YEAR<=1976 & INCOME~=0 & INCOME~=98 & INCOME~=99
for X in any 72 73 77 82 86 91 98 06 \ Y in numlist 13 13 17 18 21 22 24 26: replace INCOMEX=. if INCOMEX==Y 
gen lower=. 
gen upper=.
for X in num 1/12 \ Y in num 0 2000 4000 6000 8000 10000 12500 15000 17500 20000 25000 30000 \ Z in num 2000 4000 6000 8000 10000 12500 15000 17500 20000 25000 30000 -9: replace lower=Y if INCOME72==X \ replace upper=Z if INCOME72==X
for X in num 1/12 \ Y in num 0 1000 3000 4000 5000 6000 7000 8000 10000 15000 20000 25000 \ Z in num 1000 3000 4000 5000 6000 7000 8000 10000 15000 20000 25000 -9: replace lower=Y if INCOME73==X \ replace upper=Z if INCOME73==X
for X in num 1/16 \ Y in num 0 1000 3000 4000 5000 6000 7000 8000 10000 12500 15000 17500 20000 22500 25000 50000 \ Z in num 1000 3000 4000 5000 6000 7000 8000 10000 12500 15000 17500 20000 22500 25000 50000 -9: replace lower=Y if INCOME77==X \ replace upper=Z if INCOME77==X
for X in num 1/17 \ Y in num 0 1000 3000 4000 5000 6000 7000 8000 10000 12500 15000 17500 20000 22500 25000 35000 50000 \ Z in num 1000 3000 4000 5000 6000 7000 8000 10000 12500 15000 17500 20000 22500 25000 35000 50000 -9: replace lower=Y if INCOME82==X \ replace upper=Z if INCOME82==X
for X in num 1/20 \ Y in num 0 1000 3000 4000 5000 6000 7000 8000 10000 12500 15000 17500 20000 22500 25000 30000 35000 40000 50000 60000 \ Z in num 1000 3000 4000 5000 6000 7000 8000 10000 12500 15000 17500 20000 22500 25000 30000 35000 40000 50000 60000 -9: replace lower=Y if INCOME86==X \ replace upper=Z if INCOME86==X
for X in num 1/21 \ Y in num 0 1000 3000 4000 5000 6000 7000 8000 10000 12500 15000 17500 20000 22500 25000 30000 35000 40000 50000 60000 75000 \ Z in num 1000 3000 4000 5000 6000 7000 8000 10000 12500 15000 17500 20000 22500 25000 30000 35000 40000 50000 60000 75000 -9: replace lower=Y if INCOME91==X \ replace upper=Z if INCOME91==X
for X in num 1/23 \ Y in num 0 1000 3000 4000 5000 6000 7000 8000 10000 12500 15000 17500 20000 22500 25000 30000 35000 40000 50000 60000 75000 90000 110000 \ Z in num 1000 3000 4000 5000 6000 7000 8000 10000 12500 15000 17500 20000 22500 25000 30000 35000 40000 50000 60000 75000 90000 110000 -9: replace lower=Y if INCOME98==X \ replace upper=Z if INCOME98==X
for X in num 1/25 \ Y in num 0 1000 3000 4000 5000 6000 7000 8000 10000 12500 15000 17500 20000 22500 25000 30000 35000 40000 50000 60000 75000 90000 110000 130000 150000 \ Z in num 1000 3000 4000 5000 6000 7000 8000 10000 12500 15000 17500 20000 22500 25000 30000 35000 40000 50000 60000 75000 90000 110000 130000 150000 -9: replace lower=Y if INCOME06==X \ replace upper=Z if INCOME06==X
for var INCOME??: la var X "Family income" 

* Using interval regression approach 
gen top_coded=0 if lower~=.
gen bottom_coded=0 if lower~=.
gen lninc=.
levelsof year, local(years)
foreach y of local years {
	summ lower if year==`y' & lower~=-9
	replace bottom_coded=1 if year==`y' & lower==r(min)
	* Do interval regression
	gen u=ln(upper) if year==`y'
	replace u=. if upper==-9 & year==`y'
	gen l=ln(lower) if year==`y'
	replace l=. if lower==0 & year==`y'
	intreg l u if year==`y' [pw=wt]
	egen grp=group(lower upper) if year==`y'
	levelsof grp, local(groups)
	foreach g of local groups {
		qui summ l if year==`y' & grp==`g'
		local lower=r(mean)
		qui summ u if year==`y' & grp==`g'
		local upper=r(mean)
		predict hat, e(`lower',`upper')
		replace lninc=hat if year==`y' & grp==`g'
		drop hat
	}
	drop l u grp
}

gen income=exp(lninc)
la var lninc "Ln(Income) - interval estimation"
la var income "Nominal family income"
levelsof year, local(years)
foreach y of local years {
	table lower if year==`y', c(m upper m inc n inc n upper)
}
drop lninc

gen yr_inc=YEAR-1
gen cpi=.
for X in num 1971/2007 \ Y in any 68.2 70.3 74.7 82.1 88.9 94 100 104.4 114.4 127.1 139.2 147.6 153.9 160.2 165.7 168.7 174.4 180.8 188.6 198 205.1 210.3 215.5 220.1 225.4 231.4 236.4 239.7 244.7 252.9 260.0 264.2 270.1 277.4 286.7 296.1 304.5: replace cpi=Y if yr_inc==X
la var cpi "CPI-RS; with CPI-U for 1971-1977; http://www.bls.gov/cpi/cpiurs1978_2007.pdf"
gen faminc=income/cpi*286.7
la var faminc "Last year's family income, 2005 dollars"

* Equivalized income
gen equivs= 1+.5*(ADULTS-1)+.3*(BABIES+PRETEEN+TEENS) /* OECD-modified equivalence scale */
replace equivs=1+.5*(HOMPOP>1)+.3*(HOMPOP-2)*(HOMPOP>2) if HOMPOP~=. & equivs==.
gen faminc_equiv=faminc/equivs

* Gini coefficient: 
gen ginisample=1 if faminc_equiv+wt+happy+year~=.
egen totpop=sum(wt) if ginisample==1, by(year)
gen popshare=wt/totpop if ginisample==1~=.
egen totfaminc=sum(faminc_equiv*wt) if ginisample==1, by(year)
gen incshare=wt*faminc_equiv/totfaminc
sort ginisample year faminc_equiv
for X in any inc pop: gen cdf_X=0 \ replace cdf_X=cdf_X[_n-1]+Xshare if year==year[_n-1] & Xshare~=. & ginisample==1 \ replace cdf_X=. if ginisample~=1
twoway (line cdf_inc cdf_pop, sort) (line cdf_pop cdf_pop, by(year) lpattern(dash))
sort ginisample year faminc_equiv
gen darea=(cdf_inc+cdf_inc[_n-1])*(cdf_pop-cdf_pop[_n-1])/2 if year==year[_n-1] & ginisample==1
egen gini=sum(darea), by(year)
replace gini=1-2*gini
drop darea cdf* incshare popshare totpop totfaminc

gen wt10000=round(wt*10000,1)
ginidesc faminc_equiv [aw=wt10000] if ginisample==1, by(year)
drop wt10000
table year, c(m gini n gini)

* MLD
gen ln_faminc_equiv=ln(faminc_equiv)
xi: reg ln_faminc_equiv i.year if ginisample==1 [pw=wt]
predict meanlog if ginisample==1
xi: reg faminc_equiv i.year if ginisample==1 [pw=wt]
predict logmean if ginisample==1
replace logmean=ln(logmean)
gen mld=logmean-meanlog
la var mld "Mean log deviation: faminc_equiv"
la var logmean "mean[ln(faminc_equiv)]"
la var meanlog "log(mean(faminc_equiv)]"

* Make adjustments for series breaks.
gen married=MARITAL==1
for any a1 a2: gen X=0
replace a1=1 if (year==1972 & MARITAL==1) | (year==1980 & MARITAL==1 & FORM==3) | (year==1987 & MARITAL==1 & FORM==3)
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
gen wt_correction=wt*correction_factor
la var wt "Weight, adjusting for happiness series breaks"
table year [aw=wt], c(m hap1_hat_adj m hap2_hat_adj m hap3_hat_adj n hap1_hat_adj n happy)
tab year happy [aw=wt_correction], row nofreq

compress
save gss_micro, replace

use gss_micro, clear
gen n=1/wt if happy~=.
collapse (sum) n (mean) hap_hat hap1_hat hap2_hat hap3_hat hap1_hat_raw hap2_hat_raw hap3_hat_raw [pw=wt], by(year)
for X in num 1/3: rename hapX_hat haphat_X \ rename hapX_hat_raw hapraw_X
save gss_macro, replace
