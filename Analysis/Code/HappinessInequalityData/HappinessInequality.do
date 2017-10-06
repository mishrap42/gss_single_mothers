* You need to change the next line only
* The key input is the file "04697-001-Data.dta", which is the 1972-2006 GSS file.
cd "C:\Users\jwolfers\Documents\Justin@Wharton\Happiness and inequality\Data"
clear
set mem 700m
set more off
set maxvar 10000
* set printcolor gs1
* set copycolor gs1
do "GSS_clean.do"
use gss_micro , clear
cap log close
log using gss_micro.log, text replace

egen t_yr=tag(year) if happy~=.

*********************Figure 1***********************************
for X in any 1 2 3 \ Y in any vhap phap unhap \ Z in any "Not too happy" "Pretty happy" "Very happy": gen Y=hapX_hat_adj if year==2004 \ gen str13 Y_lab="Z" if year==2004
xi i.year
for X in any 1 2 3: gen iX=happy==X if happy~=. \ reg iX _I* [aw=wt] \ predict hapraw_X \ reg iX _I* [aw=wt_corr] \ predict haphat_X \ drop iX
gen yearm1=year
#delimit ;
twoway
	(scatter hapraw_1 year, msymbol(circle_hollow) mcolor(black))
	(connected haphat_1 year, msymbol(circle) mcolor(black) lcolor(black) lpattern(solid))
	(lfit haphat_1 year, lcolor(black) lpattern(longdash) lwidth(vvthin)) 
	(scatter vhap yearm1, msymbol(none) mlabel(vhap_lab) mlabcolor(black) mlabpos(12) mlabsize(medium))
	(scatter hapraw_2 year, msymbol(triangle_hollow) mcolor(black))
	(connected haphat_2 year, msymbol(triangle) mcolor(black) lcolor(black) lpattern(solid))
	(lfit haphat_2 year, lcolor(black) lpattern(longdash) lwidth(vvthin)) 
	(scatter phap yearm1, msymbol(none) mlabel(phap_lab) mlabcolor(black) mlabpos(6) mlabsize(medium))
	(scatter hapraw_3 year, msymbol(square_hollow) mcolor(black))
	(connected haphat_3 year, msymbol(square) mcolor(black) lcolor(black) lpattern(solid))
	(lfit haphat_3 year, lcolor(black) lpattern(longdash) lwidth(vvthin)) 
	(scatter unhap yearm1, msymbol(none) mlabel(unhap_lab) mlabcolor(black) mlabpos(6) mlabsize(medium))
	if t_yr==1
,
	title("Happiness Trends in the United States")
	xtitle("Year")
	xlabel(1972(4)2008)
	xmtick(1972(1)2008)
	legend(off)
	ytitle("Proportion")
	ylabel(0(.1).6, angle(horizontal) format(%9.2f))
	note("Hollow dots show uncorrected data; Solid dots correct for changes in question ordering.", ring(0) pos(6) )
	xsize(10) ysize(7.5)
	name(fig1_pub, replace)
;
#delimit cr
graph export fig1.eps, replace fontface(Times)
graph export fig1.tif, replace

#delimit ;
twoway
	(scatter hapraw_1 year, msymbol(circle_hollow) mcolor(cranberry))
	(connected haphat_1 year, msymbol(circle) mcolor(cranberry) lcolor(cranberry))
	(lfit haphat_1 year, lcolor(cranberry) lpattern(longdash) lwidth(vvthin)) 
	(scatter vhap yearm1, msymbol(none) mlabel(vhap_lab) mlabcolor(cranberry) mlabpos(12) mlabsize(medium))
	(scatter hapraw_2 year, msymbol(triangle_hollow) mcolor(blue))
	(connected haphat_2 year, msymbol(triangle) mcolor(blue) lcolor(blue))
	(lfit haphat_2 year, lcolor(blue) lpattern(longdash) lwidth(vvthin)) 
	(scatter phap yearm1, msymbol(none) mlabel(phap_lab) mlabcolor(blue) mlabpos(12) mlabsize(medium))
	(scatter hapraw_3 year, msymbol(square_hollow) mcolor(green))
	(connected haphat_3 year, msymbol(square) mcolor(green) lcolor(green))
	(lfit haphat_3 year, lcolor(green) lpattern(longdash) lwidth(vvthin)) 
	(scatter unhap yearm1, msymbol(none) mlabel(unhap_lab) mlabcolor(green) mlabpos(12) mlabsize(medium))
	if t_yr==1
,
	title("Happiness Trends in the United States")
	xtitle("Year")
	xlabel(1972(4)2008)
	xmtick(1972(1)2008)
	legend(off)
	ytitle("Proportion")
	ylabel(0(.1).6, angle(horizontal) format(%9.2f))
	note("Hollow dots show uncorrected data; Solid dots correct for changes in question ordering.", ring(0) pos(6) )
	xsize(10) ysize(7.5)
	name(fig1, replace)
;
#delimit cr

*********************Naive approach*****************************
* Figure 2: Naive graph
xi i.year, noomit
reg happy _I* [pw=wt_correct], noconst
predict mu_naive if happy~=.
gen hapsq=happy*happy
reg hapsq _I* [pw=wt_correct], noconst
predict mos if happy~=.
gen v_naive=mos-mu_naive^2
drop mos

#delimit ;
twoway
	(line mu_naive year, sort lcolor(black) lwidth(thin))
	(line v_naive year, sort lcolor(black) lwidth(thick) yaxis(2))
	if t_yr==1
,
	note("Simple cardinalization: Not too happy=1; Pretty happy=2; Very happy=3.", ring(0) pos(6))
	xtitle("")
	xlabel(1972(4)2008)
	xmtick(1972(1)2008)
	ytitle("Average level of happiness (1-3)")
	ylabel(2.18(0.02)2.30, angle(horizontal) format(%9.2f))
	ytitle("Variance of happiness", axis(2))
	ylabel(.34(0.02).46, axis(2) angle(horizontal) format(%9.2f))
	text(2.287 1990 "Average happiness" "(left axis)", color(black))
	text(2.254 2004 "Variance" "of happiness" "(right axis)", color(black))
	legend(off)
	xsize(10) ysize(7.5)
	name(fig2_pub, replace)
;
#delimit cr
graph export fig2.eps, replace fontface(Times)
graph export fig2.tif, replace


#delimit ;
twoway
	(line mu_naive year, sort lcolor(navy) lwidth(thin))
	(line v_naive year, sort lcolor(cranberry) lwidth(thick) yaxis(2))
	if t_yr==1
,
	title("Trends in the Distribution of U.S. Happiness")
	subtitle("Simple cardinalization: Not too happy=1; Pretty happy=2; Very happy=3.")
	xtitle("")
	xlabel(1972(4)2008)
	xmtick(1972(1)2008)
	ytitle("Average level of happiness (1-3)")
	ylabel(2.18(0.02)2.30, angle(horizontal) format(%9.2f))
	ytitle("Variance of happiness", axis(2))
	ylabel(.34(0.02).46, axis(2) angle(horizontal) format(%9.2f))
	text(2.287 1990 "Average happiness" "(left axis)", color(navy))
	text(2.254 2004 "Variance" "of happiness" "(right axis)", color(cranberry))
	legend(off)
	xsize(10) ysize(7.5)
	name(fig2, replace)
;
#delimit cr

**** Figure 3

gen nth=haphat_1
gen vh=haphat_3
egen tau=count(vh+nth) if t_yr==1
gen invnth_norm=invnorm(nth)
gen inv1mvh_norm=invnorm(1-vh)
gen invnth_logit=ln(nth/(1-nth)) *sqrt(3)/_pi
gen inv1mvh_logit=ln((1-vh)/vh) *sqrt(3)/_pi
gen invnth_uniform=nth*sqrt(12)-sqrt(3)
gen inv1mvh_uniform=(1-vh)*sqrt(12)-sqrt(3)

#delimit ;
for X in any norm logit uniform:
	egen num1_X=sum(invnth_X) if t_yr==1 \
	egen denom1_X=sum(inv1mvh_X-invnth_X) if t_yr==1 \
	egen denom2_X=sum((inv1mvh_X-invnth_X)^(-2)) if t_yr==1\ 
	gen phi=tau/(denom1*denom1*denom2) \
	gen c1_X=num1_X*sqrt(phi) \
	egen num2_X=sum(inv1mvh_X) if t_yr==1 \
	gen c2_X=num2_X*sqrt(phi) \
	gen mu_X=(c1_X*inv1mvh_X-c2_X*invnth_X)/(inv1mvh_X-invnth_X) \
	gen v_X=((c2_X-c1_X)/(inv1mvh_X-invnth_X))^2 \
	drop denom1_X denom2_X num1_X num2_X phi
;
#delimit cr

#delimit ;
twoway
	(line mu_norm year, sort lcolor(navy) lpattern(solid))
	(line mu_logit year, sort lwidth(thin) lcolor(green) lpattern(longdash))
	(line mu_uniform year, sort lcolor(cranberry) lpattern(dash))
/*	(line mn_simple year, sort yaxis(2)) */
	if t_yr==1
,
	title("Average Happiness", ring(0))
	xtitle("")
	xlabel(1972(4)2008)
	xmtick(1972(1)2008)
	ytitle("De-Meaned Level of Happiness" "Units: Cross-sectional SD(happiness)")
	ylabel(-0.4(0.2)0.2, angle(horizontal) format(%9.2f))
/*	ytitle("Simple mean", axis(2))
	ylabel(2.12(0.03)2.30, axis(2) angle(horizontal) format(%9.2f)) */
	legend(
		title("Assuming that the distribution of happiness is:", size(medium))
		order(1 "Normal" 2 "Logistic" 3 "Uniform" /* 4 "Discrete" "(right axis)" */)
		region(fcolor(none))
		rows(1)
		ring(0) pos(6)
	)
	xsize(10) ysize(7.5)
	name(mean, replace)
;
#delimit cr

#delimit ;
twoway
	(line v_norm year, sort lcolor(navy) lpattern(solid))
	(line v_logit year, sort lwidth(thin) lcolor(green) lpattern(longdash))
	(line v_uniform year, sort lcolor(cranberry) lpattern(dash))
/*	(line v_simple year, sort yaxis(2)) */
	if t_yr==1
,
	title("Happiness Inequality", ring(0))
	xtitle("")
	xlabel(1972(4)2008)
	xmtick(1972(1)2008)
	ytitle("Variance of happiness" "Relative to average level in sample")
	ylabel(0.8(0.2)1.4 1 " 1.00", angle(horizontal) format(%9.2f))
/*	ytitle("Simple Variance", axis(2))
	ylabel(.52(0.03).7, axis(2) angle(horizontal) format(%9.2f)) */
	legend(off)
	xsize(10) ysize(7.5)
	name(v, replace)
;
#delimit cr

#delimit ;
graph combine mean v, 
	title("Trends in the Distribution of U.S. Happiness")
	rows(2) 
	name(mean_v, replace)
;
#delimit cr


#delimit ;
twoway
	(line mu_norm year, sort lcolor(black) lpattern(solid))
	(line mu_logit year, sort lwidth(thin) lcolor(gs5) lpattern(longdash))
	(line mu_uniform year, sort lcolor(gs10) lpattern(dash))
/*	(line mn_simple year, sort yaxis(2)) */
	if t_yr==1
,
	title("Average Happiness")
	xtitle("")
	xlabel(1972(4)2008)
	xmtick(1972(1)2008)
	ytitle("De-Meaned Level of Happiness" "Units: Cross-sectional SD(happiness)")
	ylabel(-0.4(0.2)0.2, angle(horizontal) format(%9.2f))
/*	ytitle("Simple mean", axis(2))
	ylabel(2.12(0.03)2.30, axis(2) angle(horizontal) format(%9.2f)) */
	legend(
		title("Assuming that the distribution of happiness is:", size(medium))
		order(1 "Normal" 2 "Logistic" 3 "Uniform" /* 4 "Discrete" "(right axis)" */)
		region(fcolor(none))
		rows(1)
		ring(0) pos(12)
	)
	xsize(10) ysize(7.5)
	name(mean_pub, replace)
;
#delimit cr

#delimit ;
twoway
	(line v_norm year, sort lcolor(black) lpattern(solid))
	(line v_logit year, sort lwidth(thin) lcolor(gs5) lpattern(longdash))
	(line v_uniform year, sort lcolor(gs10) lpattern(dash))
/*	(line v_simple year, sort yaxis(2)) */
	if t_yr==1
,
	title("Happiness Inequality")
	xtitle("")
	xlabel(1972(4)2008)
	xmtick(1972(1)2008)
	ytitle("Variance of happiness" "Relative to average level in sample")
	ylabel(0.8(0.2)1.4 1 " 1.00", angle(horizontal) format(%9.2f))
/*	ytitle("Simple Variance", axis(2))
	ylabel(.52(0.03).7, axis(2) angle(horizontal) format(%9.2f)) */
	legend(
		title("Assuming that the distribution of happiness is:", size(medium))
		order(1 "Normal" 2 "Logistic" 3 "Uniform" /* 4 "Discrete" "(right axis)" */)
		region(fcolor(none))
		rows(1)
		ring(0) pos(12)
	)
	xsize(10) ysize(7.5)
	name(v_pub, replace)
;
#delimit cr

#delimit ;
graph combine mean_pub v_pub, 
	rows(2) 
	name(mean_v_pub, replace)
	xsize(4.125) ysize(7.125)
;
#delimit cr


******************Simple ordered probit************************************
* Needs to be fixed so that it forces the mean variance to be one

cap program drop hetprobit_simple_3
program define hetprobit_simple_3  /*Implement the simple algorithm described in paper; Works only for 3-outcome variables */
	qui {
	tempvar `1'_1 `1'_2 `1'_3 `1'_1_h `1'_2_h `1'_3_h z1mf3 zf1 tau sum_z1mf3 sum_zf1 denom1 denom2 frac phi d1 d2
	for X in any mu_`2' v_`2' t_`2': cap drop X
	xi i.`2'
	foreach i of numlist 1/3 {
		gen ``1'_`i''=(`1'==`i') if `1'~=.
		reg ``1'_`i'' _I* [pw=wt_correct] 
		predict ``1'_`i'_h' if `1'~=. & `2'~=""
	}
	gen `z1mf3'=invnorm(1-``1'_3_h')
	gen `zf1'=invnorm(``1'_1_h')
	tab `2' if `1'~=.
	gen `tau'=r(r)
	egen t_`2'=tag(`2') if `1'~=.
	egen `sum_z1mf3'=sum(`z1mf3') if t_`2'==1
	egen `sum_zf1'=sum(`zf1') if t_`2'==1
	egen `denom1'=sum(`z1mf3'-`zf1') if t_`2'==1
	gen `frac'=(`z1mf3'-`zf1')^(-2)
	egen `denom2'=sum(`frac') if t_`2'==1
	gen `phi'=`tau'/(`denom1'*`denom1'*`denom2')
	gen `d1'=sqrt(`phi')*`sum_zf1'
	gen `d2'=sqrt(`phi')*`sum_z1mf3'
	gen mu_`2'=(`d1'*`z1mf3'-`d2'*`zf1')/(`z1mf3'-`zf1')
	gen v_`2'=((`d2'-`d1')/(`z1mf3'-`zf1'))^2
	summ `d1'
	local d1=r(mean)
	summ `d2'
	local d2=r(mean)
	summ `phi'
	local phi=r(mean)
	}
	di("Cutpoints are `d1' and `d2' and phi=`phi'")
	table `2' if t_`2'==1, c(m ``1'_1_h' m ``1'_2_h' m ``1'_3_h' m mu_`2' m v_`2') format(%9.3f)
	tomode mu_`2', by(`2') replace
	tomode v_`2', by(`2') replace
end

* Table 1
gen str4 yr=string(year)
hetprobit_simple_3 happy yr
egen n=count(wt_corr) if happy~=., by(year)
for X in varlist *_hat mu_yr v_yr: format X %9.3f
sort year
list year hap1_hat hap2_hat hap3_hat mu_yr v_yr n if t_yr==1, clean 


** Decade-by-decade analysis
gen decade=(int(year/10)*10)
gen str4 dec=string(decade)
egen mu_dec=mean(mu_yr) if t_yr==1, by(decade)
egen v_dec=mean(v_yr) if t_yr==1, by(decade)
for X in varlist mu_dec v_dec: tomode X, by(decade) replace
* hetprobit_simple_3 happy dec
table decade, c(m mu_dec m v_dec)

egen z=rank(_n), by(decade)
replace z=(z-1)/100-3
replace z=. if z>3
gen fz_decade=normalden(z, mu_dec, sqrt(v_dec))
#delimit ;
twoway 
	(line fz_decade z if decade==1970, sort lwidth(medium) lpattern(solid) lcolor(cranberry))
	(line fz_decade z if decade==1980, sort lwidth(thin) lpattern(dot) lcolor(navy) )
	(line fz_decade z if decade==1990, sort lwidth(thin) lpattern(dash) lcolor(green) )
	(line fz_decade z if decade==2000, sort lwidth(medium) lpattern(longdash) lcolor(black) )
,
	title("Distribution of Happiness, by Decade")
	xtitle("Happiness Level (z-score)")
	xlabel(-3(1)3)
	ytitle("PDF: Proportion of population at each level of happiness")
	ylabel(0(.1).4, angle(horizontal))
	text(.345 0 "1970s", color(cranberry) box fcolor(none) lwidth(medium) lpattern(solid) lcolor(cranberry))
	text(.36 .8 "1980s", color(navy) box fcolor(none) lwidth(thin) lpattern(dot) lcolor(navy))
	text(.41 +.52 "1990s", color(green) box fcolor(none) lwidth(thin) lpattern(dash) lcolor(green) )
	text(.33 1 "2000s", color(black) box fcolor(none) lwidth(medium) lpattern(longdash) lcolor(black))
	legend(off)
	xsize(10) ysize(7.5)
	name(fig4, replace)
;
#delimit cr

gen Fz_decade=normal((z-mu_dec)/sqrt(v_dec))
#delimit ;
twoway 
	(line Fz_decade z if decade==1970, sort lwidth(medium) lpattern(solid) lcolor(cranberry))
	(line Fz_decade z if decade==1980, sort lwidth(thin) lpattern(dot) lcolor(navy) )
	(line Fz_decade z if decade==1990, sort lwidth(thin) lpattern(dash) lcolor(green) )
	(line Fz_decade z if decade==2000, sort lwidth(medium) lpattern(longdash) lcolor(black) )
,
	title("Distribution of Happiness, by Decade")
	xtitle("Happiness Level (z-score)")
	xlabel(-3(1)3)
	ytitle("Cumulative Distribution Function" "(Proportion with happiness>x)")
	ylabel(0(.1)1, angle(horizontal))
	text(.92 2.3 "1970s", color(cranberry) box fcolor(none) lwidth(medium) lpattern(solid) lcolor(cranberry))
	text(.12 -.9 "1980s", color(navy) box fcolor(none) lwidth(thin) lpattern(dot) lcolor(navy))
	text(.05 -1 "1990s", color(green) box fcolor(none) lwidth(thin) lpattern(dash) lcolor(green) )
	text(.92 .9 "2000s", color(black) box fcolor(none) lwidth(medium) lpattern(longdash) lcolor(black))
	legend(off)
	xsize(10) ysize(7.5)
	name(fig4b, replace)
;
#delimit cr

#delimit ;
twoway 
	(line fz_decade z if decade==1970, sort lwidth(medium) lpattern(solid) lcolor(cranberry))
	(line fz_decade z if decade==1980, sort lwidth(thin) lpattern(dot) lcolor(navy) )
	(line fz_decade z if decade==1990, sort lwidth(thin) lpattern(dash) lcolor(green) )
	(line fz_decade z if decade==2000, sort lwidth(medium) lpattern(longdash) lcolor(black) )
,
	title("PDF", ring(0))
	xtitle("")
	xlabel(-3(1)3)
	ytitle("PDF: Proportion of population at each level of happiness")
	ylabel(0(.1).5, angle(horizontal))
	text(.34 0 "1970s", color(cranberry) box fcolor(none) lwidth(medium) lpattern(solid) lcolor(cranberry))
	text(.36 .8 "1980s", color(navy) box fcolor(none) lwidth(thin) lpattern(dot) lcolor(navy))
	text(.43 0 "1990s", color(green) box fcolor(none) lwidth(thin) lpattern(dash) lcolor(green) )
	text(.33 1 "2000s", color(black) box fcolor(none) lwidth(medium) lpattern(longdash) lcolor(black))
	legend(off)
	xsize(10) ysize(7.5)
	name(fig4pdf, replace)
;
twoway 
	(line Fz_decade z if decade==1970, sort lwidth(medium) lpattern(solid) lcolor(cranberry))
	(line Fz_decade z if decade==1980, sort lwidth(thin) lpattern(dot) lcolor(navy) )
	(line Fz_decade z if decade==1990, sort lwidth(thin) lpattern(dash) lcolor(green) )
	(line Fz_decade z if decade==2000, sort lwidth(medium) lpattern(longdash) lcolor(black) )
,
	title("CDF", ring(0))
	xtitle("")
	xlabel(-3(1)3)
	ytitle("CDF: Proportion with happiness>x")
	ylabel(0(.2)1, angle(horizontal))
	text(.92 2.3 "1970s", color(cranberry) box fcolor(none) lwidth(medium) lpattern(solid) lcolor(cranberry))
	text(.12 -.75 "1980s", color(navy) box fcolor(none) lwidth(thin) lpattern(dot) lcolor(navy))
	text(.05 -.9 "1990s", color(green) box fcolor(none) lwidth(thin) lpattern(dash) lcolor(green) )
	text(.92 .8 "2000s", color(black) box fcolor(none) lwidth(medium) lpattern(longdash) lcolor(black))
	legend(off)
	xsize(10) ysize(7.5)
	name(fig4cdf, replace)
;
graph combine
	fig4pdf fig4cdf
,
	title("Distribution of Happiness, by Decade")
	b1title("Happiness Levels (z-scores)")
	imargin(tiny)
	xsize(10) ysize(7.5)
	name(fig4_combined, replace)
;
#delimit cr


#delimit ;
twoway 
	(line fz_decade z if decade==1970, sort lwidth(medium) lpattern(solid) lcolor(black))
	(line fz_decade z if decade==1980, sort lwidth(thin) lpattern(dot) lcolor(gs5) )
	(line fz_decade z if decade==1990, sort lwidth(thin) lpattern(dash) lcolor(gs10) )
	(line fz_decade z if decade==2000, sort lwidth(medium) lpattern(longdash) lcolor(black) )
	if abs(z)<2.5
,
	title("PDF", ring(0))
	xtitle("")
	xlabel(-2(1)2)
	ytitle("PDF: Proportion of population at each level of happiness")
	ylabel(0(.1).5, angle(horizontal))
	text(.35 0 "1970s", color(black) box fcolor(none) lwidth(medium) lpattern(solid) lcolor(black))
	text(.385 0 "1980s", color(gs5) box fcolor(none) lwidth(thin) lpattern(dot) lcolor(gs5))
	text(.435 0 "1990s", color(gs10) box fcolor(none) lwidth(thin) lpattern(dash) lcolor(gs10) )
	text(.355 .75 "2000s", color(black) box fcolor(none) lwidth(medium) lpattern(longdash) lcolor(black))
	legend(off)
	xsize(10) ysize(7.5)
	name(fig4pdf_pub, replace)
;
#delimit ;
twoway 
	(line Fz_decade z if decade==1970, sort lwidth(medium) lpattern(solid) lcolor(black))
	(line Fz_decade z if decade==1980, sort lwidth(thin) lpattern(dot) lcolor(gs5) )
	(line Fz_decade z if decade==1990, sort lwidth(thin) lpattern(dash) lcolor(gs10) )
	(line Fz_decade z if decade==2000, sort lwidth(medium) lpattern(longdash) lcolor(black) )
	if(abs(z)<2.5)
,
	title("CDF", ring(0))
	xtitle("")
	xlabel(-2(1)2)
	ytitle("CDF: Proportion with happiness>x")
	ylabel(0(.2)1, angle(horizontal))
	text(.82 1.4 "1970s", color(black) box fcolor(none) lwidth(medium) lpattern(solid) lcolor(black))
	text(.3 -.25 "1980s", color(gs5) box fcolor(none) lwidth(thin) lpattern(dot) lcolor(gs5))
	text(.05 -1.1 "1990s", color(gs10) box fcolor(none) lwidth(thin) lpattern(dash) lcolor(gs10) )
	text(.7 .15 "2000s", color(black) box fcolor(none) lwidth(medium) lpattern(longdash) lcolor(black))
	legend(off)
	xsize(10) ysize(7.5)
	name(fig4cdf_pub, replace)
;
graph combine
	fig4pdf_pub fig4cdf_pub
,
	b1title("Happiness Levels (z-scores)")
	imargin(tiny)
	xsize(7.125) ysize(4.125)
	name(fig4_combined, replace)
;
#delimit cr


for X in num 25 75: gen zX=mu_dec+invnorm(X/100)*sqrt(v_dec)
table decade, c(m z25 m z75 m mu_dec)

levelsof decade, local(decades)
foreach q of num 25 50 75 {
	qui foreach d of local decades {
		summ mu_dec if decade==`d'
		local mu_`q'_`d'=r(mean)
		summ v_dec if decade==`d'
		local sd_`q'_`d'=sqrt(r(mean))
		local z_`q'_`d'=`mu_`q'_`d''+invnorm(`q'/100)*`sd_`q'_`d''
	}
	local diff_`q'=`z_`q'_2000'-`z_`q'_1970'
	di("At the `q'th percentile happiness rose from `z_`q'_1970' in 1970 to `z_`q'_2000' in 2000, or by `diff_`q''")
	local mudiff_`q'=`mu_`q'_2000'-`mu_`q'_1970'
	local sddiff_`q'=invnorm(`q'/100)*(`sd_`q'_2000'-`sd_`q'_1970')
	di("Of this rise, `mudiff_`q'' was due to the mean shift; `sddiff_`q'' was due to the dispersion shift")
}
local zcrossover=(`mu_50_2000'-`mu_50_1970')/(`sd_50_1970'-`sd_50_2000')
di("Happiness crossover: z=`zcrossover'")
local pcrossover_1970=normal((`zcrossover'-`mu_50_1970')/`sd_50_1970')
local pcrossover_2000=normal((`zcrossover'-`mu_50_2000')/`sd_50_2000')
di("Which occurs at the `pcrossover_1970' or `pcrossover_2000' percentile of the distribution")


* Figure 5: Annual Changes since 1972
hetprobit_simple_3 happy yr
for X in num 5 25 50 75 95: gen z_X=mu_yr +invnorm(1-X/100)*sqrt(v_yr)  \  summ z_X if year==1972 \ replace z_X=z_X-r(mean) 

#delimit ;
twoway
	(line z_95 year, sort lcolor(navy) lpattern(longdash))
	(line z_75 year, sort lcolor(green)  lpattern(dash))
	(line z_50 year, sort lcolor(black) lpattern(solid))
	(line z_25 year, sort lcolor(orange)  lpattern(dot))
	(line z_5 year, sort lcolor(brown) lpattern(tight_dot))
	if t_yr==1
,
	title("Evolution of the U.S. Happiness Distribution")
	subtitle("Change Since 1972")
	xlabel(1972(4)2008)
	xmtick(1972(1)2010)
	xtitle("")	
	ytitle("Change in happiness level since 1972" "Scale: z-scores")
	ylabel(-.4(.2).4, angle(horizontal))
	legend(off)
	text(.28 2007 "5th percentile" "=Unhappiest 5%", color(navy))
	text(.09 2007 "25th percentile", color(green))
	text(-.04 2008 "Median of" "happiness" "distribution", color(black))
	text(-.18 2007 "75th percentile", color(orange))
	text(-.36 2007 "95th percentile" "=Happiest 5%", color(brown))
	xsize(10) ysize(7.5)
	name(fig5, replace)
;
#delimit cr


#delimit ;
twoway
	(line z_95 year, sort lcolor(black) lpattern(longdash))
	(line z_75 year, sort lcolor(gs5)  lpattern(dash))
	(line z_50 year, sort lcolor(gs10) lpattern(solid))
	(line z_25 year, sort lcolor(gs5)  lpattern(dot))
	(line z_5 year, sort lcolor(black) lpattern(tight_dot))
	if t_yr==1
,
	subtitle("Change Since 1972", ring(0) pos(1))
	xlabel(1972(4)2008)
	xmtick(1972(1)2010)
	xtitle("")	
	ytitle("Change in happiness level since 1972" "Scale: z-scores")
	ylabel(-.4(.2).4, angle(horizontal))
	legend(off)
	text(.28 2006.8 "5th percentile" "=Unhappiest 5%", color(black))
	text(.09 2007 "25th percentile", color(gs5))
	text(-.04 2008 "Median of" "happiness" "distribution", color(gs10))
	text(-.18 2007 "75th percentile", color(gs5))
	text(-.36 2007 "95th percentile" "=Happiest 5%", color(black))
	xsize(10) ysize(7.5)
	name(fig5_pub, replace)
;
#delimit cr


list year z_5 z_25 z_50 z_75 z_95 if t_yr==1

summ z_* if year==2006
gen lninc=ln(faminc)
oprobit happy lninc

* Table 2: Estimates by demographic group
drop t_yr
global xvars _I*
gen d1970=year>=1970 & year<1980
gen d1980=year>=1980 & year<1990
gen d1990=year>=1990 & year<2000
gen d2000=year>=2000 & year<2010
gen time100=(year-1972)/100
gen post=year>=1989
gen postXtime100=post*(year-1989)/100

gen one=1
reg one one, nocons
outreg one using "gss_inequality_decades", se coefastr 3aster ctitle("one") replace
outreg one using "gss_inequality_trends", se coefastr 3aster ctitle("one") replace
outreg one using "gss_inequality_chow", se coefastr 3aster ctitle("one") replace

gen str14 all="Whole_Sample"
gen str10 mar="1Married" if MARITAL==1
replace mar="2Unmarried" if MARITAL>1 
gen str6 sex="1Men" if SEX==1
replace sex="2Women" if SEX==2
gen str16 ed="4LT_high_school" if EDUC>=0 & EDUC<=11
replace ed="3High_school_grad" if EDUC==12
replace ed="2Some_college" if EDUC>12 & EDUC<=15
replace ed="1College_grad" if EDUC>=16
gen str14 agegrp="1Age_18_to_34" if AGE>=18 & AGE<=34
replace agegrp="2Age_35_to_49" if AGE >=35 & AGE<50
replace agegrp="3Age_50_plus" if AGE>=50
gen str10 race="1White" if RACE==1
replace race="2Black" if RACE==2
replace race="3Other" if RACE==3
gen str10 white="1White" if RACE==1
replace white="2Non_White" if RACE==2 | RACE==3
gen str10 reg="1Northeast" if REGION==1 | REGION==2
replace reg="2Midwest" if REGION==3 | REGION==4
replace reg="3South" if REGION==5 | REGION==6 | REGION==7
replace reg="4West" if REGION==8 | REGION==9
gen str10 imm2="1US" if BORN==1
replace imm2="2Immigrant" if BORN==2
gen str10 imm3="1US" if BORN==1
replace imm3="2Immigrant" if BORN==2
replace imm3="3Unknown" if BORN>2 & year<1977
for X in num 2 3: for Y in num 1 2 4 6 8: replace immX="2Immigrant" if PARBORN==Y 

char ed[omit] "3High_school_grad"
char sex[omit] "1Men"
char white[omit] "1White"
char agegrp[omit] "2Age_35_to_49"
char mar[omit] "1Married"
char reg[omit] "1Northeast"
char imm2[omit] "1US"
char imm3[omit] "1US"

global groups "ed sex white mar agegrp reg"
global xigroups ""
foreach j of global groups {
	global xigroups "$xigroups xi i.`j'"
}

* Generate annual time series for each block of table two.
foreach v of varlist all $groups  {
	gen str30 `v'yr=`v'+"_"+string(year) if `v'~=""
	hetprobit_simple_3 happy `v'yr
}

foreach v of any all $groups { /*Panels A, B and C*/
	levelsof `v', local(vlist)
	foreach vl of local vlist {
		egen r=rank(year) if `v'=="`vl'" & t_`v'yr==1 
		tsset r
		reg v_`v' d1970-d2000 if `v'=="`vl'" & t_`v'yr==1, noconst	
		outreg d1970 d1980 d1990 d2000 using "gss_inequality_decades", ctitle("v_`v'_`vl'") nose append
		newey v_`v' time100 if `v'=="`vl'" & t_`v'yr==1, lag(1)
		outreg time100 using "gss_inequality_trends", se coefastr 3aster ctitle("v_`v'_`vl'") append
		newey v_`v' time100 postXtime100 if `v'=="`vl'" & t_`v'yr==1, lag(1)
		lincom time100+postXtime100
		local pval= min(2*ttail(r(df),r(estimate)/r(se)),2*(1-(ttail(r(df),r(estimate)/r(se)))))
		outreg time100 postXtime100 using "gss_inequality_chow", se coefastr 3aster ctitle("v_`v'_`vl'") addstat("Diff", r(estimate), "SE(diff)",r(se),"p-val",`pval') adec(3) append
		drop r
	}
}
foreach v of any all $groups { /*Panels D, E and F*/
	levelsof `v', local(vlist)
	foreach vl of local vlist {
		egen r=rank(year) if `v'=="`vl'" & t_`v'yr==1 
		tsset r
		reg mu_`v' d1970-d2000 if `v'=="`vl'" & t_`v'yr==1 , noconst	
		outreg d1970 d1980 d1990 d2000 using "gss_inequality_decades", ctitle("mu_`v'_`vl'") nose append
		newey mu_`v' time100 if `v'=="`vl'" & t_`v'yr==1, lag(1)
		outreg time100 using "gss_inequality_trends", se coefastr 3aster ctitle("mu_`v'_`vl'") append
		newey mu_`v' time100 postXtime100 if `v'=="`vl'" & t_`v'yr==1, lag(1)
		lincom time100+postXtime100
		local pval= min(2*ttail(r(df),r(estimate)/r(se)),2*(1-(ttail(r(df),r(estimate)/r(se)))))
		outreg time100 postXtime100 using "gss_inequality_chow", se coefastr 3aster ctitle("mu_`v'_`vl'") addstat("Diff", r(estimate), "SE(diff)",r(se), "p-val",`pval') adec(3) append
		drop r
	}
}

* How pervasive is the declining trend?
set matsize 2000
egen g=group(sex agegrp ed)
gen str200 g_char=sex+ed+agegrp
compress g_char
gen str30 gyr=string(g)+string(year) if g~=.
hetprobit_simple_3 happy gyr
gen vg_beta=.
gen vg_se=.
gen g_n=.
summ g
local gs=r(max)
foreach i of numlist 1/`gs' {
	reg v_g time100 if g==`i' & t_gyr==1
	replace vg_beta=_b[time100] if g==`i' & t_gyr==1
	replace vg_se=_se[time100] if g==`i' & t_gyr==1
	replace g_n=e(N) if g==`i' & t_gyr==1
}
table g_char if t_gyr==1, c(m vg_beta m vg_se)
egen tag_g=tag(g) if t_gyr==1
sort vg_beta
list g_char vg_beta vg_se g_n if tag_g==1

summ g
local gs=r(max)
local pos=0
foreach i of numlist 1/`gs' {
	tab g_char if g==`i'
	gen gyear=string(year) if g==`i'
	hetprobit_simple_3 happy gyear
	reg v_gyear year if g==`i' & t_gyear==1
	if _b[year]>0 {
		local pos=`pos'+1
	}
	drop gyear
}
di("`pos'")

save gss_temp, replace

*************************** Full Regression *****************************
use gss_temp, clear
global groups "ed sex white mar agegrp reg"
global xigroups ""
foreach j of global groups {
	global xigroups "$xigroups xi i.`j'"
}

cap program drop hetprobit_max 
program define hetprobit_max
	version 6
	args lnf theta v $cuts
	local c0=-999999
	local c$J=999999
	foreach j of numlist 1/$J {
		local jm1=`j'-1
		quietly replace `lnf'=ln(normprob((`theta'-`c`jm1'')/(sqrt(`v')))-normprob((`theta'-`c`j'')/sqrt(`v'))) if $ML_y1==`j'
	}
	qui summ `theta' [w=wt_correct]
	local mn=r(mean)
	qui summ `v' [w=wt_correct]
	local vd=r(mean)
	quietly replace `lnf'=`lnf'-(`mn'-0)^2-((`vd')-1)^2 /*This is the normalization*/
end

cap program drop hetprobit
program define hetprobit
	version 9
	* Clean up LHS
	cap drop y
	egen y=group(`1')
	qui summ y
	local Jminus1=r(max)-1
	global J=r(max)
	foreach j of numlist 1/`Jminus1' {
		local cutpoints "`cutpoints' /c`j'"
		local cuts "`cuts' c`j'"
		local initcut `initcut' `j'
	}
	global cuts "`cuts'"
	* Clean up RHS: No collinearity
	qui reg `1' $xvars, noconst
	global xvars_final=""
	foreach v of varlist $xvars {
		cap local a=_b[`v']
		if `a'!=0 {
			global xvars_final "$xvars_final `v'"
		}
		else {
			di("Dropping `v': Perfectly collinear")
			drop `v'
			}
	}
di("	ml model lf hetprobit_max (y=$xvars_final, noconst) (y=$xvars_final, noconst) `cutpoints' [pw=wt_correct]")
	ml model lf hetprobit_max (y=$xvars_final, noconst) (y=$xvars_final, noconst) `cutpoints' [pw=wt_correct]
	foreach v of varlist $xvars_final {
		local initmu `initmu' 0
		local initv `initv' 1
	}
	ml init `initmu' `initv' `initcut', copy
	ml max, difficult
	qui for X in any mu v: for Y in any coeff se: cap drop X_Y
	qui for X in any mu v: for Y in any coeff se: gen X_Y=.
	qui foreach v of varlist $xvars_final {
		replace mu_coeff=[eq1]_b[`v'] if `v'==1
		replace mu_se=[eq1]_se[`v'] if `v'==1
		replace v_coeff=[eq2]_b[`v'] if `v'==1
		replace v_se=[eq2]_se[`v'] if `v'==1
	}
end

cap drop _I*
$xigroups
cap gen _Iall_1=1
cap drop xy*
levelsof year, local(years)

foreach v of varlist _I* {
	foreach y of local years {
		qui gen xy`y'`v'=(year==`y') & `v'==1 if year~=. & `v'~=. & happy~=.
	}
}
global xvars xy*
set matsize 2000
hetprobit happy $xvars
predict mu_year_base if happy~=., equation(eq1)
predict v_year_base if happy~=., equation(eq2)
gen hapsample=1 if happy~=. & mu_year_base~=. & v_year_base~=.

foreach v of any all $groups { /* Grab raw coefficients */
	for ! in any muraw_y_`v' vraw_y_`v': gen !=0 if hapsample==1
	foreach w of varlist xy????_I`v'* {
		replace muraw_y_`v'=[eq1]_b[`w'] if `w'==1 & hapsample==1
		replace vraw_y_`v'=[eq2]_b[`w'] if `w'==1 & hapsample==1
	}
}

levelsof year, local(years) /* Figure out happiness of average person */
qui foreach v of varlist _I* {
	summ `v' if hapsample==1 [aw=wt]
	foreach y of local years {
		gen av_xy`y'`v'=r(mean)*(year==`y') if xy`y'`v'~=.
		gen old_xy`y'`v'=xy`y'`v'
		replace xy`y'`v'=av_xy`y'`v' if xy`y'`v'~=. 
	}
}
predict mu_ave if hapsample==1, equation(eq1) /* Note that this is unaffected by compositional change */
predict v_ave if hapsample==1, equation(eq2)

foreach v of any all $groups { /* Figure out happiness for nearly-average person*/
	foreach w of varlist xy????_I`v'* {
		replace `w'=old_`w'
	}
	predict mu_`v' if hapsample==1, equation(eq1)
	predict v_`v' if hapsample==1, equation(eq2)
	foreach w of varlist xy????_I`v'* {
		replace `w'=av_`w'
	}
}

qui for ! in varlist xy*: replace !=old_!
drop old_xy* av_xy*

drop xy* 
drop _I*

* How pervasive is this trend?
/*set matsize 2000
egen g=group(sex white agegrp ed)
gen str200 g_char=sex+white+ed+agegrp
compress g_char
xi i.g_char, noomit
cap drop xy*
foreach v of varlist _I* {
	qui gen xxt`v'=year*`v' if year~=. & `v'~=. & happy~=.
	qui gen xxc`v'=`v' if year~=. & `v'~=. & happy~=.
}
global xvars xx*
set matsize 2000
hetprobit happy $xvars
*/


/*
foreach v of any $groups all {
	xi i.`v'*i.year, noomit
	drop _Iyear_????
	drop _I`v'_?
	global xvars _I*
	hetprobit happy $xvars
	predict musimple_`v' if happy~=., equation(eq1)
	predict vsimple_`v' if happy~=., equation(eq2)
}
drop _I*
*/


** Repeat all of the above, by decade
$xigroups
cap gen _Iall_1=1
cap drop xy*
levelsof decade, local(decades)

foreach v of varlist _I* {
	foreach d of local decades {
		qui gen xy`d'`v'=(decade==`d') & `v'==1 if decade~=. & `v'~=. & happy~=.
	}
}
global xvars xy*
set matsize 2000
hetprobit happy $xvars
predict mu_dec_base if happy~=., equation(eq1)
predict v_dec_base if happy~=., equation(eq2)
gen hapsample_d=1 if happy~=. & mu_dec_base~=. & v_dec_base~=.

foreach v of any all $groups {
	for ! in any muraw_d_`v' museraw_d_`v': gen !=0 if hapsample_d==1
	for ! in any  vraw_d_`v' vseraw_d_`v': gen !=0 if hapsample_d==1
	foreach w of varlist xy????_I`v'* {
		replace muraw_d_`v'=[eq1]_b[`w'] if `w'==1 & hapsample_d==1
		replace museraw_d_`v'=[eq1]_se[`w'] if `w'==1 & hapsample_d==1
		replace vraw_d_`v'=[eq2]_b[`w'] if `w'==1 & hapsample_d==1
		replace vseraw_d_`v'=_se[`w'] if `w'==1 & hapsample_d==1
	}
}
drop xy* 
drop _Iall

foreach v of any all $groups {
	table `v' decade , c(m muraw_d_`v' m museraw_d_`v' m vraw_d_`v' m vseraw_d_`v') format(%9.3f)
	tab `v' decade [aw=wt] if hapsample_d==1, col nofreq
}


foreach v of any $groups all {
	xi i.`v'*i.decade, noomit
	drop _Idecade_????
	drop _I`v'_?
	global xvars _I*
	hetprobit happy $xvars
	predict musimple_d_`v' if happy~=., equation(eq1)
	predict vsimple_d_`v' if happy~=., equation(eq2)
}
drop _I*

foreach v of any all $groups {
	table `v' decade , c(m musimple_d_`v' m vsimple_d_`v') format(%9.3f)
}

for X in varlist all $groups: tab X decade [aw=wt] if hapsample_d==1, col nofreq 
for X in varlist all $groups: tab X [aw=wt] if hapsample_d==1


save gss_results, replace


use gss_results, clear
* Create a graph that summarizes all of the results
* Between-group differences

#delimit ;
twoway
	(line mu_ed year if ed=="1College_grad", sort lcolor(black) lwidth(thick) yaxis(1))
	(line mu_ed year if ed=="2Some_college", sort lcolor(navy) lwidth(medthick))
	(line mu_ed year if ed=="3High_school_grad", sort lcolor(green) lwidth(medium) yaxis(1))
	(line mu_ed year if ed=="4LT_high_school", sort lcolor(cranberry) lwidth(vthin))
	if t_edyr==1
,
	xtitle("")
	xlabel(none)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(-.75(.25).25, format(%9.2f) angle(horizontal))
	legend(title("Education") 
		order(1 "College grads" 2 "Some college" 3 "High school" 4 "<High school")
 		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(6) ring(0) rows(2))
	xsize(10) ysize(7.5)
	name(mu_ed, replace)
;
#delimit cr

#delimit ;
twoway
	(line mu_sex year if sex=="2Women", sort lcolor(magenta))
	(line mu_sex year if sex=="1Men", sort lcolor(blue) lwidth(thick))
	if t_sexyr==1
,
	xtitle("")
	xlabel(none)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(-.75(.25).25, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Gender")
		order(2 "Men" 1 "Women") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(6) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(mu_sex, replace)
;
#delimit cr

#delimit ;
twoway
	(line mu_white year if white=="1White", sort lcolor(green))
	(line mu_white year if white=="2Non_White", sort lcolor(black) lwidth(medthick))
	if t_whiteyr==1
,
	xtitle("")
	xlabel(none)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(-.75(.25).25, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Race")
			order(1 "White" 2 "Non-white")
			region(fcolor(none) )
			symxsize(*.5) colgap(*.2) keygap(*.1)
			pos(6) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(mu_white, replace)
;
#delimit cr

#delimit ;
twoway
	(line mu_agegrp year if agegrp=="1Age_18_to_34", sort lcolor(ltblue) lwidth(thick))
	(line mu_agegrp year if agegrp=="2Age_35_to_49", sort lcolor(olive) lwidth(medium))
	(line mu_agegrp year if agegrp=="3Age_50_plus", sort lcolor(lavender) lwidth(vthin))
	if t_agegrpyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(-.75(.25).25, format(%9.2f) angle(horizontal) )
	legend(title("Age Group")
		order(1 "18-34 yrs" 2 "35-49 yrs" 3 "50+ yrs") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(6) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(mu_age, replace)
;
#delimit cr

#delimit ;
twoway
	(line mu_mar year if mar=="1Married", sort lcolor(purple))
	(line mu_mar year if mar=="2Unmarried", sort lcolor(orange) lwidth(medthick))
	if t_maryr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(-.75(.25).25, format(%9.2f) angle(horizontal) nolabel)
	legend(
		title("Marital Status")
		order(1 "Married" 2 "Unmarried") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		region(fcolor(none))
		pos(6) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(mu_mar, replace)
;
#delimit cr

/*
egen t_imm3yr=tag(year imm3) if happy~=.
#delimit ;
twoway
	(line mu_imm3 year if imm3=="1US", sort lcolor(gray) lwidth(thick))
	(line mu_imm3 year if imm3=="2Immigrant", sort lcolor(navy) lwidth(thin))
	(line mu_imm3 year if imm3 =="3Unknown", sort lcolor(green) lwidth(thin) lpattern(dash))
	if t_imm3yr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(-.75(.25).25, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Nativity")
		order(1 "US born" 2 "Immigrant" 3 "Unknown") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(6) ring(0) rows(2))
	xsize(10) ysize(7.5)
	name(mu_imm3, replace)
;
#delimit cr
*/ 

#delimit ;
twoway
	(line mu_reg year if reg=="1Northeast", sort lcolor(gray) lwidth(thick))
	(line mu_reg year if reg=="2Midwest", sort lcolor(navy) lwidth(medthick))
	(line mu_reg year if reg=="3South", sort lcolor(green) lwidth(medium))
	(line mu_reg year if reg=="4West", sort lcolor(brown) lwidth(vthin))
	if t_regyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(-.75(.25).25, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Region")
		order(1 "Northeast" 2 "Midwest" 3 "South" 4 "West") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(6) ring(0) rows(2))
	xsize(10) ysize(7.5)
	name(mu_reg, replace)
;
#delimit cr


#delimit ;
graph combine
	mu_ed mu_sex mu_white
	mu_age mu_mar mu_reg /* mu_imm3 */
,
	title("Trends in Average Levels of Happiness Between Groups")
	rows(2)
	imargin(vsmall)
	l2title("Average Happiness Levels")
	l1title("Estimated as Average Happiness = Group*Year Fixed Effects", size(medsmall))
	ycommon
	xsize(10) ysize(7.5)
	name(mu, replace)
;




*** Dispersion
* Between-group differences

#delimit ;
twoway
	(line v_ed year if ed=="1College_grad", sort lcolor(black) lwidth(thick) yaxis(1))
	(line v_ed year if ed=="2Some_college", sort lcolor(navy) lwidth(medthick))
	(line v_ed year if ed=="3High_school_grad", sort lcolor(green) lwidth(medium) yaxis(1))
	(line v_ed year if ed=="4LT_high_school", sort lcolor(cranberry) lwidth(vthin))
	if t_edyr==1
,
	xtitle("")
	xlabel(none)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.6(.2)1.8, format(%9.2f) angle(horizontal))
	legend(title("Education") 
		order(1 "College grads" 2 "Some college" 3 "High school" 4 "<High school")
 		symxsize(*.5) colgap(*.2) keygap(*.1)
		region(fcolor(none))
		pos(12) ring(0) rows(2))
	xsize(10) ysize(7.5)
	name(v_ed, replace)
;
#delimit cr

#delimit ;
twoway
	(line v_sex year if sex=="2Women", sort lcolor(magenta))
	(line v_sex year if sex=="1Men", sort lcolor(blue) lwidth(thick))
	if t_sexyr==1
,
	xtitle("")
	xlabel(none)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.6(.2)1.8, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Gender")
		order(2 "Men" 1 "Women") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		region(fcolor(none))
		pos(12) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(v_sex, replace)
;
#delimit cr

#delimit ;
twoway
	(line v_white year if white=="1White", sort lcolor(green))
	(line v_white year if white=="2Non_White", sort lcolor(black) lwidth(medthick))
	if t_whiteyr==1
,
	xtitle("")
	xlabel(none)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.6(.2)1.8, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Race")
			order(1 "White" 2 "Non-white")
			region(fcolor(none) )
			symxsize(*.5) colgap(*.2) keygap(*.1)
			pos(12) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(v_white, replace)
;
#delimit cr

#delimit ;
twoway
	(line v_agegrp year if agegrp=="1Age_18_to_34", sort lcolor(ltblue) lwidth(thick))
	(line v_agegrp year if agegrp=="2Age_35_to_49", sort lcolor(olive) lwidth(medium))
	(line v_agegrp year if agegrp=="3Age_50_plus", sort lcolor(lavender) lwidth(vthin))
	if t_agegrpyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.6(.2)1.8, format(%9.2f) angle(horizontal) )
	legend(title("Age Group")
		order(1 "18-34 yrs" 2 "35-49 yrs" 3 "50+ yrs") 
		region(fcolor(none))
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(12) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(v_age, replace)
;
#delimit cr

#delimit ;
twoway
	(line v_mar year if mar=="1Married", sort lcolor(purple))
	(line v_mar year if mar=="2Unmarried", sort lcolor(orange) lwidth(medthick))
	if t_maryr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.6(.2)1.8, format(%9.2f) angle(horizontal) nolabel)
	legend(
		title("Marital Status")
		order(1 "Married" 2 "Unmarried") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		region(fcolor(none))
		pos(12) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(v_mar, replace)
;
#delimit cr

/*
#delimit ;
twoway
	(line v_imm3 year if imm3 =="1US", sort lcolor(gray) lwidth(thick))
	(line v_imm3 year if imm3 =="2Immigrant", sort lcolor(navy) lwidth(thin))
	(line v_imm3 year if imm3 =="3Unknown", sort lcolor(green) lwidth(thin) lpattern(dash))
	if t_imm3yr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.6(.2)1.8, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Nativity")
		order(1 "US born" 2 "Immigrant" 3 "Unknown") 
		region(fcolor(none))
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(12) ring(0) rows(2))
	xsize(10) ysize(7.5)
	name(v_imm3, replace)
;
#delimit cr
*/

#delimit ;
twoway
	(line v_reg year if reg=="1Northeast", sort lcolor(gray) lwidth(thick))
	(line v_reg year if reg=="2Midwest", sort lcolor(navy) lwidth(medthick))
	(line v_reg year if reg=="3South", sort lcolor(green) lwidth(medium))
	(line v_reg year if reg=="4West", sort lcolor(brown) lwidth(vthin))
	if t_regyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.6(.2)1.8, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Region")
		order(1 "Northeast" 2 "Midwest" 3 "South" 4 "West") 
		region(fcolor(none))
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(12) ring(0) rows(2))
	xsize(10) ysize(7.5)
	name(v_reg, replace)
;
#delimit cr


#delimit ;
graph combine
	v_ed v_sex v_white
	v_age v_mar v_reg /*v_imm3*/
,
	title("Trends in Happiness Inequality Within Groups")
	rows(2)
	imargin(vsmall)
	l2title("Happiness Inequality")
	l1title("Estimated as Var Happiness = Group*Year Fixed Effects", size(medsmall))
	xsize(10) ysize(7.5)
	name(v, replace)
;
#delimit cr


*** Sample proportions

xi i.year
foreach v of global groups {
	gen f_`v'=.
	levelsof `v', local(values)
	foreach vl of local values {
		gen f=`v'=="`vl'" if hapsample==1
		reg f _I* [aw=wt]
		predict hat if hapsample==1 & f~=.
		replace f_`v'=hat if `v'=="`vl'"
		drop f hat
	}
}

#delimit ;
twoway
	(line f_ed year if ed=="1College_grad", sort lcolor(black) lwidth(thick) yaxis(1))
	(line f_ed year if ed=="2Some_college", sort lcolor(navy) lwidth(medthick))
	(line f_ed year if ed=="3High_school_grad", sort lcolor(green) lwidth(medium) yaxis(1))
	(line f_ed year if ed=="4LT_high_school", sort lcolor(cranberry) lwidth(vthin))
	if t_edyr==1
,
	xtitle("")
	xlabel(none)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(0(.2)1, format(%9.2f) angle(horizontal))
	legend(title("Education") 
		order(1 "College grads" 2 "Some college" 3 "High school" 4 "<High school")
 		symxsize(*.5) colgap(*.2) keygap(*.1)
		region(fcolor(none))
		pos(12) ring(0) rows(2))
	xsize(10) ysize(7.5)
	name(f_ed, replace)
;
#delimit cr

#delimit ;
twoway
	(line f_sex year if sex=="2Women", sort lcolor(magenta))
	(line f_sex year if sex=="1Men", sort lcolor(blue) lwidth(thick))
	if t_sexyr==1
,
	xtitle("")
	xlabel(none)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(0(.2)1.0, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Gender")
		order(2 "Men" 1 "Women") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		region(fcolor(none))
		pos(12) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(f_sex, replace)
;
#delimit cr

#delimit ;
twoway
	(line f_white year if white=="1White", sort lcolor(green))
	(line f_white year if white=="2Non_White", sort lcolor(black) lwidth(medthick))
	if t_whiteyr==1
,
	xtitle("")
	xlabel(none)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.0(.2)1.0, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Race")
			order(1 "White" 2 "Non-white")
			region(fcolor(none) )
			symxsize(*.5) colgap(*.2) keygap(*.1)
			pos(12) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(f_white, replace)
;
#delimit cr

#delimit ;
twoway
	(line f_agegrp year if agegrp=="1Age_18_to_34", sort lcolor(ltblue) lwidth(thick))
	(line f_agegrp year if agegrp=="2Age_35_to_49", sort lcolor(olive) lwidth(medium))
	(line f_agegrp year if agegrp=="3Age_50_plus", sort lcolor(lavender) lwidth(vthin))
	if t_agegrpyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.0(.2)1.0, format(%9.2f) angle(horizontal) )
	legend(title("Age Group")
		order(1 "18-34 yrs" 2 "35-49 yrs" 3 "50+ yrs") 
		region(fcolor(none))
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(12) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(f_age, replace)
;
#delimit cr

#delimit ;
twoway
	(line f_mar year if mar=="1Married", sort lcolor(purple))
	(line f_mar year if mar=="2Unmarried", sort lcolor(orange) lwidth(medthick))
	if t_maryr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.0(.2)1.0, format(%9.2f) angle(horizontal) nolabel)
	legend(
		title("Marital Status")
		order(1 "Married" 2 "Unmarried") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		region(fcolor(none))
		pos(12) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(f_mar, replace)
;
#delimit cr

/*
#delimit ;
twoway
	(line f_imm3 year if imm3 =="1US", sort lcolor(gray) lwidth(thick))
	(line f_imm3 year if imm3 =="2Immigrant", sort lcolor(navy) lwidth(thin))
	(line f_imm3 year if imm3 =="3Unknown", sort lcolor(green) lwidth(thin) lpattern(dash))
	if t_imm3yr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.6(.2)1.8, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Nativity")
		order(1 "US born" 2 "Immigrant" 3 "Unknown") 
		region(fcolor(none))
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(12) ring(0) rows(2))
	xsize(10) ysize(7.5)
	name(f_imm3, replace)
;
#delimit cr
*/

#delimit ;
twoway
	(line f_reg year if reg=="1Northeast", sort lcolor(gray) lwidth(thick))
	(line f_reg year if reg=="2Midwest", sort lcolor(navy) lwidth(medthick))
	(line f_reg year if reg=="3South", sort lcolor(green) lwidth(medium))
	(line f_reg year if reg=="4West", sort lcolor(brown) lwidth(vthin))
	if t_regyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.0(.2)1.0, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Region")
		order(1 "Northeast" 2 "Midwest" 3 "South" 4 "West") 
		region(fcolor(none))
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(12) ring(0) rows(2))
	xsize(10) ysize(7.5)
	name(f_reg, replace)
;
#delimit cr


#delimit ;
graph combine
	f_ed f_sex f_white
	f_age f_mar f_reg /*f_imm3*/
,
	title("Trends in Sample Composition Across Groups")
	rows(2)
	imargin(vsmall)
	l2title("Proportion of sample")
	xsize(10) ysize(7.5)
	name(f, replace)
;
#delimit cr


* Publication versions
use gss_results, clear
* Create a graph that summarizes all of the results
* Between-group differences

#delimit ;
twoway
	(line mu_ed year if ed=="1College_grad", sort lcolor(gs12) lwidth(thick) yaxis(1))
	(line mu_ed year if ed=="2Some_college", sort lcolor(gs8) lwidth(medthick))
	(line mu_ed year if ed=="3High_school_grad", sort lcolor(gs4) lwidth(medium) yaxis(1))
	(line mu_ed year if ed=="4LT_high_school", sort lcolor(black) lwidth(thin))
	if t_edyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	ytitle("")
	ylabel(-.75(.25).25, format(%9.2f) angle(horizontal))
	legend(title("Education") 
		order(1 "College grads" 2 "Some college" 3 "High school" 4 "<High school")
 		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(6) ring(0) rows(2))
	xsize(10) ysize(7.5)
	name(mu_ed_pub, replace)
;
#delimit cr

#delimit ;
twoway
	(line mu_sex year if sex=="2Women", sort lcolor(black))
	(line mu_sex year if sex=="1Men", sort lcolor(gs6) lwidth(thick))
	if t_sexyr==1
,
	xtitle("")
	xlabel(none)
	xlabel(1970(10)2010)
	ytitle("")
	ylabel(-.75(.25).25, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Gender")
		order(2 "Men" 1 "Women") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(6) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(mu_sex_pub, replace)
;
#delimit cr

#delimit ;
twoway
	(line mu_white year if white=="1White", sort lcolor(gs6))
	(line mu_white year if white=="2Non_White", sort lcolor(black) lwidth(thick))
	if t_whiteyr==1
,
	xtitle("")
	xlabel(none)
	xlabel(1970(10)2010)
	ytitle("")
	ylabel(-.75(.25).25, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Race")
			order(1 "White" 2 "Non-white")
			region(fcolor(none) )
			symxsize(*.5) colgap(*.2) keygap(*.1)
			pos(6) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(mu_white_pub, replace)
;
#delimit cr

#delimit ;
twoway
	(line mu_agegrp year if agegrp=="1Age_18_to_34", sort lcolor(gs12) lwidth(thick))
	(line mu_agegrp year if agegrp=="2Age_35_to_49", sort lcolor(gs6) lwidth(medium))
	(line mu_agegrp year if agegrp=="3Age_50_plus", sort lcolor(black) lwidth(vthin))
	if t_agegrpyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(-.75(.25).25, format(%9.2f) angle(horizontal) )
	legend(title("Age Group")
		order(1 "18-34 yrs" 2 "35-49 yrs" 3 "50+ yrs") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(6) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(mu_age_pub, replace)
;
#delimit cr

#delimit ;
twoway
	(line mu_mar year if mar=="1Married", sort lcolor(black))
	(line mu_mar year if mar=="2Unmarried", sort lcolor(gs6) lwidth(thick))
	if t_maryr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(-.75(.25).25, format(%9.2f) angle(horizontal) nolabel)
	legend(
		title("Marital Status")
		order(1 "Married" 2 "Unmarried") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		region(fcolor(none))
		pos(6) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(mu_mar_pub, replace)
;
#delimit cr


#delimit ;
twoway
	(line mu_reg year if reg=="1Northeast", sort lcolor(gs12) lwidth(thick))
	(line mu_reg year if reg=="2Midwest", sort lcolor(gs8) lwidth(medthick))
	(line mu_reg year if reg=="3South", sort lcolor(gs4) lwidth(medium))
	(line mu_reg year if reg=="4West", sort lcolor(black) lwidth(thin))
	if t_regyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(-.75(.25).25, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Region")
		order(1 "Northeast" 2 "Midwest" 3 "South" 4 "West") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(6) ring(0) rows(2))
	xsize(10) ysize(7.5)
	name(mu_reg_pub, replace)
;
#delimit cr


#delimit ;
graph combine
	mu_ed_pub mu_sex_pub mu_white_pub
,
	rows(1)
	imargin(vsmall)
	l1title("Average (Conditional) Happiness Levels")
	ycommon
	xsize(7.125) ysize(4.125)
	name(mu1_pub, replace)
;
graph combine
	mu_age_pub mu_mar_pub mu_reg_pub
,
	rows(1)
	imargin(vsmall)
	l1title("Average (Conditional) Happiness Levels")
	ycommon
	xsize(7.125) ysize(4.125)
	name(mu2_pub, replace)
;



*** Dispersion
* Between-group differences

#delimit ;
twoway
	(line v_ed year if ed=="1College_grad", sort lcolor(gs12) lwidth(thick) yaxis(1))
	(line v_ed year if ed=="2Some_college", sort lcolor(gs8) lwidth(medthick))
	(line v_ed year if ed=="3High_school_grad", sort lcolor(gs4) lwidth(medium) yaxis(1))
	(line v_ed year if ed=="4LT_high_school", sort lcolor(black) lwidth(thin))
	if t_edyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.6(.2)1.8, format(%9.2f) angle(horizontal))
	legend(title("Education") 
		order(1 "College grads" 2 "Some college" 3 "High school" 4 "<High school")
 		symxsize(*.5) colgap(*.2) keygap(*.1)
		region(fcolor(none))
		pos(12) ring(0) rows(2))
	xsize(10) ysize(7.5)
	name(v_ed_pub, replace)
;
#delimit cr

#delimit ;
twoway
	(line v_sex year if sex=="2Women", sort lcolor(black))
	(line v_sex year if sex=="1Men", sort lcolor(gs6) lwidth(thick))
	if t_sexyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.6(.2)1.8, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Gender")
		order(2 "Men" 1 "Women") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		region(fcolor(none))
		pos(12) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(v_sex_pub, replace)
;
#delimit cr

#delimit ;
twoway
	(line v_white year if white=="1White", sort lcolor(gs6))
	(line v_white year if white=="2Non_White", sort lcolor(black) lwidth(thick))
	if t_whiteyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.6(.2)1.8, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Race")
			order(1 "White" 2 "Non-white")
			region(fcolor(none) )
			symxsize(*.5) colgap(*.2) keygap(*.1)
			pos(12) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(v_white_pub, replace)
;
#delimit cr

#delimit ;
twoway
	(line v_agegrp year if agegrp=="1Age_18_to_34", sort lcolor(gs12) lwidth(thick))
	(line v_agegrp year if agegrp=="2Age_35_to_49", sort lcolor(gs6) lwidth(medium))
	(line v_agegrp year if agegrp=="3Age_50_plus", sort lcolor(black) lwidth(vthin))
	if t_agegrpyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.6(.2)1.8, format(%9.2f) angle(horizontal) )
	legend(title("Age Group")
		order(1 "18-34 yrs" 2 "35-49 yrs" 3 "50+ yrs") 
		region(fcolor(none))
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(12) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(v_age_pub, replace)
;
#delimit cr

#delimit ;
twoway
	(line v_mar year if mar=="1Married", sort lcolor(black))
	(line v_mar year if mar=="2Unmarried", sort lcolor(gs6) lwidth(medthick))
	if t_maryr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.6(.2)1.8, format(%9.2f) angle(horizontal) nolabel)
	legend(
		title("Marital Status")
		order(1 "Married" 2 "Unmarried") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		region(fcolor(none))
		pos(12) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(v_mar_pub, replace)
;
#delimit cr

#delimit ;
twoway
	(line v_reg year if reg=="1Northeast", sort lcolor(gs12) lwidth(thick))
	(line v_reg year if reg=="2Midwest", sort lcolor(gs8) lwidth(medthick))
	(line v_reg year if reg=="3South", sort lcolor(gs4) lwidth(medium))
	(line v_reg year if reg=="4West", sort lcolor(black) lwidth(thin))
	if t_regyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.6(.2)1.8, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Region")
		order(1 "Northeast" 2 "Midwest" 3 "South" 4 "West") 
		region(fcolor(none))
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(12) ring(0) rows(2))
	xsize(10) ysize(7.5)
	name(v_reg_pub, replace)
;
#delimit cr


#delimit ;
graph combine
	v_ed_pub v_sex_pub v_white_pub
,
	rows(1)
	imargin(vsmall)
	l1title("(Conditional) Happiness Inequality")
	xsize(7.125) ysize(4.125)
	name(v1_pub, replace)
;
#delimit cr

#delimit ;
graph combine
	v_age_pub v_mar_pub v_reg_pub 
,
	rows(1)
	imargin(vsmall)
	l1title("(Conditional) Happiness Inequality")
	xsize(7.125) ysize(4.125)
	name(v2_pub, replace)
;
#delimit cr


*** Sample proportions

xi i.year
foreach v of global groups {
	gen f_`v'=.
	levelsof `v', local(values)
	foreach vl of local values {
		gen f=`v'=="`vl'" if hapsample==1
		reg f _I* [aw=wt]
		predict hat if hapsample==1 & f~=.
		replace f_`v'=hat if `v'=="`vl'"
		drop f hat
	}
}

#delimit ;
twoway
	(line f_ed year if ed=="1College_grad", sort lcolor(gs12) lwidth(thick) yaxis(1))
	(line f_ed year if ed=="2Some_college", sort lcolor(gs8) lwidth(medthick))
	(line f_ed year if ed=="3High_school_grad", sort lcolor(gs4) lwidth(medium) yaxis(1))
	(line f_ed year if ed=="4LT_high_school", sort lcolor(black) lwidth(vthin))
	if t_edyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(0(.2)1, format(%9.2f) angle(horizontal))
	legend(title("Education") 
		order(1 "College grads" 2 "Some college" 3 "High school" 4 "<High school")
 		symxsize(*.5) colgap(*.2) keygap(*.1)
		region(fcolor(none))
		pos(12) ring(0) rows(2))
	xsize(10) ysize(7.5)
	name(f_ed_pub, replace)
;
#delimit cr

#delimit ;
twoway
	(line f_sex year if sex=="2Women", sort lcolor(black))
	(line f_sex year if sex=="1Men", sort lcolor(gs6) lwidth(thick))
	if t_sexyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(0(.2)1.0, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Gender")
		order(2 "Men" 1 "Women") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		region(fcolor(none))
		pos(12) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(f_sex_pub, replace)
;
#delimit cr

#delimit ;
twoway
	(line f_white year if white=="1White", sort lcolor(gs6))
	(line f_white year if white=="2Non_White", sort lcolor(black) lwidth(thick))
	if t_whiteyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.0(.2)1.0, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Race")
			order(1 "White" 2 "Non-white")
			region(fcolor(none) )
			symxsize(*.5) colgap(*.2) keygap(*.1)
			pos(12) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(f_white_pub, replace)
;
#delimit cr

#delimit ;
twoway
	(line f_agegrp year if agegrp=="1Age_18_to_34", sort lcolor(gs12) lwidth(thick))
	(line f_agegrp year if agegrp=="2Age_35_to_49", sort lcolor(gs6) lwidth(medium))
	(line f_agegrp year if agegrp=="3Age_50_plus", sort lcolor(black) lwidth(vthin))
	if t_agegrpyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.0(.2)1.0, format(%9.2f) angle(horizontal) )
	legend(title("Age Group")
		order(1 "18-34 yrs" 2 "35-49 yrs" 3 "50+ yrs") 
		region(fcolor(none))
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(12) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(f_age_pub, replace)
;
#delimit cr

#delimit ;
twoway
	(line f_mar year if mar=="1Married", sort lcolor(black))
	(line f_mar year if mar=="2Unmarried", sort lcolor(gs6) lwidth(medthick))
	if t_maryr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.0(.2)1.0, format(%9.2f) angle(horizontal) nolabel)
	legend(
		title("Marital Status")
		order(1 "Married" 2 "Unmarried") 
		symxsize(*.5) colgap(*.2) keygap(*.1)
		region(fcolor(none))
		pos(12) ring(0) rows(1))
	xsize(10) ysize(7.5)
	name(f_mar_pub, replace)
;
#delimit cr

#delimit ;
twoway
	(line f_reg year if reg=="1Northeast", sort lcolor(gs12) lwidth(thick))
	(line f_reg year if reg=="2Midwest", sort lcolor(gs8) lwidth(medthick))
	(line f_reg year if reg=="3South", sort lcolor(gs4) lwidth(medium))
	(line f_reg year if reg=="4West", sort lcolor(black) lwidth(vthin))
	if t_regyr==1
,
	xtitle("")
	xlabel(1970(10)2010)
	xtick(1970(10)2010)
	ytitle("")
	ylabel(.0(.2)1.0, format(%9.2f) angle(horizontal) nolabel)
	legend(title("Region")
		order(1 "Northeast" 2 "Midwest" 3 "South" 4 "West") 
		region(fcolor(none))
		symxsize(*.5) colgap(*.2) keygap(*.1)
		pos(12) ring(0) rows(2))
	xsize(10) ysize(7.5)
	name(f_reg_pub, replace)
;
#delimit cr


#delimit ;
graph combine
	f_ed_pub f_sex_pub f_white_pub
,
	rows(1)
	imargin(vsmall)
	l2title("Proportion of sample")
	xsize(7.125) ysize(4.125)
	name(f1_pub, replace)
;
#delimit cr
#delimit ;
graph combine
	f_age_pub f_mar_pub f_reg_pub
,
	rows(1)
	imargin(vsmall)
	l2title("Proportion of sample")
	xsize(7.125) ysize(4.125)
	name(f2_pub, replace)
;
#delimit cr

******************************* Within v. Between Decomposition
********* Table 3: Decadal
use gss_results if hapsample==1, clear

table decade [aw=wt], c(m mu_dec_base m v_dec_base)

collapse (mean) muraw_d* vraw_d* (sum) wt, by(decade $groups all)
fillin $groups all decade
replace wt=0 if _fillin==1
foreach v of any $groups all {
	tomode muraw_d_`v', by(`v' decade) replace
	tomode vraw_d_`v', by(`v' decade) replace
}


foreach v of any all $groups {
	preserve
	collapse (mean) muraw_d_`v' vraw_d_`v' (sum) wt, by(decade `v')
	list
	gen str10 j="`v'"
	rename `v' k
	rename muraw_d_`v' beta
	rename vraw_d_`v' gamma
	sort decade
	save temp_`v', replace
	restore
}

clear
use temp_all, clear
foreach v of any $groups {
	append using temp_`v'.dta
	erase temp_`v'.dta
}
replace k=j+"_"+k

egen wt_jk=sum(wt), by(j k)
egen beta_bar=sum(beta*wt), by(k)
replace beta_bar=beta_bar/wt_jk
egen gamma_bar=sum(gamma*wt), by(k)
replace gamma_bar=gamma_bar/wt_jk

egen wt_jdec=sum(wt), by(j decade)
gen wt_per=wt/wt_jdec
egen wt_j=sum(wt), by(j)
gen wt_bar=wt_jk/wt_j

table k decade, c(m wt_per m beta m gamma)
table k, c(m wt_bar m beta_bar m gamma_bar)
table k, c(sd wt_bar sd beta_bar sd gamma_bar)

* Mean decomp

egen mu_1=sum(wt_bar*beta_bar), by(decade)
egen mu_2=sum(wt_bar*beta), by(decade)
egen mu_3=sum(wt_bar*beta), by(decade)
egen mu_4=sum(wt_per*beta), by(decade)
egen mu_5=sum(wt_per*beta), by(decade)

egen vw_1=sum(wt_bar*gamma_bar), by(decade)
egen vw_2=sum(wt_bar*gamma), by(decade)
egen vw_3=sum(wt_bar*gamma), by(decade)
egen vw_4=sum(wt_per*gamma), by(decade)
egen vw_5=sum(wt_per*gamma), by(decade)

gen mu_contrib=mu_4-mu_2
gen vw_contrib=vw_5-vw_3
table decade, c(m mu_1 m mu_2 m mu_4 m mu_contrib ) format(%9.3f)
table decade, c(m vw_1 m vw_3 m vw_5 m vw_contrib) format(%9.3f)

***************** Annual Decomposition: Figure 9
* Annual Data
use gss_results if hapsample==1, clear

collapse (mean) muraw_y* vraw_y* mu_ave v_ave (sum) wt, by(year $groups all)

fillin $groups all year
replace wt=0 if _fillin==1
foreach v of any $groups all {
	tomode muraw_y_`v', by(`v' year) replace
	tomode vraw_y_`v', by(`v' year) replace
}
egen t_year=tag(year)


* Generate weights with no time series variation
summ wt
gen wt_t=wt/(r(N)*r(mean))
egen wt_not=sum(wt), by($groups all)
summ wt_not
replace wt_not=wt_not/(r(N)*r(mean))

* Generate coefficients with no time series variation
foreach v of any $groups all {
	gen mu_not_`v'=.
	gen v_not_`v'=.
	levelsof `v', local(vals)
	foreach vl of local vals {
		summ muraw_y_`v' [aw=wt] if `v'=="`vl'"
		replace mu_not_`v'=r(mean) if `v'=="`vl'"
		summ vraw_y_`v' [aw=wt] if `v'=="`vl'"
		replace v_not_`v'=r(mean) if `v'=="`vl'"
	}
}

egen mu_t=rsum(muraw_y*)
egen vw_t=rsum(vraw_y*)
egen mu_not=rsum(mu_not_*)
egen vw_not=rsum(v_not*)

xi i.year, noomit
cap program drop decomp
program define decomp
	/* Arguments: Beta, X(for Xbeta), Gamma, X(for Xgamma), suffix*/
	reg `1' _I* [aw=`2'], noconst
	predict mu_`5'
	reg `3' _I* [aw=`4'], noconst
	predict vw_`5'
	tempvar sq mos
	gen `sq'=`1'^2
	reg `sq' _I* [aw=`2'], noconst
	predict `mos'
	gen vb_`5'=`mos'-mu_`5'^2
	gen  v_`5'=vw_`5'+vb_`5'
	twoway (line *_`5' year, sort) if t_year==1, name(g`5', replace)
end

* If everything is set at its means


decomp mu_not wt_not vw_not wt_not 1
decomp mu_t wt_not vw_not wt_not 2  /* Within change in levels */
decomp mu_t wt_not vw_t wt_not 3 /* Within change in dispersion*/
decomp mu_t wt_t vw_not wt_not 4 /* Comp change in levels */
decomp mu_t wt_t vw_t wt_t 5 /*Comp change in dispersion */



twoway (line mu_1 mu_1 mu_2 mu_3 mu_4 mu_5 year, sort) if t_year==1, title("Mean") name(mua, replace) 
twoway (line v_1 v_2 v_3 v_4 v_5 year, sort) if t_year==1, title("Variance") name(va, replace) 
twoway (line vw_1 vw_2 vw_3 vw_4 vw_5 year, sort) if t_year==1, title("Within-Group Variance") name(vw, replace) 
twoway (line vb_1 vb_2 vb_3 vb_4 vb_5 year, sort) if t_year==1, title("Between-Group Variance") name(vb, replace)  
graph combine mua va vw vb, rows(2) 


summ mu_1
global mu_1=r(mean)
#delimit ;
twoway
	(line mu_2 year, sort lwidth(medium) lcolor(green) lpattern(dash))
	(line mu_4 year, sort lwidth(thin) lcolor(blue)  lpattern(solid))
	if t_year==1
,
	title("Mean")
	title("Average Levels of Happiness")
	ytitle("")
	ylabel(-0.4(.2).4 0 " 0.0", angle(horizontal) format(%9.1f) )
	yline($mu_1, lcolor(black) lwidth(vvvthin))
	xtitle("")
	xlabel(1972(4)2008, labsize(small))
	legend(
		order(1 "Effect of within-group changes" "(fixed composition across groups)"
			2 "Plus compositional change" "=Average happiness"
			)
		size(small)
		rows(3) rowgap(tiny) bmargin(zero)
		ring(0) pos(6)
		)
	xsize(10) ysize(7.5)
	name(mean, replace)
;
#delimit cr

summ vb_1
global vb_1=r(mean)
#delimit ;
twoway
	(line vb_2 year, sort lwidth(medium) lcolor(green) lpattern(dash))
	(line vb_4 year, sort lwidth(thin) lcolor(blue)  lpattern(solid))
	if t_year==1
,
	title("Between-Group Variance of Happiness")
	ytitle("Between-Group Variance of Happiness")
	ytitle("")
	ylabel(-.4(.2).4 0 " 0.0", angle(horizontal) format(%9.1f))
	yline($vb_1, lcolor(black) lwidth(vvvthin))
	xtitle("")
	xlabel(1972(4)2008, labsize(small))
	legend(
		order(1 "Effect of within-group changes" "(fixed composition across groups)"
			2 "Plus compositional change" "=Average between-group variance")
		rowgap(tiny) bmargin(zero)
		size(small)
		region(fcolor(none))
		rows(4) ring(0) pos(6)
		)
	xsize(10) ysize(7.5)
	name(vb, replace)
;
#delimit cr

summ vw_1
global vw_1=r(mean)
#delimit ;
twoway
	(line vw_3 year, sort lwidth(medium) lcolor(purple) lpattern(longdash))
	(line vw_5 year, sort lwidth(thin) lcolor(gray))
	if t_year==1
,
	title("Average Within-Group Variance")
	ytitle("Within-Group Variance of Happiness")
	ytitle("")
	ylabel(.8(.2)1.6 1 " 1.0", angle(horizontal) format(%9.1f))
	yline($vw_1, lcolor(black) lwidth(vvvthin))
	xtitle("")
	xlabel(1972(4)2008, labsize(small))
	legend(
		order(1 "Within-group changes in dispersion" "(fixed composition across groups)"
			2 "Plus effect of compositional change" "=Average within-group variance")
		rowgap(tiny) bmargin(zero)
		size(small)
		region(fcolor(none))
		rows(4) ring(0) pos(2)
		)
	xsize(10) ysize(7.5)
	name(vw, replace)
;
#delimit cr
summ v_1
global v_1=r(mean)
#delimit ;
twoway
	(line v_2 year, sort lwidth(medium) lcolor(green) lpattern(dash))
	(line v_3 year, sort lwidth(medium) lcolor(purple) lpattern(longdash)) 
	(line v_5 year, sort lwidth(medthick) lcolor(black) mcolor(cranberry))
	if t_year==1
,
	title("Total Variance of Happiness")
	ytitle("Variance of Happiness")
	ytitle("")
	ylabel(.8(.2)1.6 1 " 1.0", angle(horizontal) format(%9.1f))
	yline($v_1, lcolor(black) lwidth(vvvthin))
	xtitle("")
	xlabel(1972(4)2008, labsize(small))
	legend(
		order(1 "Between-group changes in happiness"
			2 "Plus changes in within-group variance"
			3 "Plus effects of compositional change" "=Total variance")
		size(small)
		region(fcolor(none)) 
		rowgap(tiny) bmargin(zero) 
		rows(4) ring(0) pos(1)
		)
	xsize(10) ysize(7.5)
	name(var, replace)
;
#delimit cr


#delimit ;
graph combine
	mean vb
	vw var
,
	rows(2)
	imargin(tiny)
	xsize(10) ysize(7.5)
	name(muv, replace)
;
#delimit cr

summ mu_1 mu_2 mu_4 if t_year==1
summ vb_1 vb_2 vb_4 if t_year==1
summ vw_1 vw_3 vw_5 if t_year==1
summ v_1 v_2 v_3 v_5 if t_year==1

* Publication version of Figure 8
#delimit ;
twoway
	(line mu_2 year, sort lwidth(medium) lcolor(black) lpattern(dash))
	(line mu_4 year, sort lwidth(thin) lcolor(gs6)  lpattern(solid))
	if t_year==1
,
	ytitle("Average Levels of Happiness")
	ylabel(-0.4(.2).4 0 " 0.0", angle(horizontal) format(%9.1f) )
	yline($mu_1, lcolor(black) lwidth(vvvthin))
	xtitle("")
	xlabel(1972(4)2008, labsize(small))
	legend(title("Average Level of Happiness")
		order(
			1 "Effect of within-group changes" "(fixed composition across groups)"
			2 "Plus compositional change" "=Average happiness"
			)
		rows(3) rowgap(tiny) bmargin(zero)
		ring(0) pos(12)
		)
	xsize(10) ysize(7.5)
	name(mean_pub, replace)
;
#delimit cr

summ vb_1
global vb_1=r(mean)
#delimit ;
twoway
	(line vb_2 year, sort lwidth(medium) lcolor(black) lpattern(dash))
	(line vb_4 year, sort lwidth(thin) lcolor(gs6)  lpattern(solid))
	if t_year==1
,
	ytitle("Between-Group Variance of Happiness")
	ylabel(-.4(.2).4 0 " 0.0", angle(horizontal) format(%9.1f))
	yline($vb_1, lcolor(black) lwidth(vvvthin))
	xtitle("")
	xlabel(1972(4)2008, labsize(small))
	legend(title("Between-Group Variance of Happiness")
		order(1 "Effect of within-group changes" "(fixed composition across groups)"
			2 "Plus compositional change" "=Average between-group variance")
		rowgap(tiny) bmargin(zero)
		region(fcolor(none))
		rows(4) ring(0) pos(12)
		)
	xsize(10) ysize(7.5)
	name(vb_pub, replace)
;
#delimit cr

summ vw_1
global vw_1=r(mean)
#delimit ;
twoway
	(line vw_3 year, sort lwidth(medium) lcolor(black) lpattern(longdash))
	(line vw_5 year, sort lwidth(thin) lcolor(gs6))
	if t_year==1
,
	ytitle("Within-Group Variance of Happiness")
	ylabel(.8(.2)1.6 1 " 1.0", angle(horizontal) format(%9.1f))
	yline($vw_1, lcolor(black) lwidth(vvvthin))
	xtitle("")
	xlabel(1972(4)2008, labsize(small))
	legend(title("Within-Group Variance of Happiness")
		order(1 "Within-group changes in dispersion" "(fixed composition across groups)"
			2 "Plus effect of compositional change" "=Average within-group variance")
		rowgap(tiny) bmargin(zero)
		region(fcolor(none))
		rows(4) ring(0) pos(12)
		)
	xsize(10) ysize(7.5)
	name(vw_pub, replace)
;
#delimit cr
summ v_1
global v_1=r(mean)
#delimit ;
twoway
	(line v_2 year, sort lwidth(medium) lcolor(gs6) lpattern(dash))
	(line v_3 year, sort lwidth(medium) lcolor(black) lpattern(longdash) ) 
	(line v_5 year, sort lwidth(medthick) lcolor(black) mcolor(cranberry))
	if t_year==1
,
	ytitle("Total Variance of Happiness")
	ylabel(.8(.2)1.6 1 " 1.0", angle(horizontal) format(%9.1f))
	yline($v_1, lcolor(black) lwidth(vvvthin))
	xtitle("")
	xlabel(1972(4)2008, labsize(small))
	legend(title("Total Variance of Happiness")
		order(1 "Between-group changes in happiness"
			2 "Plus changes in within-group variance"
			3 "Plus effects of compositional change" "=Total variance")
		region(fcolor(none)) 
		rowgap(tiny) bmargin(zero) 
		rows(4) ring(0) pos(12)
		)
	xsize(10) ysize(7.5)
	name(var_pub, replace)
;
#delimit cr


#delimit ;
graph combine
	mean_pub var_pub
,
	rows(2)
	imargin(tiny)
	xsize(4.125) ysize(7.125)
 	name(muv1_pub, replace)
;
#delimit cr

#delimit ;
graph combine
	vb_pub vw_pub
,
	rows(2)
	imargin(tiny)
	xsize(4.125) ysize(7.125)
	name(muv2_pub, replace)
;
