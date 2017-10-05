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


use "/Users/prakashmishra/Dropbox/gss_single_mothers/Analysis/Inputs/GSS7216_R2.DTA"
*******************************************************************************

keep year id wrkstat found marital martype childs educ sex age babies preteen teens adults unrelat income work10 nathealy happy health life helpful trust satfam sathealt control wtss region widowed divorce

save "`INTERDIR'/gss_clean.dta", replace

