
/*------------------------------------------------------------------------------
Post: "Wellbeing I: Introduction" https://pablogguz.github.io/blog/

Code by Pablo Garcia Guzman
------------------------------------------------------------------------------*/

**# Set paths
	global root "C:\Users\\`c(username)'\\Documents/GitHub/blog_posts_code/_2024/wellbeing_i_intro/"

	global input  "$root\input\"
	global output "$root\output\"

	clear all
	set maxvar 120000
	set scheme cleanplots
	set graph on 
	
**# WHR panel ------------------------------------------------------------------
	use "$output/whr_panel.dta", clear
	
**# Long-run differences: for each country, keep latest and first year in the sample
	bys iso2c (year): g tag = 1 if _n == _N 
	bys iso2c (year): replace tag = 1 if _n == 1
	
	keep if inlist(tag,1)
	drop if iso2c == ""
	drop tag 
	
	foreach var in lifesatis log_gdp {
		bys iso2c (year): g delta_`var' = `var' - `var'[_n-1]
	}
	
**# Generate binscatter 
	scatter delta_lifesatis delta_log_gdp

	ex
	
/*

**# WHR + WDI ------------------------------------------------------------------
	use "$output/wdi_whr.dta", clear

	binscatter lifexp_male log_gdp, control(lifesatis)
	
	
	
	
	
	
