
/*------------------------------------------------------------------------------
#* Post: "Predicting socio-economic outcomes with Google Trends" https://pablogguz.github.io/blog/
#* 
#* Input: 
#* 
#* - ACS microdata for 2019 (downloaded from IPUMS)
#* 
#* Output:
#* 
#* - Hispanic population shares per county
#* 
#* Code by Pablo Garcia Guzman
------------------------------------------------------------------------------*/

**# Set paths
	global root "C:\Users\\`c(username)'\\Documents/GitHub/blog_posts_code/_2024/gtrends_hispanic/"

	global input  "$root\input\"
	global output "$root\output\"

	global acs_data "C:\Users\pablo\Dropbox\hypergamy\0_data\acs_data\raw\"

	clear all
	set maxvar 120000
	set scheme cleanplots
	set graph on 
	
**# Load data
	use "$acs_data/acs_2022.dta"
	
**# Collapse 
	g hispanic = (hispan>0) & hispan !=.
	g boricua = (hispan==2) & hispan !=.
	g mexican = (hispan==1) & hispan !=. 

	gcollapse (mean) hispanic boricua mexican [aw=perwt], by(statefip)
	
**# Save
	compress
	save "$output/hispanic_shares.dta", replace
	
