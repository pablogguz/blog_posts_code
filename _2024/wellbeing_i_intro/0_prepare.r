
#*******************************************************************************
#* Post: "Wellbeing I: Introduction" https://pablogguz.github.io/blog/
#* 
#* Code by Pablo Garcia Guzman
#*******************************************************************************

# Load packages ----

packages_to_load <- c("tidyverse", 
                      "countrycode",
                      "WDI",
                      "readxl",
                      "binsreg",
                      "haven") 

package.check <- lapply(
  packages_to_load,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)

lapply(packages_to_load, require, character=T)

# Set paths ----
username <- Sys.getenv("USERNAME")

input <- paste0("C:/Users/", username, "/Documents/GitHub/blog_posts_code/_2024/wellbeing_i_intro/input/")
output <- paste0("C:/Users/", username, "/Documents/GitHub/blog_posts_code/_2024/wellbeing_i_intro/output/")

# Script starts ----------------------------------------------------------------

# Read data from WDI ----
wdi <- WDI(
  country = "all",
  indicator = c(lifexp_male = "SP.DYN.LE00.MA.IN", 
                lifexp_fem = "SP.DYN.LE00.FE.IN", 
                lifexp_tot = "SP.DYN.LE00.IN", 
                suicide_rate_male = "SH.STA.SUIC.MA.P5",
                suicide_rate_female = "SH.STA.SUIC.FE.P5",
                suicide_rate_tot = "SH.STA.SUIC.P5",
                survival_to65_male = "SP.DYN.TO65.MA.ZS",
                survival_to65_female = "SP.DYN.TO65.FE.ZS",
                gdp_pc_ppp = "NY.GDP.PCAP.PP.KD",
                gdp_pc_usd = "NY.GDP.PCAP.CD"),
  latest = 1
) 

# Read data from WHR 2023 ----
# Source: https://worldhappiness.report/ed/2023/

whr_panel <- read_xls(paste0(input, "/DataForTable2.1.xls")) %>%
  mutate(
    iso2c = countrycode(`Country name`, "country.name", "iso2c"),
    lifesatis = `Life Ladder`,
    log_gdp = `Log GDP per capita`) %>%
  select(iso2c, lifesatis, log_gdp, year)
  
whr <- read_xls(paste0(input, "/DataForFigure2.1WHR2023.xls")) %>%
  mutate(
    iso2c = countrycode(`Country name`, "country.name", "iso2c"),
    lifesatis = `Ladder score`,
    log_gdp = `Logged GDP per capita`) %>%
  select(iso2c, lifesatis, log_gdp)
  
## Save ----
write_dta(whr_panel, paste0(output, "whr_panel.dta"))

# Merge datasets ----
data <- left_join(whr, wdi) %>%
  mutate(iso2c = as.factor(iso2c))

# Save ----
write_dta(data, paste0(output, "wdi_whr.dta"))
