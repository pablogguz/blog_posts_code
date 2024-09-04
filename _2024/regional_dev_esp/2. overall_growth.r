#* ------------------------------------------------------------------------------
#* Post: "A tale of regional development in Spain"
#*
#* This script:
#* - Analysis of ARDECO data for NUTS regions in Spain
#* 
#* Code by Pablo Garcia Guzman
#* ------------------------------------------------------------------------------

packages_to_load <- c(
    "tidyverse",
    "haven",
    "readxl",
    "tmap",
    "sf",
    "extrafont",
    "ggpubr",
    "ggrepel",
    "ARDECO",
    "fst",
    "giscoR",
    "stargazer",
    "fixest",
    "countrycode",
    "pander"
)

package.check <- lapply(
    packages_to_load,
    FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
        }
    }
)

lapply(packages_to_load, require, character = T)

# Set paths ----
username <- Sys.getenv("USERNAME")

root <- paste0("C:/Users/", username, "/Documents/GitHub/blog_posts_code/_2024/regional_dev_esp/")
input <- paste0(root, "/input/")
output <- paste0(root, "/output/")

options(vsc.dev.args = list(width = 800, height = 500))

if (!dir.exists(input)) {
  dir.create(input, recursive = TRUE)
}

if (!dir.exists(output)) {
  dir.create(output, recursive = TRUE)
}

blog_path <- paste0("C:/Users/", username, "/Documents/GitHub/pablogguz.github.io/static/img/")

# Script starts ----------------------------------------------------------------

# Download NUTS labels from GISCO 
nuts3 <- gisco_get_nuts(nuts_level = 3, year = 2021)

esp_labels <- nuts3 %>%
  filter(CNTR_CODE == "ES") %>%
  rename(
    nutscode = NUTS_ID,
    nutsname = NAME_LATN
  ) %>%
  as.data.frame() %>%
  select(nutscode, nutsname)

# Load ARDECO data 
data <- read.fst(paste0(output, "ardeco_data.fst"))

nuts_labels <- nuts3 %>%
  rename(
    nutscode = NUTS_ID,
    nutsname = NAME_LATN
  ) %>%
  as.data.frame() %>%
  select(nutscode, nutsname)

# Growth rates, country-level 
country_level <- data %>%
  filter(level == 0) %>% # NUTS 3
  mutate(
    iso2c = substr(nutscode, 1, 2),
  ) %>%
  select(nutscode, iso2c, year, SOVGDP_EUR2015, SNPTD_Persons, SOVGDH_EUR2015) %>%
  rename(
    pop = SNPTD_Persons,
    gdp_pc_eur_2015 = SOVGDP_EUR2015,
    productivity = SOVGDH_EUR2015
  ) %>%
  filter(
    !grepl("_NM", nutscode) & # remove non-metropolitan region category
    !grepl("ZZ", nutscode) # remove "extra-regio" category (all missing)
  ) %>%
  filter(between(as.numeric(year), 1995, 2023)) %>%
  mutate(
    year = as.numeric(year),
    log_gdp = log(gdp_pc_eur_2015),
    log_prod = log(productivity),
    country_name = countrycode(iso2c, "iso2c", "country.name"),
    country_name = ifelse(iso2c == "UK", "United Kingdom", country_name),
    country_name = ifelse(iso2c == "EL", "Greece", country_name)
  ) %>%
  # Keep earliest and latest year for each region
  group_by(nutscode) %>%
  filter(year == min(year) | year == max(year)) %>% 
  mutate(
    growth_rate = (gdp_pc_eur_2015[year == max(year)] - gdp_pc_eur_2015[year == min(year)]) / gdp_pc_eur_2015[year == min(year)] * 100,
    prod_growth = (productivity[year == max(year)] - productivity[year == min(year)]) / productivity[year == min(year)] * 100
  ) %>%
  mutate(
    log_gdp_2023 = log_gdp[year == max(year)],
    log_prod_2023 = log_prod[year == max(year)]
  ) %>%
  ungroup() %>%
  filter(year == min(year)) %>%
  rename(
    log_gdp_1995 = log_gdp,
    log_prod_1995 = log_prod
  ) %>%
  # Drop countries with at least one missing growth rate
  group_by(iso2c) %>%
  filter(all(!is.na(growth_rate))) %>%
  ungroup() %>%
  filter(iso2c != "IE") # Remove Ireland, GDP figures are meaningless

# Plot
country_level_plot <- country_level %>%
    select(nutscode, country_name, iso2c, growth_rate, prod_growth, log_gdp_1995, gdp_pc_eur_2015) %>%
    mutate(
        median_growth = median(growth_rate, na.rm = TRUE),
        median_gdp_1995 = median(gdp_pc_eur_2015, na.rm = TRUE),
        income_group = case_when(
            growth_rate > median_growth & gdp_pc_eur_2015 > median_gdp_1995 ~ "High income, high growth",
            growth_rate <= median_growth & gdp_pc_eur_2015 <= median_gdp_1995 ~ "Low income, low growth",
            growth_rate > median_growth & gdp_pc_eur_2015 <= median_gdp_1995 ~ "High income, low growth",
            growth_rate <= median_growth & gdp_pc_eur_2015 > median_gdp_1995 ~ "Low income, high growth"
        )
    )

# Scatter plot with 4 quadrants
chart <- country_level_plot %>%
    ggplot(aes(x = gdp_pc_eur_2015, y = growth_rate, color = income_group)) +
    geom_point(size = 3, alpha = 0.7, stroke = 0) +
    geom_vline(aes(xintercept = median_gdp_1995), linetype = "dashed", color = "gray", alpha = 0.8) +
    geom_hline(aes(yintercept = median_growth), linetype = "dashed", color = "gray", alpha = 0.8) +
    labs(
        title = "Cross-country convergence in Europe",
        x = "GDP per capita in 1995",
        y = "GDP per capita growth rate (1995-2023)",
        caption = "@pablogguz_ | Source: ARDECO database and author's calculations"
    )  +
    geom_text_repel(data = country_level_plot, 
                  aes(label = country_name, color = income_group),  
                  min.segment.length = 0,
                  box.padding = 0.5,
                  size = 4.5, 
                  family = "Open Sans",
                  show.legend = FALSE) +
    scale_color_manual(
        values = c(
                "High income, high growth" = "orange", 
                "Low income, low growth" = "red4", 
                "High income, low growth" = "navy", 
                "Low income, high growth" = "aquamarine4"
        )
    ) +
    theme_minimal(base_family = "Open Sans") +
    theme(
        text = element_text(family = "Open Sans", size = 11),
        plot.title = element_text(hjust = 0, size = 16),
        axis.title = element_text(size = 14),  
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        plot.background = element_rect(fill = "#f2f2f2", color = NA),
        plot.caption = element_text(size = 9, hjust = 0, vjust = 0, colour = "#3C4043"),
        legend.position = "top",
        legend.justification = "left",
        legend.title = element_blank()
    ) +
    scale_x_continuous(labels = scales::comma, trans = "log10") +
    scale_y_continuous(labels = scales::comma, breaks = seq(0, max(country_level_plot$growth_rate), by = 50))

print(chart)
ggsave(paste0(blog_path, "/growth_4q", ".png"), chart, dpi = 600, width = 9, height = 6)
ggsave(paste0(output, "/growth_4q", ".png"), chart, dpi = 600, width = 9, height = 6)


# # Growth rates, NUTS 3
# nuts3_growth <- data %>%
#   filter(level == 3) %>% # NUTS 3
#   mutate(
#     iso2c = substr(nutscode, 1, 2),
#   ) %>%
#   select(nutscode, iso2c, year, SOVGDP_EUR2015, SNPTD_Persons, SOVGDH_EUR2015) %>%
#   rename(
#     pop = SNPTD_Persons,
#     gdp_pc_eur_2015 = SOVGDP_EUR2015,
#     productivity = SOVGDH_EUR2015
#   ) %>%
#   filter(
#     !grepl("_NM", nutscode) & # remove non-metropolitan region category
#     !grepl("ZZ", nutscode) # remove "extra-regio" category (all missing)
#   ) %>%
#   # Keep countries with at least 5 NUTS3 regions
#   group_by(iso2c, year) %>%
#   filter(n() >= 5) %>%
#   ungroup() %>%
#   filter(between(as.numeric(year), 1995, 2023)) %>%
#   left_join(nuts_labels) %>%
#   mutate(
#     year = as.numeric(year),
#     log_gdp = log(gdp_pc_eur_2015),
#     log_prod = log(productivity)
#   ) %>%
#   # Keep earliest and latest year for each region
#   group_by(nutscode) %>%
#   filter(year == min(year) | year == max(year)) %>% 
#   mutate(
#     growth_rate = (gdp_pc_eur_2015[year == max(year)] - gdp_pc_eur_2015[year == min(year)]) / gdp_pc_eur_2015[year == min(year)] * 100,
#     prod_growth = (gdp_pc_eur_2015[year == max(year)] - gdp_pc_eur_2015[year == min(year)]) / gdp_pc_eur_2015[year == min(year)] * 100
#   ) %>%
#   mutate(
#     log_gdp_2023 = log_gdp[year == max(year)],
#     log_prod_2023 = log_prod[year == max(year)]
#   ) %>%
#   ungroup() %>%
#   filter(year == min(year)) %>%
#   rename(
#     log_gdp_1995 = log_gdp,
#     log_prod_1995 = log_prod
#   ) %>%
#   # Drop countries with at least one missing growth rate
#   group_by(iso2c) %>%
#   filter(all(!is.na(growth_rate))) %>%
#   ungroup() %>%
#   filter(iso2c != "IE") # Remove Ireland, GDP figures are meaningless

# nuts3_growth <- nuts3_growth %>%
#   select(nutscode, nutsname, iso2c, growth_rate, prod_growth) 

# # Histogram of growth rates
# nuts3_growth %>%
#     ggplot(aes(x = growth_rate)) +
#     geom_histogram(data = . %>% filter(iso2c == "ES"), aes(y = ..count../sum(..count..)), bins = 30, fill = "red", color = NA, alpha = 0.7) +
#     geom_histogram(data = . %>% filter(iso2c != "ES"), aes(y = ..count../sum(..count..)), bins = 30, fill = "blue", color = NA, alpha = 0.7) +
#     labs(
#         title = "",
#         x = "GDP per capita growth 1995-2023 (log difference)",
#         y = "Fraction"
#     ) +
#     theme_minimal() +
#     theme(
#         plot.title = element_text(size = 14, hjust = 0.5)
#     )
