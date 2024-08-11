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
    "pander",
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

if (!dir.exists(input)) {
  dir.create(input, recursive = TRUE)
}

if (!dir.exists(output)) {
  dir.create(output, recursive = TRUE)
}

blog_path <- paste0("C:/Users/", username, "/Documents/GitHub/pablogguz.github.io/static/img/")

# Script starts ----------------------------------------------------------------

# Download NUTS labels from GISCO 
nuts3 <- gisco_get_nuts(nuts_level = 3)

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

# Convergence analysis 
convergence <- data %>%
  filter(level == 3) %>% # NUTS 3
  mutate(
    iso2c = substr(nutscode, 1, 2),
  ) %>%
  select(nutscode, iso2c, year, SOVGDP_EUR2015, SNPTD_Persons) %>%
  rename(
    pop = SNPTD_Persons,
    gdp_pc_eur_2015 = SOVGDP_EUR2015
  ) %>%
  filter(
    !grepl("_NM", nutscode) & # remove non-metropolitan region category
    !grepl("ZZ", nutscode) # remove "extra-regio" category (all missing)
  ) %>%
  # Keep countries with at least 15 NUTS3 regions
  group_by(iso2c, year) %>%
  filter(n() >= 15) %>%
  ungroup() %>%
  filter(between(as.numeric(year), 1980, 2023)) %>%
  left_join(esp_labels) %>%
  mutate(
    year = as.numeric(year),
    log_gdp = log(gdp_pc_eur_2015)
  ) %>%
  # Keep earliest and latest year for each region
  group_by(nutscode) %>%
  filter(year == min(year) | year == max(year)) %>% 
  mutate(
    growth_rate = (log_gdp[year == max(year)] - log_gdp[year == min(year)]) / (max(year) - min(year))
  ) %>%
  mutate(
    log_gdp_2023 = log_gdp[year == max(year)]
  ) %>%
  ungroup() %>%
  filter(year == min(year)) %>%
  rename(
    log_gdp_1980 = log_gdp
  ) %>%
  # Drop countries with at least one missing growth rate
  group_by(iso2c) %>%
  filter(all(!is.na(growth_rate))) %>%
  ungroup() %>%
  filter(iso2c != "IE") # Remove Ireland, GDP figures are meaningless

# Regression 
convergence_esp <- convergence %>%
  filter(iso2c == "ES")

reg <- feols(growth_rate ~ log_gdp_1980, data = convergence_esp, se = "hetero")
etable(reg, postprocess.df = pandoc.table.return, style = "rmarkdown", se.below = TRUE, title = "")

# Visualize 
chart <- ggplot(convergence_esp, aes(x = log_gdp_1980, y = growth_rate)) +
  geom_point(data = convergence_esp, size = 3, alpha = 0.5, stroke = 0, color = "navy") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "navy") +
  geom_text_repel(data = convergence_esp, 
                  aes(label = nutsname),
                  min.segment.length = 0.5,
                  box.padding = 0.5,
                  max.overlaps = 7,
                  size = 4.5, 
                  family = "Open Sans",
                  show.legend = FALSE) +
  labs(x = str_wrap("Log GDP per capita in 1980", 60), 
       y = str_wrap("Log difference in GDP per capita (2023-1980)", 60),
       caption = "@pablogguz_ | Source: ARDECO database and author's calculations",
       title = "β-convergence in Spain (1980-2023)\n") +
  theme_minimal(base_family = "Open Sans") +
  theme(
    text = element_text(family = "Open Sans", size = 12),
    plot.title = element_text(hjust = 0, size = 18),
    axis.title = element_text(size = 15),  
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.caption = element_text(size = 9, hjust = 0, vjust = 0, colour = "#3C4043")
  )

ggsave(paste0(blog_path, "/convergence.png"), chart, dpi = 600, width = 9, height = 6)
ggsave(paste0(output, "/convergence.png"), chart, dpi = 600, width = 9, height = 6)

# Run regression by country, extract coefficients and create bar plot
convergence_countries <- convergence %>%
  group_by(iso2c) %>%
  nest() %>%
  mutate(
    reg = map(data, ~ feols(growth_rate ~ log_gdp_1980, data = .x, se = "hetero")),
    coef_log_gdp = map_dbl(reg, ~ coef(.x)["log_gdp_1980"])
  ) %>%
  select(iso2c, coef_log_gdp) %>%
  ungroup() %>%
  mutate(
    iso2c = ifelse(iso2c == "EL", "GR", iso2c), # change Greece to official ISO code
    iso2c = ifelse(iso2c == "UK", "GB", iso2c), # change UK to official ISO code
    countryname = countrycode::countrycode(iso2c, "iso2c", "country.name")
  )

# Create the bar plot
chart <- ggplot(convergence_countries, aes(x = reorder(countryname, coef_log_gdp), y = coef_log_gdp)) +
  geom_bar(aes(fill = ifelse(countryname == "Spain", "Spain", "Other")), 
           stat = "identity", alpha = 0.7) +
  scale_fill_manual(values = c("Spain" = "red", "Other" = "navy")) +
  labs(
    x = "",
    y = "β-convergence coefficient value",
    title = "Within-country β-convergence coefficients (1980-2023)",
    caption = "@pablogguz_ | Source: ARDECO database and author's calculations"
  ) +
  theme_minimal(base_family = "Open Sans") +
  theme(
    text = element_text(family = "Open Sans", size = 12),
    plot.title = element_text(hjust = 0, size = 18),
    axis.title = element_text(size = 15),  
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.caption = element_text(size = 9, hjust = 0, vjust = 0, colour = "#3C4043"),
    legend.position = "none"  # Remove the legend
  ) + coord_flip()
   
ggsave(paste0(blog_path, "/convergence_ctries.png"), chart, dpi = 600, width = 9, height = 6)
ggsave(paste0(output, "/convergence_ctries.png"), chart, dpi = 600, width = 9, height = 6)

# Sigma-convergence 
sigma <- convergence %>%
  group_by(iso2c) %>%
  mutate(
    sd_1980 = sd(log_gdp_1980),
    sd_2023 = sd(log_gdp_2023)
  ) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(iso2c, sd_1980, sd_2023) %>%
  mutate(
    iso2c = ifelse(iso2c == "EL", "GR", iso2c), # change Greece to official ISO code
    iso2c = ifelse(iso2c == "UK", "GB", iso2c), # change UK to official ISO code
    countryname = countrycode::countrycode(iso2c, "iso2c", "country.name")
  )

# Create dumbbell chart 
chart <- ggplot(sigma) +
  # Draw lines connecting 1980 and 2023 standard deviations
  geom_segment(aes(x = sd_1980, xend = sd_2023, y = reorder(countryname, sd_2023), yend = reorder(countryname, sd_2023)),
               color = "grey", size = 1) +
  # Add points for 1980 values
  geom_point(aes(x = sd_1980, y = reorder(countryname, sd_2023), color = "1980"), 
             size = 3, alpha = 0.7) +
  # Add points for 2023 values
  geom_point(aes(x = sd_2023, y = reorder(countryname, sd_2023), color = "2023"), 
             size = 3, alpha = 0.7) +
  scale_color_manual(values = c("1980" = "navy", "2023" = "red")) +
  labs(
    x = "Standard deviation of log GDP per capita",
    y = "",
    title = "Within-country σ-convergence (1980-2023)",
    caption = "@pablogguz_ | Source: ARDECO database and author's calculations",
    color = ""  # This sets the legend title to be blank
  ) +
  theme_minimal(base_family = "Open Sans") +
  theme(
    text = element_text(family = "Open Sans", size = 12),
    plot.title = element_text(hjust = 0, size = 18),
    axis.title = element_text(size = 15),  
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.caption = element_text(size = 9, hjust = 0, vjust = 0, colour = "#3C4043"),
    legend.position = "top", 
    legend.title = element_blank(),
    legend.justification = c(0, 1),  
    legend.text = element_text(size = 13)  
  ) 

ggsave(paste0(blog_path, "/sigma.png"), chart, dpi = 600, width = 9, height = 6)
ggsave(paste0(output, "/sigma.png"), chart, dpi = 600, width = 9, height = 6)
