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

options(vsc.dev.args = list(width = 600, height = 400))

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
  # Keep countries with at least 5 NUTS3 regions
  group_by(iso2c, year) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  filter(between(as.numeric(year), 1995, 2023)) %>%
  left_join(esp_labels) %>%
  mutate(
    year = as.numeric(year),
    log_gdp = log(gdp_pc_eur_2015),
    log_prod = log(productivity)
  ) %>%
  # Keep earliest and latest year for each region
  group_by(nutscode) %>%
  filter(year == min(year) | year == max(year)) %>% 
  mutate(
    growth_rate = (log_gdp[year == max(year)] - log_gdp[year == min(year)]) / (max(year) - min(year)),
    prod_growth = (log_prod[year == max(year)] - log_prod[year == min(year)]) / (max(year) - min(year))
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

# Regression 
convergence_esp <- convergence %>%
  filter(iso2c == "ES")

reg_gdp <- feols(growth_rate ~ log_gdp_1995, data = convergence_esp, se = "hetero")
reg_prod <- feols(prod_growth ~ log_prod_1995, data = convergence_esp, se = "hetero")

etable(reg_gdp, postprocess.df = pandoc.table.return, style = "rmarkdown", se.below = TRUE, title = "")
etable(reg_prod, postprocess.df = pandoc.table.return, style = "rmarkdown", se.below = TRUE, title = "")

# Chart with GDP and productivity side-by-side
convergence_combined <- convergence_esp %>%
  mutate(
    type = "GDP per capita",
    x_var = log_gdp_1995,
    y_var = growth_rate
  ) %>%
  select(nutsname, x_var, y_var, type) %>%
  bind_rows(
    convergence_esp %>%
      mutate(
        type = "Real labour productivity",
        x_var = log_prod_1995,
        y_var = prod_growth
      ) %>%
      select(nutsname, x_var, y_var, type)
  )

# Create the faceted plot
caption <- "@pablogguz_ | Source: ARDECO database and author's calculations. Real labour productivity is measured as GDP in constant prices per hour worked."

chart <- ggplot(convergence_combined, aes(x = x_var, y = y_var)) +
  geom_point(size = 3, alpha = 0.5, stroke = 0, color = "navy") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "navy") +
  geom_text_repel(
    aes(label = nutsname),
    min.segment.length = 0.5,
    box.padding = 0.5,
    max.overlaps = 7,
    size = 3, 
    family = "Open Sans",
    show.legend = FALSE
  ) +
  labs(
    x = "Log value in 1995", 
    y = "Log difference (2023-1995)",
    caption = str_wrap(caption, width = 125),
    title = "β-convergence in Spain (1995-2023)\n"
  ) +
  theme_minimal(base_family = "Open Sans") +
  theme(
    text = element_text(family = "Open Sans", size = 11),
    plot.title = element_text(hjust = 0, size = 16),
    axis.title = element_text(size = 14),  
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    plot.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.caption = element_text(size = 9, hjust = 0, vjust = 0, colour = "#3C4043"),
    strip.text = element_text(size = 14)  # Increase size of grid titles
  ) +
  facet_grid(. ~ type, scales = "free_x", space = "fixed")

# Print the chart
print(chart)

# Save the chart
ggsave(paste0(blog_path, "/convergence.png"), chart, dpi = 600, width = 9, height = 6)
ggsave(paste0(output, "/convergence.png"), chart, dpi = 600, width = 9, height = 6)

# Convergence function
# convergence_function <- function(x_var, y_var, x_lab, y_lab, title) {
#   chart <- ggplot(convergence_esp, aes(x = {{x_var}}, y = {{y_var}})) +
#     geom_point(size = 3, alpha = 0.5, stroke = 0, color = "navy") +
#     geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "navy") +
#     geom_text_repel(
#       aes(label = nutsname),
#       min.segment.length = 0.5,
#       box.padding = 0.5,
#       max.overlaps = 7,
#       size = 4.5, 
#       family = "Open Sans",
#       show.legend = FALSE
#     ) +
#     labs(
#       x = str_wrap(x_lab, 60), 
#       y = str_wrap(y_lab, 60),
#       caption = "@pablogguz_ | Source: ARDECO database and author's calculations",
#       title = title
#     ) +
#     theme_minimal(base_family = "Open Sans") +
#     theme(
#       text = element_text(family = "Open Sans", size = 11),
#       plot.title = element_text(hjust = 0, size = 16),
#       axis.title = element_text(size = 14),  
#       axis.text.y = element_text(size = 11),
#       axis.text.x = element_text(size = 11),
#       plot.background = element_rect(fill = "#f2f2f2", color = NA),
#       plot.caption = element_text(size = 9, hjust = 0, vjust = 0, colour = "#3C4043")
#     )

#   ggsave(paste0(blog_path, "/convergence_", deparse(substitute(x_var)), ".png"), chart, dpi = 600, width = 9, height = 6)
#   ggsave(paste0(output, "/convergence_", deparse(substitute(x_var)), ".png"), chart, dpi = 600, width = 9, height = 6)
  
#   return(chart) 
# }

# convergence_function(
#   log_gdp_1995, 
#   growth_rate, 
#   "Log GDP per capita in 1995", 
#   "Log difference in GDP per capita (2023-1995)", 
#   "β-convergence in Spain (1995-2023)\n"
# )

# convergence_function(
#   log_prod_1995, 
#   prod_growth, 
#   "Log real labour productivity per hour worked in 1995", 
#   "Log difference in real labour productivity per hour worked (2023-1995)", 
#   "β-convergence in productivity in Spain (1995-2023)\n"
# )

# Run regression by country, extract coefficients and create bar plot
convergence_countries <- convergence %>%
  group_by(iso2c) %>%
  nest() %>%
  mutate(
    reg_gdp = map(data, ~ feols(growth_rate ~ log_gdp_1995, data = .x, se = "hetero")),
    reg_prod = map(data, ~ if (all(!is.na(.x$log_prod_1995) & !is.na(.x$prod_growth))) {
      feols(prod_growth ~ log_prod_1995, data = .x, se = "hetero")
    } else {
      NULL  # If missing productivity data, return NULL
    }),
    coef_log_gdp = map_dbl(reg_gdp, ~ coef(.x)["log_gdp_1995"]),
    coef_log_prod = map_dbl(reg_prod, ~ if (!is.null(.x)) coef(.x)["log_prod_1995"] else NA_real_)  # Check if reg_prod is not NULL
  ) %>%
  select(iso2c, coef_log_gdp, coef_log_prod) %>%
  ungroup() %>%
  mutate(
    iso2c = ifelse(iso2c == "EL", "GR", iso2c), # change Greece to official ISO code
    iso2c = ifelse(iso2c == "UK", "GB", iso2c), # change UK to official ISO code
    countryname = countrycode::countrycode(iso2c, "iso2c", "country.name")
  )

# Lollipop chart function
lollipop_chart_function <- function(data, x_var, y_var, x_lab, y_lab, title) {
  toplot <- data %>%
    filter(!is.na({{y_var}}))

  chart <- ggplot(toplot, aes(x = reorder({{x_var}}, {{y_var}}), y = {{y_var}})) +
    geom_segment(aes(xend = reorder({{x_var}}, {{y_var}}), yend = 0, color = ifelse({{x_var}} == "Spain", "Spain", "Other")), size = 1, alpha = 0.3) +
    geom_point(aes(color = ifelse({{x_var}} == "Spain", "Spain", "Other")), size = 3) +
    scale_color_manual(values = c("Spain" = "red", "Other" = "navy")) +
    labs(
      x = x_lab,
      y = y_lab,
      title = title,
      caption = "@pablogguz_ | Source: ARDECO database and author's calculations"
    ) +
    theme_minimal(base_family = "Open Sans") +
    theme(
      text = element_text(family = "Open Sans", size = 11),
      plot.title = element_text(hjust = 0, size = 16),
      axis.title = element_text(size = 14),  
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11, color = ifelse(levels(reorder(data[[deparse(substitute(x_var))]], data[[deparse(substitute(y_var))]])) == "Spain", "red", "grey30")),
      plot.background = element_rect(fill = "#f2f2f2", color = NA),
      plot.caption = element_text(size = 9, hjust = 0, vjust = 0, colour = "#3C4043"),
      legend.position = "none"  # Remove the legend
    ) + coord_flip()
  
  ggsave(paste0(blog_path, "/convergence_ctries_", deparse(substitute(y_var)), ".png"), chart, dpi = 600, width = 9, height = 6)
  ggsave(paste0(output, "/convergence_ctries_", deparse(substitute(y_var)), ".png"), chart, dpi = 600, width = 9, height = 6)
  
  return(chart)
}

# Generate charts
lollipop_chart_function(
  convergence_countries, 
  countryname, 
  coef_log_gdp, 
  "", 
  "β-convergence coefficient value", 
  "Within-country β-convergence coefficients (1995-2023)"
)

lollipop_chart_function(
  convergence_countries, 
  countryname, 
  coef_log_prod, 
  "", 
  "β-convergence coefficient value", 
  "Within-country β-convergence coefficients for productivity (1995-2023)"
)

# Sigma-convergence 
eu15 <- c( # excl. Ireland
  "AT", "BE", "DE", "DK", "ES", "FI", "FR", "EL", "IT", "LU", "NL", "PT", "SE", "UK"
)

sigma_full <- data %>%
  filter(level == 3) %>% # NUTS 3
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
  # Keep countries with at least 5 NUTS3 regions
  group_by(iso2c, year) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  filter(between(as.numeric(year), 1995, 2023)) %>%
  mutate(
    spain = ifelse(iso2c == "ES", "Spain", "Other EU 15"),
  ) %>%
  filter( # keep only EU 27 countries
    iso2c %in% eu15
  ) %>%
  group_by(iso2c, year) %>%
  mutate(
    sd_log_gdp = sd(log(gdp_pc_eur_2015)),
    year = as.numeric(year)
  ) %>%
  ungroup() %>%
  group_by(spain, year) %>%
  summarize(
    sd_log_gdp = mean(sd_log_gdp, na.rm = TRUE)
  )

# Create line chart over time 
caption <- "@pablogguz_ | Source: ARDECO database and author's calculations. Other EU 15 refers to a simple cross-country average and includes Austria, Belgium, Denmark, Finland, France, Germany, Greece, Italy, Luxembourg, Netherlands, Portugal, Sweden, and the United Kingdom."

chart <- ggplot(sigma_full, aes(x = year, y = sd_log_gdp, color = spain)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("Spain" = "red", "Other EU 15" = "navy")) +
  labs(
    x = "Year",
    y = "Standard deviation of log GDP per capita",
    title = "Within-country σ-convergence (1995-2023)",
    caption = str_wrap(caption, width = 125),
    color = ""  # This sets the legend title to be blank
  ) +
  theme_minimal(base_family = "Open Sans") +
  theme(
    text = element_text(family = "Open Sans", size = 11),
    plot.title = element_text(hjust = 0, size = 16),
    axis.title = element_text(size = 14),  
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    plot.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.caption = element_text(size = 9, hjust = 0, vjust = 0, colour = "#3C4043"),
    legend.position = "top", 
    legend.title = element_blank(),
    legend.justification = c(0, 1),  
    legend.text = element_text(size = 13)
  ) +
  scale_x_continuous(breaks = seq(1995, 2023, 2))

chart

ggsave(paste0(blog_path, "/sigma.png"), chart, dpi = 600, width = 9, height = 6)
ggsave(paste0(output, "/sigma.png"), chart, dpi = 600, width = 9, height = 6)

# # Dumbbel chart
# sigma <- convergence %>%
#   group_by(iso2c) %>%
#   mutate(
#     sd_1995 = sd(log_gdp_1995),
#     sd_2023 = sd(log_gdp_2023)
#   ) %>%
#   filter(row_number()==1) %>%
#   ungroup() %>%
#   select(iso2c, sd_1995, sd_2023) %>%
#   mutate(
#     iso2c = ifelse(iso2c == "EL", "GR", iso2c), # change Greece to official ISO code
#     iso2c = ifelse(iso2c == "UK", "GB", iso2c), # change UK to official ISO code
#     countryname = countrycode::countrycode(iso2c, "iso2c", "country.name")
#   )

# chart <- ggplot(sigma) +
#   # Draw lines connecting 1995 and 2023 standard deviations
#   geom_segment(aes(x = sd_1995, xend = sd_2023, y = reorder(countryname, sd_2023), yend = reorder(countryname, sd_2023)),
#                color = "grey", size = 1) +
#   # Add points for 1995 values
#   geom_point(aes(x = sd_1995, y = reorder(countryname, sd_2023), color = "1995"), 
#              size = 3, alpha = 0.7) +
#   # Add points for 2023 values
#   geom_point(aes(x = sd_2023, y = reorder(countryname, sd_2023), color = "2023"), 
#              size = 3, alpha = 0.7) +
#   scale_color_manual(values = c("1995" = "navy", "2023" = "red")) +
#   labs(
#     x = "Standard deviation of log GDP per capita",
#     y = "",
#     title = "Within-country σ-convergence (1995-2023)",
#     caption = "@pablogguz_ | Source: ARDECO database and author's calculations",
#     color = ""  # This sets the legend title to be blank
#   ) +
#   theme_minimal(base_family = "Open Sans") +
#   theme(
#     text = element_text(family = "Open Sans", size = 11),
#     plot.title = element_text(hjust = 0, size = 16),
#     axis.title = element_text(size = 14),  
#     axis.text.y = element_text(size = 11, color = ifelse(levels(reorder(sigma$countryname, sigma$sd_2023)) == "Spain", "red", "grey30")),
#     axis.text.x = element_text(size = 11),
#     plot.background = element_rect(fill = "#f2f2f2", color = NA),
#     plot.caption = element_text(size = 9, hjust = 0, vjust = 0, colour = "#3C4043"),
#     legend.position = "top", 
#     legend.title = element_blank(),
#     legend.justification = c(0, 1),  
#     legend.text = element_text(size = 13)  
#   ) 

# ggsave(paste0(blog_path, "/sigma.png"), chart, dpi = 600, width = 9, height = 6)
# ggsave(paste0(output, "/sigma.png"), chart, dpi = 600, width = 9, height = 6)
