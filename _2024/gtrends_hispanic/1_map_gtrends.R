
#*******************************************************************************
#* Post: "Predicting socio-economic outcomes with Google Trends" https://pablogguz.github.io/blog/
#* 
#* Input: 
#* 
#* - Google Trends data on searches containing "bad bunny" for US states
#* - ACS data on % of hispanic population per state (microdata downloaded from IPUMS)
#* - Shapefiles for US state boundaries (2018, https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html)
#* 
#* Output:
#* 
#* - Some cute charts
#* 
#* Code by Pablo Garcia Guzman
#*******************************************************************************

packages_to_load <- c("tidyverse",
                      "haven",
                      "readxl",
                      "tmap",
                      "sf",
                      "extrafont",
                      "ggpubr",
                      "ggrepel") 

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

root <- paste0("C:/Users/", username, "/Documents/GitHub/blog_posts_code/_2024/gtrends_hispanic/")
input <- paste0(root, "/input/")
output <- paste0(root, "/output/")

blog_path <- paste0("C:/Users/", username, "/Documents/GitHub/pablogguz.github.io/static/img/")

path_data <- paste0(root, "data/")

# Script starts ----------------------------------------------------------------

# Read gtrends data ----
data <- read_xlsx(paste0(input, "gtrends_badbunny.xlsx")) %>%
  mutate(value = as.numeric(value)) %>%
  rename(NAME = state)

data_pesopluma <- read_xlsx(paste0(input, "gtrends_pesopluma.xlsx")) %>%
  mutate(value = as.numeric(value)) %>%
  rename(NAME = state,
         value_pesopluma = value)

data_anuelaa <- read_xlsx(paste0(input, "gtrends_anuelaa.xlsx")) %>%
  mutate(value = as.numeric(value)) %>%
  rename(NAME = state,
         value_anuelaa = value)

# Read shapefile ----
shape <- st_read(paste0(input, "cb_2018_us_state_20m.shp")) %>%
  left_join(data) %>%
  left_join(data_pesopluma) %>%
  left_join(data_anuelaa)

# leave out AK, HI, and PR (state FIPS: 02, 15, and 72)
shape <- shape[!(shape$STATEFP %in% c("02","15","72")),]  

## plot map ----
shape_df <- st_as_sf(shape)

map <- ggplot(data = shape_df) +
  geom_sf(aes(fill = value), color = "grey70", linewidth = .3) +
  scale_fill_distiller(palette = "Blues", na.value = "lightgray", 
                       name = stringr::str_wrap("Relative search interest", width = 40),
                       guide = guide_colourbar(title.position = "top",
                                               title.hjust = 0.5, barheight = 0.4,
                                               barwidth = 12),
                       direction = 1) +
  labs(
    title = "Google Trends search interest for 'Bad Bunny' across U.S. States",
    caption = "@pablogguz_ | Source: Google Trends"
  ) +
  theme_void() +
  theme(text = element_text(family = "Open Sans"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.background = element_rect(fill = "#f2f2f2", color = NA),
        legend.title.align = 0.5,
        plot.caption = element_text(size = 8, hjust = 0.01, vjust = 2, colour = "#3C4043")) +
  coord_sf(crs = "+init=epsg:2163")

map 

ggsave(paste0(output, "map_badbunny.png"), map, height = 6, width = 8, dpi = 200)
ggsave(paste0(blog_path, "map_badbunny.png"), map, height = 6, width = 8, dpi = 200)

# Load data on hispanic population shares ---- 
hispanic <- read_dta(paste0(output, "/hispanic_shares.dta")) %>%
  mutate(STATEFP = as.character(statefip), 
         STATEFP = ifelse(nchar(STATEFP) == 1, paste0("0", STATEFP), STATEFP)) %>%
  select(-statefip)

## plot maps ----

shape_df <- left_join(shape_df, hispanic)

map <- ggplot(data = shape_df) +
  geom_sf(aes(fill = hispanic), color = "grey70", linewidth = .3) +
  scale_fill_distiller(palette = "Blues", na.value = "lightgray", 
                       name = stringr::str_wrap("Population share (per cent)", width = 40),
                       guide = guide_colourbar(title.position = "top",
                                               title.hjust = 0.5, barheight = 0.4,
                                               barwidth = 12),
                       direction = 1) +
  labs(
    title = "Hispanic population share across U.S. states",
    caption = "@pablogguz_ | Source: 2022 ACS"
  ) +
  theme_void() +
  theme(text = element_text(family = "Open Sans"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.background = element_rect(fill = "#f2f2f2", color = NA),
        legend.title.align = 0.5,
        plot.caption = element_text(size = 8, hjust = 0.01, vjust = 2, colour = "#3C4043")) +
  coord_sf(crs = "+init=epsg:2163") 
 
map

ggsave(paste0(output, "map_hispanic.png"), map, height = 6, width = 8, dpi = 200)
ggsave(paste0(blog_path, "map_hispanic.png"), map, height = 6, width = 8, dpi = 200)

# Plot correlations ----

# tweak to omit some point labels
shape_df$label <- ifelse(rank(-shape_df$hispanic) <= 25, as.character(shape_df$NAME), NA)

corr <- ggplot(shape_df, aes(x = value, y = hispanic)) + 
  geom_point(color = "#6487cc", size = 3) +  
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "#6487cc", alpha = 0.6) +  
  geom_text_repel(aes(label = label), family = "Open Sans",
                  box.padding = 0.35, point.padding = 0.5) +
  labs(x = "Google Trends' relative search interest for 'Bad Bunny'",
       y = "Hispanic population share",
       title = "Predicting hispanic population shares in the U.S. with Google Trends",
       caption = "@pablogguz_ | Source: 2022 ACS and Google Trends") +
  theme_minimal() +
  theme(
    text = element_text(family = "Open Sans", size = 12),
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "#3C4043")
  ) +
  stat_cor(method = "pearson", aes(label = ..rr.label..),
           label.y = 0.15, label.x = 75, size = 5, color = "#6487cc", family = "Open Sans")

corr 

ggsave(paste0(output, "corr_badbunny_hispanic.png"), corr, height = 6, width = 9, dpi = 200)
ggsave(paste0(blog_path, "corr_badbunny_hispanic.png"), corr, height = 6, width = 9, dpi = 200)

corr <- ggplot(shape_df, aes(x = value_pesopluma, y = mexican)) + 
  geom_point(color = "#6487cc", size = 3) +  
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "#6487cc", alpha = 0.6) +  
  geom_text_repel(aes(label = label), family = "Open Sans",
                  box.padding = 0.35, point.padding = 0.5) +
  labs(x = "Google Trends' relative search interest for 'Peso Pluma'", 
       y = "Mexican population share",
       title = "Predicting mexican population shares in the U.S. with Google Trends",
       caption = "@pablogguz_ | Source: 2022 ACS and Google Trends") +
  theme_minimal() +
  theme(
    text = element_text(family = "Open Sans", size = 12),
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "#f2f2f2", color = NA),
    plot.caption = element_text(size = 8, hjust = 0, vjust = 0, colour = "#3C4043")
  ) +
  stat_cor(method = "pearson", aes(label = ..rr.label..),
           label.y = 0.15, label.x = 75, size = 5, color = "#6487cc", family = "Open Sans")
corr 

ggsave(paste0(output, "corr_pesopluma_mexican.png"), corr, height = 6, width = 9, dpi = 200)
ggsave(paste0(blog_path, "corr_pesopluma_mexican.png"), corr, height = 6, width = 9, dpi = 200)

