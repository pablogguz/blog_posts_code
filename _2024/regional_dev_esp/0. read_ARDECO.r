#* ------------------------------------------------------------------------------
#* Post: "A tale of regional development in Spain"
#*
#* This script:
#* - Fetches, process and saves data from ARDECO for NUTS regions in Spain
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
    "fst"
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

# Fetch data for Spain 

# vec_vars <- ardeco_get_variable_list()$code # get vector of variables
vec_vars <- c("SOVGDP", "ROWCDH", "ROWCDW", "SOVGDH", "SOVGDE", "SNPTD", "SNPBN")

data_list <- map(
    vec_vars,
    ~ ardeco_get_dataset_data(
        variable = .x,
        version = 2021,
        level = '0,2,3,4,9'
    )
)

# Filter out non-data frame elements
data_list <- keep(data_list, is.data.frame)

# Reshape each data frame and merge them
reshaped_data_list <- map(data_list, function(df) {
    colnames(df) <- tolower(colnames(df))
    variable_name <- unique(df$variable)
    df <- df %>%
        select(-variable) %>%
        pivot_wider(names_from = unit, values_from = value, names_prefix = paste0(variable_name, "_"))
    return(df)
})

# Combine the reshaped data frames by merging on common columns
final_data <- reduce(reshaped_data_list, full_join, by = c("versions", "level", "nutscode", "year"))

# Display the final combined dataset
View(final_data)

# Remove non-alphanumeric characters from column names
remove_nonalpha <- function(df) {
  colnames(df) <- colnames(df) %>%
    str_replace_all(" ", "") %>%  # Remove spaces
    str_replace_all("[^[:alnum:]_]", "")  # Remove non-alphanumeric characters except underscore
  return(df)
}

final_data <- final_data %>%
  remove_nonalpha()

# Save
write.fst(final_data, paste0(output, "ardeco_data.fst"))
