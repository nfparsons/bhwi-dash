# =============================================================================
# 1. SETUP/DEPENDENCIES
# =============================================================================

if (!require("pacman")) {
  install.packages("pacman", repos = "https://cran.rstudio.org")
}
library(pacman)

p_load(
  here,
  conflicted,
  rio,
  googledrive,
  googlesheets4,
  lubridate,
  stringr,
  tidyverse,
  janitor
)

# Conflict Resolution
conflicts_prefer(
  dplyr::filter,
  dplyr::first,
  dplyr::summarize,
  dplyr::select,
  janitor::clean_names,
  janitor::chisq.test,
  lubridate::year,
  rio::export,
  tidyselect::starts_with
)

# Global Options, Themes, and Table Settings
set.seed(13)
options(scipen = 9999)

# =============================================================================
# 2. GOOGLE SHEETS AUTHENTICATION
# =============================================================================

# This will open a browser window for you to log in.
# It will save a token so you only have to do this once.
googledrive::drive_auth()

# =============================================================================
# 3. DATA LOCATION
# =============================================================================

sheets_url <- "https://docs.google.com/spreadsheets/d/1lRMyvGBAXcUaQtmBxXDFyrs889L505OFRRyvC0zQJtg/edit?usp=drive_link"
local_file_path <- "data/_Funding Request (Responses) and Tracker(BH Workforce Initiative Funding Request Form).xlsx"

# =============================================================================
# 4. DATA DOWNLOAD
# =============================================================================

# We use drive_get() to locate the file by its URL.
# Then drive_download() to download it to your local file path.
# The `overwrite = TRUE` argument is important; it will replace the old file with the new one.
googledrive::drive_get(sheets_url) %>%
  googledrive::drive_download(path = local_file_path, overwrite = TRUE)

print(paste("File downloaded successfully to:", local_file_path))




