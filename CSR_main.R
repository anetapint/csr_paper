# Packages
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(hablar)
library(plm)
library(lmtest)
library(corrplot)
library(stargazer)
library(car)

# Source files
source("CSR_p1_data_join.R")
source("CSR_p2_data_prep.R")
source("CSR_p3_primary_secondary.R")
source("CSR_p4_model_run.R")

# Turn off scientific notation
options(scipen = 999)

# SET-UP =======================================================================

# Specify which period shift should be used
PERIOD_ADJUST <- "yearly_data_prices_ahead_half_year"
# Options:
# "no_period_adjust", "yearly_data_no_period_adjust", "prices_ahead_half_year", 
# "yearly_data_prices_ahead_half_year", "prices_ahead_quarter_year",
#  "yearly_data_prices_ahead_quarter_year"

# Specify the data we want to work with
DATA_TO_USE <- "csr_data_rnd_adj"

# Specify which ESG matrix we want to use
SECTOR_MATRIX <- "materiality_matrix"
# "original_matrix", "materiality_matrix", "random_matrix"

# Set random seed for random matrix generation (if random matrix is not used, this number can be anything)
SET_SEED <- 2021

# Specify if we want to use time fixed effects
TIME_DUMMIES <- "no_time_dummies"      # "time_dummies", "no_time_dummies", "trend_var"

# RUN ANALYSIS =================================================================

# Run data joining
csr_data_joined <- dataJoin(PERIOD_ADJUST = PERIOD_ADJUST)

# Run data preparation (returns list with three types of prepared data - full, old, with adjusted rnd)
prepared_data <- dataPrep(csr_data_joined = csr_data_joined)

# Retrieve selected dataset from the list
prepared_data_selected <- prepared_data[[DATA_TO_USE]]

# Calculate primary and secondary csr scores for selected data
csr_data_with_prim_sec <- primSec(data_prim_sec = prepared_data_selected,
                                  SECTOR_MATRIX = SECTOR_MATRIX,
                                  SET_SEED = SET_SEED)

###### GO TO CSR_p4_model_run_with_outputs script and run it step by step ######



# Run models
# all_results <- runModels(csr_data_with_prim_sec = csr_data_with_prim_sec,
#                           TIME_DUMMIES = TIME_DUMMIES)
# 
# 
# ## Report with results 
# 
# # Render the markdown report
# rmarkdown::render(
#   input = "Models_results.rmd",
#   output_file =
#     paste(
#       "Results_new", PERIOD_ADJUST, SECTOR_MATRIX, TIME_DUMMIES,
#       paste0(Sys.Date(), ".html"),
#       sep = "_"
#     )
# )
