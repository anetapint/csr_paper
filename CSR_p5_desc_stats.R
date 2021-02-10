DATA_FOR_DESC_STATS <- csr_data_with_prim_sec

# Add log price and log assets
DATA_FOR_DESC_STATS <- DATA_FOR_DESC_STATS %>%
  dplyr::mutate("Log(Share price)" = log(Price),
                "Log(Assets)" = log(assets))

# Rename needed columns nicely
DATA_FOR_DESC_STATS <- DATA_FOR_DESC_STATS %>%
  dplyr::rename("Share price (USD)" = "Price",
                "NIPS (USD)" = "nips",
                "BVPS (USD)" = "bps",
                "LTDTA (%)" = "ltda",
                "Assets (USD mil.)" = "assets",
                "RDPS (USD)" = "rdps",
                "RDPS (USD), NAs as 0" = "rdps_zero",
                "RDPS (USD), NAs as ind. avg" = "rdps_ind_avg",
                "RDPS (USD), NAs as % sales" = "rdps_perc_rev",
                "TRESGC Score (0-100)" = "esgc_score",
                "Strategic CSR (0-100)" = "primary_csr_score",
                "Non-Strategic CSR (0-100)" = "secondary_csr_score")

# Columns we want for desc stats table
COLUMNS_FOR_DESC_STATS <- c("Share price (USD)", "NIPS (USD)", "BVPS (USD)", 
                            "LTDTA (%)", "Assets (USD mil.)", "RDPS (USD)", 
                            "RDPS (USD), NAs as 0",
                            "RDPS (USD), NAs as ind. avg",
                            "RDPS (USD), NAs as % sales",
                            "TRESGC Score (0-100)", "Strategic CSR (0-100)", 
                            "Non-Strategic CSR (0-100)") 

# Columns we want for corrplot
COLUMNS_FOR_CORR <- c("Log(Share price)", "NIPS (USD)", "BVPS (USD)", 
                            "LTDTA (%)", "Log(Assets)", 
                            "RDPS (USD) - NAs equal zero",
                            "RDPS (USD) - NAs as ind. avg",
                            "RDPS (USD) - NAs as % sales",
                            "TRESGC Score (0-100)", "Strategic CSR (0-100)", 
                            "Non-Strategic CSR (0-100)") 

# Get desc stats
summary(DATA_FOR_DESC_STATS[COLUMNS_FOR_DESC_STATS])

# Get just the columns for which we want the correlation plot
model_data_no_nas <- na.omit(DATA_FOR_DESC_STATS[COLUMNS_FOR_DESC_STATS])

# Plot correlations
corrplot(cor(model_data_no_nas))


