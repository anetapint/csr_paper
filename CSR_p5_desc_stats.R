DATA_FOR_DESC_STATS <- csr_data_with_prim_sec

DATA_FOR_DESC_STATS <- DATA_FOR_DESC_STATS %>%
  dplyr::rename("Share price (USD)" = "Price",
                "NIPS (USD)" = "nips",
                "BVPS (USD)" = "bps",
                "LTDTA (%)" = "ltda",
                "Assets (USD mil.)" = "assets",
                "RDPS (USD)" = "rdps_ind_avg",
                "TRESGC Score (0-100)" = "esgc_score",
                "Strategic CSR (0-100)" = "primary_csr_score",
                "Non-Strategic CSR (0-100)" = "secondary_csr_score")

COLUMNS_FOR_DESC_STATS <- c("Share price (USD)", "NIPS (USD)", "BVPS (USD)", 
                            "LTDTA (%)", "Assets (USD mil.)", "RDPS (USD)", 
                            "TRESGC Score (0-100)", "Strategic CSR (0-100)", 
                            "Non-Strategic CSR (0-100)") 

summary(DATA_FOR_DESC_STATS[COLUMNS_FOR_DESC_STATS])

# Get just the columns for which we want the correlation plot
model_data_no_nas <- na.omit(DATA_FOR_DESC_STATS[COLUMNS_FOR_DESC_STATS])

# Plot correlations
corrplot(cor(model_data_no_nas))


