### VARIABLE CLASSES AND CALCULATED COLUMNS ###

# Convert to numeric
cols_names <- setdiff(colnames(csr_data_joined), c("Date", "Price", "Company", 
                                                   "Sector", "Year", 
                                                   "Quarter_year"))

csr_data_joined[cols_names] <- sapply(csr_data_joined[cols_names], as.numeric)

# Add columns that we will need for our model
csr_data <- csr_data_joined %>%
  dplyr::mutate(
    nips = ni_aft_tax/shares_out, # net income per share (after tax)
    ltda = debt/assets,           # long-term debt to assets
    rdps = rnd/shares_out         # research and development per share
  )


### DATA CLEAN - PERIOD TILL 2016 AND FULL PERIOD ###

# Remove rows with NAs - because of RnD we lose significant portion of data
csr_data_old <- csr_data %>% 
  dplyr::filter(Year < 2017) %>%
  tidyr::drop_na()

csr_data_full <- csr_data %>%
  tidyr::drop_na()


### RESEARCH AND DEVELOPMENT DATA SOLUTION ###

### In the full dataset, try to replace RnD with: ###

## 1. Zeros ##
csr_data_rnd_zero <- csr_data

csr_data_rnd_zero$rnd[is.na(csr_data_rnd_zero$rnd)] <- 0

# Adjust RDPS
csr_data_rnd_zero <- csr_data_rnd_zero %>%
  dplyr::mutate(rdps = rnd/shares_out)

# Clean the data
csr_data_rnd_zero <- csr_data_rnd_zero %>%
  tidyr::drop_na()

## 2. Industry Average ##
csr_data_rnd_ind_avg <- csr_data





# Join data with industry averages
csr_data_rnd_ind_avg <- csr_data_rnd_ind_avg %>%
  dplyr::left_join(industry_averages, by = "Sector")

# Fill RnD NAs: if there are enough companies in given sector, then industry average,
# otherwise 5% of revenue
csr_data_rnd_ind_avg <- csr_data_rnd_ind_avg %>%
  dplyr::mutate(rnd2 = ifelse(!is.na(rnd), 
                             rnd,
                             ifelse((Sector %in% company_counts_above_2),
                                    rnd_avg,
                                    (0.5 * revenue))))

# Adjust RDPS
csr_data_rnd_ind_avg <- csr_data_rnd_ind_avg %>%
  dplyr::mutate(rdps = rnd2/shares_out)

# Clean the data
csr_data_rnd_ind_avg <- csr_data_rnd_ind_avg %>%
  tidyr::drop_na()

## 3. % of Revenue ##
csr_data_rnd_perc_rev <- csr_data

# When rnd is zero, use 0.5% of sales
csr_data_rnd_perc_rev <- csr_data_rnd_perc_rev %>%
  dplyr::mutate(rnd2 = ifelse(is.na(rnd), (0.005 * revenue), rnd))

# Adjust RDPS
csr_data_rnd_perc_rev <- csr_data_rnd_perc_rev %>%
  dplyr::mutate(rdps = rnd2/shares_out)

# Clean the data
csr_data_rnd_perc_rev <- csr_data_rnd_perc_rev %>%
  tidyr::drop_na()

################################
# Adjusted RnD dataset
csr_data_rnd_adj <- csr_data

# Calculate industry averages per sector
industry_averages <- csr_data %>%
  group_by(Sector) %>%
  summarise(rnd_avg = mean(rnd, na.rm = TRUE))

# Find how many companies stay in sector after NAs removal
company_counts <- csr_data_full %>%
  group_by(Sector) %>%
  summarise(count = length(unique(Company)))

# Find sectors with more than 2 companies
company_counts_above_2 <- company_counts %>%
  filter(count > 2) %>%
  select(Sector) 

# Convert to a vector
company_counts_above_2 <- company_counts_above_2[[1]]

# Join data with industry averages
csr_data_rnd_adj <- csr_data_rnd_adj %>%
  dplyr::left_join(industry_averages, by = "Sector")

csr_data_rnd_adj <- csr_data_rnd_adj %>%
  dplyr::mutate(rnd_zero = ifelse(!is.na(rnd), rnd, 0
                                  ),
                rnd_ind_avg = ifelse(!is.na(rnd), 
                                     rnd,
                                     ifelse((Sector %in% company_counts_above_2),
                                            rnd_avg,
                                            "Hello"
                                            )
                                      ),
                rnd_perc_rev = ifelse(is.na(rnd), (0.005 * revenue), rnd)
                )


# Adjust RDPS
csr_data_rnd_zero <- csr_data_rnd_zero %>%
  dplyr::mutate(rdps = rnd/shares_out)

### CLEAN INTERMEDIATE RESULTS ###
rm(csr_data_joined, cols_names)
