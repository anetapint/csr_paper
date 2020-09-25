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

# Clean the data
csr_data_rnd_zero <- csr_data_rnd_zero %>%
  tidyr::drop_na()

## 2. Industry Average ##
csr_data_rnd_ind_avg <- csr_data

#TODO

# Clean the data
csr_data_rnd_ind_avg <- csr_data_rnd_ind_avg %>%
  tidyr::drop_na()

## 3. % of Revenue ##
csr_data_rnd_perc_rev <- csr_data

#TODO

# Clean the data
csr_data_rnd_perc_rev <- csr_data_rnd_perc_rev %>%
  tidyr::drop_na()

### CLEAN INTERMEDIATE RESULTS ###
rm(csr_data_joined, cols_names)
