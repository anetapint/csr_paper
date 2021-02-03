dataPrep <- function(csr_data_joined) {
  
  ### VARIABLE CLASSES AND CALCULATED COLUMNS ###
  
  # Convert to numeric
  cols_names <- setdiff(colnames(csr_data_joined), c("Date", "Price", "Company", 
                                                     "Sector", "econ_sector", 
                                                     "Year", "Quarter_year"))
  
  csr_data_joined[cols_names] <- sapply(csr_data_joined[cols_names], as.numeric)
  
  # Set negative rnd as NA
  csr_data_joined[csr_data_joined$rnd < 0 & !is.na(csr_data_joined$rnd), ]$rnd <- NA
  
  # Add columns that we will need for our model
  csr_data <- csr_data_joined %>%
    dplyr::mutate(
      nips = ni_aft_tax/shares_out, # net income per share (after tax)
      ltda = debt/assets,           # long-term debt to assets
      rdps = rnd/shares_out         # research and development per share
    )
  
  
  ### DATA CLEAN - PERIOD TILL 2016 AND FULL PERIOD ###
  
  # Remove rows with NAs - because of RnD we lose significant portion of data
  csr_data_old_period <- csr_data %>% 
    dplyr::filter(Year < 2017) %>%
    tidyr::drop_na()
  
  csr_data_full_period <- csr_data %>%
    tidyr::drop_na()
  
  
  ### RESEARCH AND DEVELOPMENT DATA SOLUTION ###
  
  ### In the full dataset, try to replace RnD with zeros/ industry averages/% of sales ###
  csr_data_rnd_adj <- csr_data
  
  # Calculate industry averages per sector
  industry_averages <- csr_data %>%
    dplyr::group_by(Sector) %>%
    dplyr::summarise(rnd_avg = mean(rnd, na.rm = TRUE))
  
  # Find how many companies stay in sector after NAs removal
  company_counts <- csr_data_full_period %>%
    dplyr::group_by(Sector) %>%
    dplyr::summarise(count = length(unique(Company)))
  
  # Find sectors with more than 2 companies with RnD data
  company_counts_above_2 <- company_counts %>%
    dplyr::filter(count > 2) %>%
    dplyr::select(Sector) 
  
  # Convert to a vector
  company_counts_above_2 <- company_counts_above_2[[1]]
  
  # Join data with industry averages
  csr_data_rnd_adj <- csr_data_rnd_adj %>%
    dplyr::left_join(industry_averages, by = "Sector")
  
  # Intermediate column for industry average/perc revenue
  csr_data_rnd_adj <- csr_data_rnd_adj %>%
    dplyr::mutate(rnd_ind_avg = ifelse((Sector %in% company_counts_above_2),
                                       rnd_avg,
                                       (0.005 * revenue)
                                       )
    )
  
  csr_data_rnd_adj <- csr_data_rnd_adj %>%
    dplyr::mutate(rnd_zero = ifelse(!is.na(rnd), rnd, 0
                                    ),
                  rnd_ind_avg = ifelse(!is.na(rnd), 
                                       rnd,
                                       rnd_ind_avg
                                      ),
                  rnd_perc_rev = ifelse(is.na(rnd), (0.005 * revenue), rnd)
                  )
  
  # Adjust RDPS
  csr_data_rnd_adj <- csr_data_rnd_adj %>%
    dplyr::mutate(rdps_zero     = rnd_zero/shares_out,
                  rdps_ind_avg  = rnd_ind_avg/shares_out,
                  rdps_perc_rev = rnd_perc_rev/shares_out)
  
  return(list(
    csr_data_old_period  = csr_data_old_period,
    csr_data_full_period = csr_data_full_period,
    csr_data_rnd_adj     = csr_data_rnd_adj
  ))
  
}
