dataJoin <- function(PERIOD_ADJUST = "no_period_adjust") {
  
  ### DATA LOAD ###
  
  financials = read.csv("Financials.csv", header = TRUE, stringsAsFactors = FALSE)
  sectors    = read.csv("Sectors.csv", header = TRUE, stringsAsFactors = FALSE)
  prices     = read.csv("Prices.csv", header = TRUE, stringsAsFactors = FALSE)
  esg_scores = read.csv("ESGScores.csv", header = TRUE, stringsAsFactors = FALSE)
  
  # Convert dates to date format
  financials[, "Date"] <- as.Date(financials[, "Date"], format = "%m/%d/%Y")
  prices[, "Date"]     <- as.Date(prices[, "Date"], format = "%m/%d/%Y")
  esg_scores[, "Date"] <- as.Date(esg_scores[, "Date"], format = "%m/%d/%Y")
  
  # Rename columns
  esg_scores <- esg_scores %>%
    dplyr::rename(
      res_use_score  = Resource.Use.Score,
      emis_score     = Emissions.Score,
      env_inno_score = Environmental.Innovation.Score,
      workf_score    = Workforce.Score,
      hum_r_score    = Human.Rights.Score,
      comm_score     = Community.Score,
      prod_score     = Product.Responsibility.Score,
      mgmt_score     = Management.Score,
      shar_score     = Shareholders.Score,
      csrs_score     = CSR.Strategy.Score
      )
  
  financials <- financials %>%
    dplyr::rename(
      debt       = Total.Long.Term.Debt,
      assets     = Total.Assets..Reported,
      bps        = Tangible.Book.Value.Per.Share,
      ni_bef_tax = Net.Income.Before.Taxes,
      ni_aft_tax = Net.Income.After.Taxes,
      shares_out = Total.Common.Shares.Outstanding,
      revenue    = Revenue,
      rnd        = Research.And.Development,
      roa        = ROA.Total.Assets..Percent
    )
  
  ### PRICES PRE-PROCESSING ###
  company_cols <- setdiff(colnames(prices), "Date")
  
  prices <- prices %>% 
    tidyr::gather(company_cols, key = "Company", value = "Price")
  
  
  ### SECTOR IDENTIFICATION ###
  
  # Rename needed columns
  names(sectors)[names(sectors) == "TRBC.Economic.Sector.Name"] <- "econ_sector"
  names(sectors)[names(sectors) == "TRBC.Business.Sector.Name"] <- "buss_sector"
  
  # Create sector (sub)group column 
  sectors <- sectors %>%
    dplyr::mutate(Sector = ifelse(!is.element(econ_sector, c("Consumer Cyclicals", 
                                                              "Technology", 
                                                              "Industrials")),
                                               econ_sector,
                                               buss_sector
                                              )
                 ) %>%
    dplyr::select(c("Company", "Sector", "econ_sector"))
    
  
  
  ### DATA CONCATENATION ###
  
  ## 1. Financials with ESG Scores ##
  
  # We will match based on year - add year column
  
  financials <- financials %>% 
    dplyr::mutate(Year = lubridate::year(financials[, "Date"]))
  
  esg_scores <- esg_scores %>% 
    dplyr::mutate(Year = lubridate::year(esg_scores[, "Date"]))
  
  # Sometimes we get ESG score for previous year only in January, February, March, or April
  # If this happens, move this score to the previous year
  esg_scores <- esg_scores %>%
    dplyr::mutate(Date = lubridate::as_date(ifelse(lubridate::month(Date) %in% c(1, 2, 3, 4),
                                            as.Date(paste0("12/31/", as.character(Year - 1)), format = "%m/%d/%Y"),
                                            Date
                                            )
                                     )
          )
  
  # Recalculate the year column after adjustment
  esg_scores <- esg_scores %>% 
    dplyr::mutate(Year = lubridate::year(esg_scores[, "Date"]))
  
  # Check duplicates in esg scores
  esg_duplicates <- esg_scores %>% 
    hablar::find_duplicates(Company, Date) %>%
    dplyr::select(Company, Date, everything())
  
  # --> we can see that we want just the first row from duplicates in our esg data
  esg_scores <- setdiff(esg_scores, esg_duplicates[2:4, ])
  
  # Join
  fin_esg <- financials %>%
    dplyr::left_join(esg_scores, by = c("Company", "Year")) %>%
    dplyr::mutate(Date = Date.x) %>%
    dplyr::select(-c("Date.x", "Date.y"))
  
  # Check duplicates in fin_esg
  fin_esg_duplicates <- fin_esg %>% 
    hablar::find_duplicates(Company, Date) %>%
    dplyr::select(Company, Date, everything())
  
  # Drop all we had in duplicates
  fin_esg <- fin_esg %>% dplyr::anti_join(fin_esg_duplicates)
  
  ## 2. Financials & ESG Scores with Prices ##
  
  # We will match based on quarter/year - add quarter/year column
  fin_esg <- fin_esg %>% 
    dplyr::mutate(Quarter_year = lubridate::quarter(as.POSIXlt(fin_esg[, "Date"]), with_year = TRUE))
  
  prices <- prices %>% 
    dplyr::mutate(Quarter_year = lubridate::quarter(prices[, "Date"], with_year = TRUE))
  
  # Join
  fin_esg_price <- fin_esg %>%
      dplyr::left_join(prices, by = c("Company", "Quarter_year")) %>%
      dplyr::mutate(Date = Date.x) %>%
      dplyr::select(-c("Date.x", "Date.y")) %>%
      dplyr::arrange(Company, Date)
  
  fin_esg_price_duplicates <- fin_esg_price %>% 
    hablar::find_duplicates(Company, Date) %>%
    dplyr::select(Company, Date, everything())
  # --> OK
  
  if (PERIOD_ADJUST == "prices_ahead_half_year") {
    
    # Create lead price
    fin_esg_price <- plyr::ddply( 
      fin_esg_price, .(Company), transform,
      # This assumes that the data is sorted
      Price = dplyr::lead(Price, 2) 
    )
    
  } else if (PERIOD_ADJUST == "yearly_data_prices_ahead_half_year") {
    
    # Create lead price
    fin_esg_price <- plyr::ddply( 
      fin_esg_price, .(Company), transform,
      # This assumes that the data is sorted
      Price = dplyr::lead(Price, 2) 
    )
    
    # Keep just year-end data
    fin_esg_price <- fin_esg_price %>%
      dplyr::filter(lubridate::month(Date) %in% c(1, 12))
    
  } else if (PERIOD_ADJUST == "prices_ahead_quarter_year") {
    
    # Create lead price
    fin_esg_price <- plyr::ddply( 
      fin_esg_price, .(Company), transform,
      # This assumes that the data is sorted
      Price = dplyr::lead(Price, 1) 
    )
    
  } else if (PERIOD_ADJUST == "yearly_data_prices_ahead_quarter_year") {
    
    # Create lead price
    fin_esg_price <- plyr::ddply( 
      fin_esg_price, .(Company), transform,
      # This assumes that the data is sorted
      Price = dplyr::lead(Price, 1) 
    )
    
    # Keep just year-end data
    fin_esg_price <- fin_esg_price %>%
      dplyr::filter(lubridate::month(Date) %in% c(1, 12))
    
  }
  
  
  # Check duplicates in fin_esg_price
  fin_esg_price_duplicates <- fin_esg_price %>% 
    hablar::find_duplicates(Company, Date) %>%
    dplyr::select(Company, Date, everything())
  # --> OK
  
  ## 3. Financials & ESG Scores & Prices with Sector -> full CSR data ##
  csr_data_joined <- fin_esg_price %>%
    dplyr::left_join(sectors, by = c("Company"))
  
  return(csr_data_joined)

}
