runModels <- function(csr_data_with_prim_sec, TIME_DUMMIES) {

  csr_data_with_prim_sec[, "Year"] <- as.factor(csr_data_with_prim_sec[, "Year"])
  
  # Create year dummies
  data_rnd_adj <- 
    data.frame(csr_data_with_prim_sec[ , ! colnames(csr_data_with_prim_sec) %in% "Year"],   
               model.matrix( ~ Year - 1, csr_data_with_prim_sec))
  
  ## MODELS WITH TOTAL CSR SCORE =================================================
  
  ## a) missing R&D equal to zero
          
  # Specify formula
  if (TIME_DUMMIES == "time_dummies") {
    
    formula_rnd_zero <- 
      as.formula(log(Price) ~ nips + bps + ltda + log(assets) + dplyr::lag(esgc_score, 1) +
                            rdps_zero + Year2008 + Year2009 + Year2010 +
                            Year2011 + Year2012 + Year2013+ Year2014 + Year2015 +           
                            Year2016 + Year2017 + Year2018 + Year2019 + Year2020)
    
  } else {
    
    formula_rnd_zero <- 
      as.formula(log(Price) ~ nips + bps + ltda + log(assets) + dplyr::lag(esgc_score, 1) + rdps_zero)
  }
  
  
  model_rnd_zero_full_csr <- plm(formula_rnd_zero, data = data_rnd_adj, 
                        index = c("Company", "Date"), model = "within")
  
  summary(model_rnd_zero_full_csr)
  
  # Run the model with robust SE
  model_rnd_zero_robust_full_csr <- coeftest(model_rnd_zero_full_csr, 
                                            vcov = vcovHC(model_rnd_zero_full_csr, 
                                                          method  = "arellano", 
                                                          cluster = "group")
                          )
  
  ## b) missing R&D equal to industry average or % of revenues
  
  # Specify formula
  if (TIME_DUMMIES == "time_dummies") {
    
    formula_rnd_ind_avg <- 
      as.formula(log(Price) ~ nips + bps + ltda + log(assets) + dplyr::lag(esgc_score, 1) 
                 + rdps_ind_avg + Year2007 + Year2008 + Year2009 + Year2010 +
                   Year2011 + Year2012 + Year2013+ Year2014 + Year2015 +           
                   Year2016 + Year2017 + Year2018 + Year2019 + Year2020)
    
  } else {
    
    formula_rnd_ind_avg <- 
      as.formula(log(Price) ~ nips + bps + ltda + log(assets) + dplyr::lag(esgc_score, 1) + rdps_ind_avg)
  }

  model_rnd_ind_avg_full_csr <- plm(formula_rnd_ind_avg, data = data_rnd_adj, 
                           index = c("Company", "Date"), model = "within")
  
  summary(model_rnd_ind_avg_full_csr)
  
  # Run the model with robust SE
  model_rnd_ind_avg_robust_full_csr <- coeftest(model_rnd_ind_avg_full_csr, 
                                                vcov = vcovHC(model_rnd_ind_avg_full_csr, 
                                                              method  = "arellano", 
                                                              cluster = "group")
  )
  
  ## c) missing R&D equal to % of revenues
  
  # Specify formula
  if (TIME_DUMMIES == "time_dummies") {
    
    formula_rnd_perc_rev <- 
      as.formula(log(Price) ~ nips + bps + ltda + log(assets) + dplyr::lag(esgc_score, 1) 
                 + rdps_perc_rev + Year2007 + Year2008 + Year2009 + Year2010 +
                   Year2011 + Year2012 + Year2013+ Year2014 + Year2015 +           
                   Year2016 + Year2017 + Year2018 + Year2019 + Year2020)
    
  } else {
    
    formula_rnd_perc_rev <- 
      as.formula(log(Price) ~ nips + bps + ltda + log(assets) + dplyr::lag(esgc_score, 1) + rdps_perc_rev)
  }

  model_rnd_perc_rev_full_csr <- plm(formula_rnd_perc_rev, data = data_rnd_adj, 
                                      index = c("Company", "Date"), model = "within")
  
  summary(model_rnd_perc_rev_full_csr)
  
  # Run the model with robust SE
  model_rnd_perc_rev_robust_full_csr <- coeftest(model_rnd_perc_rev_full_csr, 
                                                 vcov = vcovHC(model_rnd_perc_rev_full_csr, 
                                                               method  = "arellano", 
                                                               cluster = "group")
  )
  
  
  ## MODELS WITH PRIMARY vs. SECONDARY CSR SCORE =================================
  
  ## a) missing R&D equal to zero
  
  # Specify formula
  if (TIME_DUMMIES == "time_dummies") {
    
    formula_rnd_zero <- 
      as.formula(log(Price) ~ nips + bps + ltda + log(assets) + dplyr::lag(primary_csr_score, 1) + 
                              dplyr::lag(secondary_csr_score, 1) + rdps_zero + 
                              Year2008 + Year2009 + Year2010 + Year2011 + Year2012 + 
                              Year2013 + Year2014 + Year2015 + Year2016 + Year2017 + 
                              Year2018 + Year2019 + Year2020)
    
  } else {
    
    formula_rnd_zero <- 
      as.formula(log(Price) ~ nips + bps + ltda + log(assets) + dplyr::lag(primary_csr_score, 1) + 
                              dplyr::lag(secondary_csr_score, 1) + rdps_zero)
    
  }
  
  model_rnd_zero <- plm(formula_rnd_zero, data = data_rnd_adj, 
                        index = c("Company", "Date"), model = "within")
  
  summary(model_rnd_zero)
  
  # Run the model with robust SE
  model_rnd_zero_robust <- coeftest(model_rnd_zero, 
                                    vcov = vcovHC(model_rnd_zero, 
                                                  method  = "arellano", 
                                                  cluster = "group")
  )
  
  ## b) missing R&D equal to industry average or % of revenues
  
  # Specify formula
  if (TIME_DUMMIES == "time_dummies") {
    
    formula_rnd_ind_avg <- 
      as.formula(log(Price) ~ nips + bps + ltda + log(assets) + dplyr::lag(primary_csr_score, 1) + 
                   dplyr::lag(secondary_csr_score, 1) + rdps_ind_avg + 
                   Year2008 + Year2009 + Year2010 + Year2011 + Year2012 + 
                   Year2013 + Year2014 + Year2015 + Year2016 + Year2017 + 
                   Year2018 + Year2019 + Year2020)
    
  } else {
      
      formula_rnd_ind_avg <- 
        as.formula(log(Price) ~ nips + bps + ltda + log(assets) + dplyr::lag(primary_csr_score, 1) + 
                     dplyr::lag(secondary_csr_score, 1) + rdps_ind_avg)
      
  }
  
  model_rnd_ind_avg <- plm(formula_rnd_ind_avg, data = data_rnd_adj, 
                           index = c("Company", "Date"), model = "within")
  
  summary(model_rnd_ind_avg)
  
  # Run the model with robust SE
  model_rnd_ind_avg_robust <- coeftest(model_rnd_ind_avg, 
                                       vcov = vcovHC(model_rnd_ind_avg, 
                                                     method  = "arellano", 
                                                     cluster = "group")
  )
  
  ## c) missing R&D equal to % of revenues
  
  # Specify formula
  if (TIME_DUMMIES == "time_dummies") {
    
    formula_rnd_perc_rev <- 
      as.formula(log(Price) ~ nips + bps + ltda + log(assets) + dplyr::lag(primary_csr_score, 1) + 
                   dplyr::lag(secondary_csr_score, 1) + rdps_perc_rev + Year2007 + 
                   Year2008 + Year2009 + Year2010 + Year2011 + Year2012 + 
                   Year2013 + Year2014 + Year2015 + Year2016 + Year2017 + 
                   Year2018 + Year2019 + Year2020)
    
  } else {
    
    formula_rnd_perc_rev <- 
      as.formula(log(Price) ~ nips + bps + ltda + log(assets) + dplyr::lag(primary_csr_score, 1) + 
                   dplyr::lag(secondary_csr_score, 1) + rdps_perc_rev)
    
  }
  
  model_rnd_perc_rev <- plm(formula_rnd_perc_rev, data = data_rnd_adj, 
                            index = c("Company", "Date"), model = "within")
  
  summary(model_rnd_perc_rev)
  
  # Run the model with robust SE
  model_rnd_perc_rev_robust <- coeftest(model_rnd_perc_rev, 
                                        vcov = vcovHC(model_rnd_perc_rev, 
                                                      method  = "arellano", 
                                                      cluster = "group")
  )
  
  # Save all results
  all_results <- list(model_rnd_zero_robust_full_csr     = model_rnd_zero_robust_full_csr,
                      model_rnd_ind_avg_robust_full_csr  = model_rnd_ind_avg_robust_full_csr,
                      model_rnd_perc_rev_robust_full_csr = model_rnd_perc_rev_robust_full_csr,
                      model_rnd_zero_robust     = model_rnd_zero_robust,
                      model_rnd_ind_avg_robust  = model_rnd_ind_avg_robust,
                      model_rnd_perc_rev_robust = model_rnd_perc_rev_robust)
  
  return(all_results)

}