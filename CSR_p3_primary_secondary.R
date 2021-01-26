primSec <- function(data_prim_sec, SECTOR_MATRIX) {
  
  ### DATA LOAD AND PREP ###
  
  # Load table indicating which CSR categories are primary and secondary per sector
  if (SECTOR_MATRIX == "original_matrix") {
    
    sectors_vs_csr = read.csv("SectorVsCSR.csv", header = TRUE, stringsAsFactors = FALSE)
    
  } else if (SECTOR_MATRIX == "materiality_matrix") {
    
    sectors_vs_csr = read.csv("SectorVsCSR_materiality.csv", header = TRUE, stringsAsFactors = FALSE)
    
  } else if (SECTOR_MATRIX == "random_matrix") {
    
    # Load original matrix
    sectors_vs_csr_orig = read.csv("SectorVsCSR.csv", header = TRUE, stringsAsFactors = FALSE)
    
    # Set random seed
    set.seed(2021)
    
    # Create dataframe with randomly generated 0s and 1s
    sectors_vs_csr_random <- data.frame(replicate(10, sample(0:1, 17, rep = TRUE)))
    
    # Name the columns of random dataframe as in original one
    colnames(sectors_vs_csr_random) <- colnames(sectors_vs_csr)[2:11]
    
    # Add sector names
    sectors_vs_csr <- cbind(sectors_vs_csr_orig[1], sectors_vs_csr_random)
    
  }
  
  # Join data with prim/sec categories
  data_prim_sec <- data_prim_sec %>%
    dplyr::left_join(sectors_vs_csr, by = c("Sector"))
  
  # Add weights per each CSR category as new columns in the data
  length <- nrow(data_prim_sec) 
  
  data_prim_sec$w_res  <- rep(0.11, length)
  data_prim_sec$w_emis <- rep(0.12, length)
  data_prim_sec$w_inno <- rep(0.11, length)
  data_prim_sec$w_work <- rep(0.16, length)
  data_prim_sec$w_hum  <- rep(0.045, length)
  data_prim_sec$w_comm <- rep(0.08, length)
  data_prim_sec$w_prod <- rep(0.07, length)
  data_prim_sec$w_mgmt <- rep(0.19, length)
  data_prim_sec$w_shar <- rep(0.07, length)
  data_prim_sec$w_csrs <- rep(0.045, length)
  
  ### PRIMARY CSR SCORE CALCULATION ###
  
  # Weights sum
  data_prim_sec <- data_prim_sec %>%
    dplyr::mutate(weights_sum = 
                    w_res  * Res + 
                    w_emis * Emis +
                    w_inno * Inno +
                    w_work * Work +
                    w_hum  * Hum +
                    w_comm * Comm +
                    w_prod * Prod +
                    w_mgmt * Mgmt +
                    w_shar * Shar +
                    w_csrs * Csrs
                    )
  
  # Calculate primary CSR score
  data_prim_sec <- data_prim_sec %>%
    dplyr::mutate(
      primary_csr_score = 
        w_res  * Res  * 1/weights_sum * res_use_score + 
        w_emis * Emis * 1/weights_sum * emis_score +
        w_inno * Inno * 1/weights_sum * env_inno_score +
        w_work * Work * 1/weights_sum * workf_score +
        w_hum  * Hum  * 1/weights_sum * hum_r_score +
        w_comm * Comm * 1/weights_sum * comm_score +
        w_prod * Prod * 1/weights_sum * prod_score +
        w_mgmt * Mgmt * 1/weights_sum * mgmt_score +
        w_shar * Shar * 1/weights_sum * shar_score +
        w_csrs * Csrs * 1/weights_sum * csrs_score
    )
  
  # Drop indicator columns
  data_prim_sec <- data_prim_sec %>%
    dplyr::select(-c("Res", "Emis", "Inno", "Work", "Hum", "Comm",
                     "Prod", "Mgmt", "Shar", "Csrs"))
  
  ### SECONDARY CSR SCORE CALCULATION ###
  
  # Reverse 1/0 indication for primary vs secondary 
  sectors_vs_csr[sectors_vs_csr == 1] <- 2
  sectors_vs_csr[sectors_vs_csr == 0] <- 1
  sectors_vs_csr[sectors_vs_csr == 2] <- 0
  
  # Join data with prim/sec categories
  data_prim_sec <- data_prim_sec %>%
    dplyr::left_join(sectors_vs_csr, by = c("Sector"))
  
  # Recalculate weights sum
  data_prim_sec <- data_prim_sec %>%
    dplyr::mutate(weights_sum_sec = 
                    w_res  * Res + 
                    w_emis * Emis +
                    w_inno * Inno +
                    w_work * Work +
                    w_hum  * Hum +
                    w_comm * Comm +
                    w_prod * Prod +
                    w_mgmt * Mgmt +
                    w_shar * Shar +
                    w_csrs * Csrs
    )
  
  # For secondary CSR score, use the same logic as for primary
  data_prim_sec <- data_prim_sec %>%
    dplyr::mutate(
      secondary_csr_score = 
        w_res  * Res  * 1/weights_sum_sec * res_use_score + 
        w_emis * Emis * 1/weights_sum_sec * emis_score +
        w_inno * Inno * 1/weights_sum_sec * env_inno_score +
        w_work * Work * 1/weights_sum_sec * workf_score +
        w_hum  * Hum  * 1/weights_sum_sec * hum_r_score +
        w_comm * Comm * 1/weights_sum_sec * comm_score +
        w_prod * Prod * 1/weights_sum_sec * prod_score +
        w_mgmt * Mgmt * 1/weights_sum_sec * mgmt_score +
        w_shar * Shar * 1/weights_sum_sec * shar_score +
        w_csrs * Csrs * 1/weights_sum_sec * csrs_score
    )
  
  # Drop indicator columns
  data_prim_sec <- data_prim_sec %>%
    dplyr::select(-c("Res", "Emis", "Inno", "Work", "Hum", "Comm",
                     "Prod", "Mgmt", "Shar", "Csrs"))
  
  # Drop weight columns
  data_prim_sec <- data_prim_sec %>%
    dplyr::select(-c("w_res", "w_emis", "w_inno", "w_work", "w_hum", "w_comm",
                     "w_prod", "w_mgmt", "w_shar", "w_csrs"))
  
  # Drop individual score columns
  data_prim_sec <- data_prim_sec %>%
    dplyr::select(-c("res_use_score", "emis_score", "env_inno_score", "workf_score",
                     "hum_r_score", "comm_score", "prod_score", "mgmt_score", 
                     "shar_score", "csrs_score"))
  
  # Rename overall score columns
  data_prim_sec <- data_prim_sec %>%
    dplyr::rename(esg_score = ESG.Score, 
                  esg_comb_score = ESG.Combined.Score,
                  esgc_score = ESG.Controversies.Score)
  
  return(data_prim_sec)
  ### SAVE THE DATASET ###
  #write.csv(data_prim_sec, paste0("edited_data/", DATA_TO_USE, ".csv"))
  
}
