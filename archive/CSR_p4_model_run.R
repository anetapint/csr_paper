library(plm)
library(lmtest)

# Clean environment
rm(list=ls())

# Turn off scientific notation
options(scipen = 999)

# Load data
data_old_period  <- read.csv("edited_data/csr_data_old_period.csv")
data_full_period <- read.csv("edited_data/csr_data_full_period.csv")
data_rnd_adj     <- read.csv("edited_data/csr_data_rnd_adj.csv")

# Load old data we had previously
previous_data <- read.csv("old_data/previous_data.csv")

##### COMPARE PREVIOUS AND CURRENT DATA ####
data_diff1  <- anti_join(data_old_period, previous_data, 
                         by = c("Company" = "Name", "Date" = "Date")
                         )

data_diff2 <- anti_join(previous_data, data_old_period, 
                        by = c("Name" = "Company", "Date" = "Date")
                        )


##### MODELS WITH TOTAL CSR SCORE #####

## A. Full period dataset (R&D without adjustments) ##

# Specify formula
formula <- as.formula(log(Price) ~ nips + bps + ltda + log(assets) + esgc_score + rdps)

# Run the model
model_full_period <- plm(formula, data = data_full_period, 
                         index = c("Company", "Date"), model = "within")

summary(model_full_period)

# Run the model with robust SE
model_full_period_robust <- coeftest(model_full_period, 
                                     vcov = vcovHC(model_full_period, 
                                                   method  = "arellano", 
                                                   cluster = "group")
                                     )

## B. Old period dataset (R&D without adjustments) ##

# Run the model
model_old_period <- plm(formula, data = data_old_period, 
                         index = c("Company", "Date"), model = "within")

summary(model_old_period)

# Run the model with robust SE
model_old_period_robust <- coeftest(model_old_period,
                                    vcov = vcovHC(model_old_period, 
                                                  method  = "arellano", 
                                                  cluster = "group")
                          )

## C. Dataset with R&D adjustments

## a) missing R&D equal to zero
        
# Specify formula
formula_rnd_zero <- as.formula(log(Price) ~ nips + bps + ltda + log(assets) + esgc_score + rdps_zero)

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
formula_rnd_ind_avg <- as.formula(log(Price) ~ nips + bps + ltda + log(assets) + esgc_score + rdps_ind_avg)

model_rnd_ind_avg <- plm(formula_rnd_ind_avg, data = data_rnd_adj, 
                         index = c("Company", "Date"), model = "within")

summary(model_rnd_ind_avg)

# Run the model with robust SE
model_rnd_ind_avg_robust <- coeftest(model_rnd_ind_avg, 
                                    vcov = vcovHC(model_rnd_ind_avg, 
                                                  method  = "arellano", 
                                                  cluster = "group")
)

## b) missing R&D equal to % of revenues

# Specify formula
formula_rnd_perc_rev <- as.formula(log(Price) ~ nips + bps + ltda + log(assets) + esgc_score + rdps_perc_rev)

model_rnd_perc_rev <- plm(formula_rnd_perc_rev, data = data_rnd_adj, 
                         index = c("Company", "Date"), model = "within")

summary(model_rnd_perc_rev)

# Run the model with robust SE
model_rnd_perc_rev_robust <- coeftest(model_rnd_perc_rev, 
                                      vcov = vcovHC(model_rnd_perc_rev, 
                                                    method  = "arellano", 
                                                    cluster = "group")
)

# Save all results for models where CSR is not split to primary and secondary
all_results_overall_csr <- list(model_full_period_robust = model_full_period_robust,
                                model_old_period_robust = model_old_period_robust,
                                model_rnd_zero_robust = model_rnd_zero_robust,
                                model_rnd_ind_avg_robust = model_rnd_ind_avg_robust,
                                model_rnd_perc_rev_robust = model_rnd_perc_rev_robust)


#######################################################################################


##### MODELS WITH PRIMARY vs. SECONDARY CSR SCORE #####

## A. Full period dataset (R&D without adjustments) ##

# Specify formula
formula <- as.formula(log(Price) ~ nips + bps + ltda + log(assets) + primary_csr_score + 
                                   secondary_csr_score + rdps)

# Run the model
model_full_period <- plm(formula, data = data_full_period, 
                         index = c("Company", "Date"), model = "within")

summary(model_full_period)

# Run the model with robust SE
model_full_period_robust <- coeftest(model_full_period, 
                                     vcov = vcovHC(model_full_period, 
                                                   method  = "arellano", 
                                                   cluster = "group")
)

## B. Old period dataset (R&D without adjustments) ##

# Run the model
model_old_period <- plm(formula, data = data_old_period, 
                        index = c("Company", "Date"), model = "within")

summary(model_old_period)

# Run the model with robust SE
model_old_period_robust <- coeftest(model_old_period,
                                    vcov = vcovHC(model_old_period, 
                                                  method  = "arellano", 
                                                  cluster = "group")
)

## C. Dataset with R&D adjustments

## a) missing R&D equal to zero

# Specify formula
formula_rnd_zero <- as.formula(log(Price) ~ nips + bps + ltda + log(assets) + primary_csr_score + 
                                            secondary_csr_score + rdps_zero)

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
formula_rnd_ind_avg <- as.formula(log(Price) ~ nips + bps + ltda + log(assets) + 
                                               primary_csr_score + secondary_csr_score + 
                                               rdps_ind_avg)

model_rnd_ind_avg <- plm(formula_rnd_ind_avg, data = data_rnd_adj, 
                         index = c("Company", "Date"), model = "within")

summary(model_rnd_ind_avg)

# Run the model with robust SE
model_rnd_ind_avg_robust <- coeftest(model_rnd_ind_avg, 
                                     vcov = vcovHC(model_rnd_ind_avg, 
                                                   method  = "arellano", 
                                                   cluster = "group")
)

## b) missing R&D equal to % of revenues

# Specify formula
formula_rnd_perc_rev <- as.formula(log(Price) ~ nips + bps + ltda + log(assets) +
                                                primary_csr_score + 
                                                secondary_csr_score + 
                                                rdps_perc_rev)

model_rnd_perc_rev <- plm(formula_rnd_perc_rev, data = data_rnd_adj, 
                          index = c("Company", "Date"), model = "within")

summary(model_rnd_perc_rev)

# Run the model with robust SE
model_rnd_perc_rev_robust <- coeftest(model_rnd_perc_rev, 
                                      vcov = vcovHC(model_rnd_perc_rev, 
                                                    method  = "arellano", 
                                                    cluster = "group")
)

# Save all results for models where CSR is not split to primary and secondary
all_results_prim_sec_csr <- list(model_full_period_robust = model_full_period_robust,
                                 model_old_period_robust = model_old_period_robust,
                                 model_rnd_zero_robust = model_rnd_zero_robust,
                                 model_rnd_ind_avg_robust = model_rnd_ind_avg_robust,
                                 model_rnd_perc_rev_robust = model_rnd_perc_rev_robust)


# Render the markdown report
rmarkdown::render(
  input = "Models_results.rmd",
  output_file =
    paste(
      "CSR_Models_Results",
      paste0(Sys.Date(), ".html"),
      sep = "_"
    )
)