data_rnd_adj <- csr_data_with_prim_sec

## MODELS WITH TOTAL CSR SCORE ===============================================

## a) missing R&D equal to zero ==============================================

# Specify formula
formula_rnd_zero <- 
    as.formula(log(Price) ~ nips + bps + ltda + log(assets) + esgc_score + rdps_zero)


model_rnd_zero_full_csr <- plm(formula_rnd_zero, data = data_rnd_adj, 
                               index = c("Company", "Date"), model = "within")

summary(model_rnd_zero_full_csr)

# Run the model with robust SE
model_rnd_zero_robust_full_csr <- coeftest(model_rnd_zero_full_csr, 
                                           vcov = vcovHC(model_rnd_zero_full_csr, 
                                                         method  = "arellano", 
                                                         cluster = "group")
)

model_rnd_zero_robust_full_csr

stargazer(model_rnd_zero_robust_full_csr)

### Testing ###

# RE model
model_rnd_zero_full_csr_RE <- plm(formula_rnd_zero, data = data_rnd_adj, 
                         index = c("Company", "Date"), model = "random")


# Hausman test
phtest(model_rnd_zero_full_csr, model_rnd_zero_full_csr_RE)

# Testing for heteroskedasticity
bptest(formula_rnd_zero, data = data_rnd_adj, studentize = F)

# Testing serial correlation - Wooldridge test
pwartest(model_rnd_zero_full_csr)


## b) missing R&D equal to industry average or % of revenues =================

# Specify formula

formula_rnd_ind_avg <- 
    as.formula(log(Price) ~ nips + bps + ltda + log(assets) + esgc_score + rdps_ind_avg)


model_rnd_ind_avg_full_csr <- plm(formula_rnd_ind_avg, data = data_rnd_adj, 
                                  index = c("Company", "Date"), model = "within")

summary(model_rnd_ind_avg_full_csr)

# Run the model with robust SE
model_rnd_ind_avg_robust_full_csr <- coeftest(model_rnd_ind_avg_full_csr, 
                                              vcov = vcovHC(model_rnd_ind_avg_full_csr, 
                                                            method  = "arellano", 
                                                            cluster = "group")
)

model_rnd_ind_avg_robust_full_csr

stargazer(model_rnd_ind_avg_robust_full_csr)

### Testing ###

# RE model
model_rnd_ind_avg_full_csr_RE <- plm(formula_rnd_ind_avg, data = data_rnd_adj, 
                                        index = c("Company", "Date"), model = "random")


# Hausman test
phtest(model_rnd_ind_avg_full_csr, model_rnd_ind_avg_full_csr_RE)

# Testing for heteroskedasticity
bptest(formula_rnd_ind_avg, data = data_rnd_adj, studentize = F)

# Testing serial correlation - Wooldridge test
pwartest(model_rnd_ind_avg_full_csr)


## c) missing R&D equal to % of revenues =====================================

# Specify formula
formula_rnd_perc_rev <- 
    as.formula(log(Price) ~ nips + bps + ltda + log(assets) + esgc_score + rdps_perc_rev)

model_rnd_perc_rev_full_csr <- plm(formula_rnd_perc_rev, data = data_rnd_adj, 
                                   index = c("Company", "Date"), model = "within")

summary(model_rnd_perc_rev_full_csr)


# Run the model with robust SE
model_rnd_perc_rev_robust_full_csr <- coeftest(model_rnd_perc_rev_full_csr, 
                                               vcov = vcovHC(model_rnd_perc_rev_full_csr, 
                                                             method  = "arellano", 
                                                             cluster = "group")
)

model_rnd_perc_rev_robust_full_csr

stargazer(model_rnd_perc_rev_robust_full_csr)


### Testing ###

# RE model
model_rnd_perc_rev_full_csr_RE <- plm(formula_rnd_perc_rev, data = data_rnd_adj, 
                                     index = c("Company", "Date"), model = "random")


# Hausman test
phtest(model_rnd_perc_rev_full_csr_RE, model_rnd_perc_rev_full_csr)

# Testing for heteroskedasticity
bptest(formula_rnd_perc_rev, data = data_rnd_adj, studentize = F)

# Testing serial correlation - Wooldridge test
pwartest(model_rnd_perc_rev_full_csr)




## MODELS WITH PRIMARY vs. SECONDARY CSR SCORE =================================

## a) missing R&D equal to zero ==============================================

# Specify formula
formula_rnd_zero <- 
    as.formula(log(Price) ~ nips + bps + ltda + log(assets) + primary_csr_score + 
                 secondary_csr_score + rdps_zero)

model_rnd_zero <- plm(formula_rnd_zero, data = data_rnd_adj, 
                      index = c("Company", "Date"), model = "within")

summary(model_rnd_zero)

# Test the significance of CSR coefficients difference

# Get regression estimates, variances, and covariance
beta6_prim = summary(model_rnd_zero)$coefficients[5]
beta7_sec = summary(model_rnd_zero)$coefficients[6]

var_beta6_prim = vcov(model_rnd_zero)[5, 5]
var_beta7_sec  = vcov(model_rnd_zero)[6, 6]
cov_b6_b7 = vcov(model_rnd_zero)[5, 6]

# Calculate standard error of the difference
SE_b6_b7 = sqrt(var_beta6_prim + var_beta7_sec - 2 * cov_b6_b7)

# t-statistics
t_stats = (beta6_prim - beta7_sec)/SE_b6_b7

# test H0: beta6_prim - beta7_sec <= 0
pt(-t_stats, df=length(data_rnd_adj) - 1)

# Run the model with robust SE
model_rnd_zero_robust <- coeftest(model_rnd_zero, 
                                  vcov = vcovHC(model_rnd_zero, 
                                                method  = "arellano", 
                                                cluster = "group")
)

model_rnd_zero_robust

stargazer(model_rnd_zero_robust)

# Calculate VIF --> pooled model needed for that
model_rnd_zero_pooled <- plm(formula_rnd_zero, data = data_rnd_adj, 
                              index = c("Company", "Date"), model = "pooling")

vif(model_rnd_zero_pooled)


### Testing ###

# RE model
model_rnd_zero_RE <- plm(formula_rnd_zero, data = data_rnd_adj, 
                                  index = c("Company", "Date"), model = "random")


# Hausman test
phtest(model_rnd_zero, model_rnd_zero_RE)

# Testing for heteroskedasticity
bptest(formula_rnd_zero, data = data_rnd_adj, studentize = F)

# Testing serial correlation - Wooldridge test
pwartest(model_rnd_zero)



## b) missing R&D equal to industry average or % of revenues =================

# Specify formula
formula_rnd_ind_avg <- 
    as.formula(log(Price) ~ nips + bps + ltda + log(assets) + primary_csr_score + 
                 secondary_csr_score + rdps_ind_avg)
  

model_rnd_ind_avg <- plm(formula_rnd_ind_avg, data = data_rnd_adj, 
                         index = c("Company", "Date"), model = "within")

summary(model_rnd_ind_avg)

# Run the model with robust SE
model_rnd_ind_avg_robust <- coeftest(model_rnd_ind_avg, 
                                     vcov = vcovHC(model_rnd_ind_avg, 
                                                   method  = "arellano", 
                                                   cluster = "group")
)

model_rnd_ind_avg_robust

stargazer(model_rnd_ind_avg_robust)

# Calculate VIF --> pooled model needed for that
model_rnd_ind_avg_pooled <- plm(formula_rnd_ind_avg, data = data_rnd_adj, 
                         index = c("Company", "Date"), model = "pooling")

vif(model_rnd_ind_avg_pooled)

### Testing ###

# RE model
model_rnd_ind_avg_RE <- plm(formula_rnd_ind_avg, data = data_rnd_adj, 
                                     index = c("Company", "Date"), model = "random")


# Hausman test
phtest(model_rnd_ind_avg, model_rnd_ind_avg_RE)

# Testing for heteroskedasticity
bptest(formula_rnd_ind_avg, data = data_rnd_adj, studentize = F)

# Testing serial correlation - Wooldridge test
pwartest(model_rnd_ind_avg)




## c) missing R&D equal to % of revenues =====================================

# Specify formula
formula_rnd_perc_rev <- 
    as.formula(log(Price) ~ nips + bps + ltda + log(assets) + primary_csr_score + 
                 secondary_csr_score + rdps_perc_rev)


model_rnd_perc_rev <- plm(formula_rnd_perc_rev, data = data_rnd_adj, 
                          index = c("Company", "Date"), model = "within")

summary(model_rnd_perc_rev)

# Run the model with robust SE
model_rnd_perc_rev_robust <- coeftest(model_rnd_perc_rev, 
                                      vcov = vcovHC(model_rnd_perc_rev, 
                                                    method  = "arellano", 
                                                    cluster = "group")
)

model_rnd_perc_rev_robust

stargazer(model_rnd_perc_rev_robust)

# Calculate VIF --> pooled model needed for that
model_rnd_perc_rev_pooled <- plm(formula_rnd_perc_rev, data = data_rnd_adj, 
                          index = c("Company", "Date"), model = "pooling")

vif(model_rnd_perc_rev_pooled)

### Testing ###

# RE model
model_rnd_perc_rev_RE <- plm(formula_rnd_perc_rev, data = data_rnd_adj, 
                                      index = c("Company", "Date"), model = "random")


# Hausman test
phtest(model_rnd_perc_rev_RE, model_rnd_perc_rev)

# Testing for heteroskedasticity
bptest(formula_rnd_perc_rev, data = data_rnd_adj, studentize = F)

# Testing serial correlation - Wooldridge test
pwartest(model_rnd_perc_rev)
