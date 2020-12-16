library(plm)
library(lmtest)

data <- read.csv(file.choose(),header=TRUE,dec=".",sep=",")

BVPS <- data$TangibleBookValuePerShare
NIPS <- data$NetIncomeAfterTaxes/data$TotalCommonSharesOutstanding
assets<-data$TotalAssets
lassets <- log(data$TotalAssets)
rev<-data$Revenue
lrevenues <- log(data$Revenue) 
LTDTA <- (data$TotalLongTermDebt/data$TotalAssets)*100
TRESGCscore <- (data$TRESGCScore)*100
lshareprice <- log(data$Shareprice)
RDPS <- data$RDPS

summary(TRESGCscore)
sd(LTDTA)
##### MODELS WITH TOTAL CSR SCORE #####

#Model_a FE - size measured as log(assets)
modela <- plm(lshareprice~NIPS+BVPS+LTDTA+lassets+TRESGCscore+RDPS, data = data, index=c("Name","Date"), model = "within")
summary(modela)

#Model_a RE
modelaRE <- plm(lshareprice~NIPS+BVPS+LTDTA+lassets+TRESGCscore+RDPS, data = data, index=c("Name","Date"), model = "random")
summary(modelaRE)

#Hausman test
phtest(modela,modelaRE)

#Model_c FE - size measured as log(revenues)
modelc <- plm(lshareprice~NIPS+BVPS+LTDTA+lrevenues+TRESGCscore+RDPS, data = data, index=c("Name","Date"), model = "within")
summary(modelc)

#Testing for heteroskedasticity
bptest(lshareprice~NIPS+BVPS+LTDTA+lassets+TRESGCscore+RDPS, data = data, studentize=F)

#Testing serial correlation
#Wooldridge test
pwartest(lshareprice~NIPS+BVPS+LTDTA+lassets+TRESGCscore+RDPS, data = data)

#Model with robust SE - size measured as log(assets)
coeftest(modela,vcov=vcovHC(modela,method="arellano",cluster="group"))

#Model with robust SE - size measured as log(revenues)
coeftest(modelc,vcov=vcovHC(modelc,method="arellano",cluster="group"))


##### MODELS WITH PRIMARY AND SECONDARY CSR (using only Fixed effects) #####

primCSR<-(data$PrimaryCSRscore)*100
secCSR<-(data$SecondaryCSRscore)*100


## 1. Including both Primary and Secondary CSR ##

## a) non-robust SE 

#Model_a  - size measured as log(assets)
model_prim_sec_a <- plm(lshareprice ~ NIPS + BVPS + LTDTA + lassets + primCSR + 
                        secCSR + RDPS, data = data, index=c("Name","Date"), model = "within")
summary(model_prim_sec_a)

#Model_c - size measured as log(revenues)
model_prim_sec_c <- plm(lshareprice ~ NIPS + BVPS + LTDTA + lrevenues + primCSR + 
                        secCSR + RDPS, data = data, index=c("Name","Date"), model = "within")
summary(model_prim_sec_c)


## b) robust SE 

#Model_a  - size measured as log(assets)
coeftest(model_prim_sec_a, vcov = vcovHC(model_prim_sec_a, method="arellano", cluster="group"))

#Model_c - size measured as log(revenues)
coeftest(model_prim_sec_c, vcov = vcovHC(model_prim_sec_c, method="arellano", cluster="group"))


## 2. Including Primary and Secondary CSR separately ##

## a) non-robust SE 
# Asi neni potrebne??

## b) robust SE 

#Model_a  - size measured as log(assets)
model_prim_a <- plm(lshareprice ~ NIPS + BVPS + LTDTA + lassets + primCSR + 
                    + RDPS, data = data, index=c("Name","Date"), model = "within")
coeftest(model_prim_a, vcov = vcovHC(model_prim_a, method="arellano", cluster="group"))

model_sec_a <- plm(lshareprice ~ NIPS + BVPS + LTDTA + lassets + secCSR + 
                    + RDPS, data = data, index=c("Name","Date"), model = "within")
coeftest(model_sec_a, vcov = vcovHC(model_sec_a, method="arellano", cluster="group"))


#Model_c - size measured as log(revenues)
model_prim_c <- plm(lshareprice ~ NIPS + BVPS + LTDTA + lrevenues + primCSR + 
                    + RDPS, data = data, index=c("Name","Date"), model = "within")
coeftest(model_prim_c, vcov = vcovHC(model_prim_c, method="arellano", cluster="group"))

model_sec_c <- plm(lshareprice ~ NIPS + BVPS + LTDTA + lrevenues + secCSR + 
                    + RDPS, data = data, index=c("Name","Date"), model = "within")
coeftest(model_sec_c, vcov = vcovHC(model_sec_c, method="arellano", cluster="group"))


##Latex export code:
#stargazer(model, title=" ", align=TRUE)