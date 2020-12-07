library(plm)
require(plm)
data=read.csv(file.choose(),header=TRUE,dec=".",sep=",")
name=data$Name
date=data$Date
BVPS=data$TangibleBookValuePerShare
NI=data$NetIncomeAfterTaxes
nshares=data$TotalCommonSharesOutstanding
NIPS=NI/nshares
assets=data$TotalAssets
lassets=log(assets)
revenues=data$Revenue
lrevenues=log(revenues) 
debt=data$TotalLongTermDebt
LTDTA=(debt/assets)*100 #Longterm debt to assets now reported as percentage
TRESGCscore=(data$TRESGCScore)*100 #now reported as percentage
shareprice=data$Shareprice
lshareprice=log(shareprice)
RDPS=data$RDPS
ncsr=data$ncsr
scsr=data$scsr
ssec=data$secondaryCSRstandardized
sprim=data$primaryCSRstandardized
#Model_a FE - size measured as log(assets)
modela=plm(lshareprice~NIPS+BVPS+LTDTA+lassets+ncsr+RDPS, data = data, index=c("Name","Date"), model = "within")
summary(modela)

#Model_a RE
modelaRE=plm(lshareprice~NIPS+BVPS+LTDTA+lassets+TRESGCscore+RDPS, data = data, index=c("Name","Date"), model = "random")
summary(modelaRE)

#Hausman test
phtest(modela,modelaRE)

#Model_c FE - size measured as log(revenues)
modelc=plm(lshareprice~NIPS+BVPS+LTDTA+lrevenues+TRESGCscore+RDPS, data = data, index=c("Name","Date"), model = "within")
summary(modelc)

#Testing for heteroskedasticity
bptest(lshareprice~NIPS+BVPS+LTDTA+lassets+TRESGCscore+RDPS, data = data, studentize=F)

#Testing serial correlation
#Wooldridge test
pwartest(lshareprice~NIPS+BVPS+LTDTA+lassets+TRESGCscore+RDPS, data = data)

#Correcting
library(lmtest)
coeftest(modela,vcov=vcovHC(modela,method="arellano",cluster="group"))
