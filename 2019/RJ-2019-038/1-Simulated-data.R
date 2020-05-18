
## LOADING DATASET ##
data(CIBMTR)
data(CIBMTR_Params)

## SIMULATED DATA ##

# True values for parameters
beta1.true <- CIBMTR_Params$beta1.true
beta2.true <- CIBMTR_Params$beta2.true
beta3.true <- CIBMTR_Params$beta3.true
alpha1.true <- CIBMTR_Params$alpha1.true
alpha2.true <- CIBMTR_Params$alpha2.true
alpha3.true <- CIBMTR_Params$alpha3.true
kappa1.true <- CIBMTR_Params$kappa1.true
kappa2.true <- CIBMTR_Params$kappa2.true
kappa3.true <- CIBMTR_Params$kappa3.true
theta.true <- CIBMTR_Params$theta.true
cens <- c(365, 365)

# Sex (M: reference category)
CIBMTR$sexP <- as.numeric(CIBMTR$sexP)-1

# Age (LessThan10: reference category)
CIBMTR$ageP20to29 <- as.numeric(CIBMTR$ageP=="20to29")
CIBMTR$ageP30to39 <- as.numeric(CIBMTR$ageP=="30to39")
CIBMTR$ageP40to49 <- as.numeric(CIBMTR$ageP=="40to49")
CIBMTR$ageP50to59 <- as.numeric(CIBMTR$ageP=="50to59")
CIBMTR$ageP60plus <- as.numeric(CIBMTR$ageP=="60plus")

# Disease type (AML: reference category)
CIBMTR$dTypeALL <- as.numeric(CIBMTR$dType=="ALL")
CIBMTR$dTypeCML <- as.numeric(CIBMTR$dType=="CML")
CIBMTR$dTypeMDS <- as.numeric(CIBMTR$dType=="MDS")

# Disease status (Early: reference category)
CIBMTR$dStatusInt <- as.numeric(CIBMTR$dStatus=="Int")
CIBMTR$dStatusAdv <- as.numeric(CIBMTR$dStatus=="Adv")

# HLA compatibility (HLA_Id_Sib: reference category)
CIBMTR$donorGrp8_8 <- as.numeric(CIBMTR$donorGrp=="8_8")
CIBMTR$donorGrp7_8 <- as.numeric(CIBMTR$donorGrp=="7_8")

# Covariate matrix
x1 <- CIBMTR[,c("sexP","ageP20to29","ageP30to39","ageP40to49","ageP50to59","ageP60plus","dTypeALL","dTypeCML","dTypeMDS","dStatusInt","dStatusAdv","donorGrp8_8","donorGrp7_8")]
x2 <- CIBMTR[,c("sexP","ageP20to29","ageP30to39","ageP40to49","ageP50to59","ageP60plus","dTypeALL","dTypeCML","dTypeMDS","dStatusInt","dStatusAdv","donorGrp8_8","donorGrp7_8")]
x3 <- CIBMTR[,c("sexP","ageP20to29","ageP30to39","ageP40to49","ageP50to59","ageP60plus","dTypeALL","dTypeCML","dTypeMDS","dStatusInt","dStatusAdv","donorGrp8_8","donorGrp7_8")]

set.seed(1405)
simCIBMTR <- simID(id=NULL, x1, x2, x3,
          beta1.true, beta2.true, beta3.true,
          alpha1.true, alpha2.true, alpha3.true,
          kappa1.true, kappa2.true, kappa3.true,
          theta.true, SigmaV.true=NULL, cens)
names(simCIBMTR) <- c("time1","event1","time2","event2")
simCIBMTR <- cbind(simCIBMTR, CIBMTR[,c("sexP","ageP20to29","ageP30to39","ageP40to49","ageP50to59","ageP60plus","dTypeALL","dTypeCML","dTypeMDS","dStatusInt","dStatusAdv","donorGrp8_8","donorGrp7_8")])

print(head(simCIBMTR))

rm(list=ls()[!ls() %in% c("simCIBMTR")])

# Arguments to specify model-related
form <- Formula(time1 + event1 | time2 + event2 ~ 
          dTypeALL + dTypeCML + dTypeMDS + sexP | 
          dTypeALL + dTypeCML + dTypeMDS | 
          dTypeALL + dTypeCML + dTypeMDS)
