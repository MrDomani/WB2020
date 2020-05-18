check.table(ecopath_guinee)
# default choice, constant sigmaLN
create.smooth(ecopath_guinee)
# sigmaLN = smooth_param*ln(TL-0.05)
create.smooth(ecopath_guinee, smooth_type = 2)
plot(create.smooth(ecopath_guinee))
A <- create.smooth(ecopath_guinee)
# Transpose of the biomass column
T_biomass <- Transpose(A, ecopath_guinee, "biomass")
# Transpose of the catch.1 column
Transpose(A, ecopath_guinee, "catch.1")
# title and log scale for the biomass
plot(T_biomass, title = "biomass", log)
# constant sigmaLN
create.ETmain(ecopath_guinee)
# sigmaLN = smooth_param*ln(TL-0.05)
create.ETmain(ecopath_guinee, smooth_type = 2)
# constant sigmaLN
ET_Main <- create.ETmain(ecopath_guinee)$ET_Main
ET_Main
names(create.ETmain(ecopath_guinee))
# [1] "ET_Main" "biomass" "biomass_acc" "prod" "prod_acc" "tab_smooth" "Y"
plot(create.ETmain(ecopath_guinee),log)
ETmain <- create.ETmain(ecopath_guinee)
create.ETdiagnosis(ETmain)
# change of the top-down parameter
create.ETdiagnosis(ETmain, TopD = 0.6)
# log scale for the BTS
diag <- create.ETdiagnosis(ETmain)
plot(diag, log)
