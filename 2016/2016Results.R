# ------- 030 ---------
extract_code_from_html(files[1], overwrite = TRUE, console_char = "R>")

# 1) 'X not found'; 'data not found' - trzeba samemy zrobić referencje
# 2) samplestat not found
# 3) match.int must be 2*number of matching variables
# najwyraźniej coś zmienili z czasem
# fit nie działa
# summary(1/glmp[CVD_Accidents$samplestat == 1]) działa ok
# to samo z kolejnym summary
# Werdykt : 2

# ------- 031 ----------
# package QPot not available (for R 4.0.0 and 3.6.1)
# Werdykt : 0

# ------- 032 ----------
# package TRONCO not available (for R 4.0.0 and 3.6.1)
# Werdykt : 0

# ------- 033 ----------
# figure 1,2bc - brak kodu
# figure 2 akceptowalne
# str(*)- OK
# figure3 - OK
# variety(), ubiguity - OK
# round() - INNE WARTOŚCI
# Pozamieniana kolejność wierszy
# barplot(), plot() OK
# diversity() - OK
# round() - OK
# diversity(data = scidat_rca_fil, type = c('rao', 'rs') ,alpha=0.7, beta = 0.3, method = 'cosine') - unused arguments
# hist() - figure5a - OK
# package ‘firdistrplus’ is not available (for R version 4.0.0)
# head() - OK
# head(sim_ent) - INNE WARTOŚCI
# figure 5c - OK
# diversity(data_set, type=c("e")), diversity(data_set, type=c("rs")) - unused argument
# d_1 <- diversity(scidat, type="e")
# d_2 <- diversity(scidat_2, type="e")
# Werdykt: 1

# ------- 034 --------
extract_code_from_html(files[5], overwrite = TRUE, console_char = ">")

# strasznie długo zajmuje
# odpalę na niemcu :P

# ------- 035 --------
# package ‘eiCompare’ is not available (for R version 4.0.0 and 3.6.1)
# Werdykt: 0

# ------- 036 --------
extract_code_from_html(files[7], overwrite = TRUE, console_char = ">")
# package rgdal not installing

# ------- 037 --------
extract_code_from_html(files[8], overwrite = TRUE, console_char = ">")
# zmiana - mid-ECFD. Nic wielkiego
# figure 1 - OK
# plot(midquantile(y, probs = p), jumps = FALSE) - p not found
# kt <- KhmaladzeTest(formula = y ~ x, taus = seq(.05, .95, by = .01), - ) zamiast ,
# figure 2 - 50%
# figure 3 - brak source
# figure 4 - NOT OK
# figure 5 - OK
# system.time(fit.rqt <- rcrq(Ozone ~ Solar.R, data = dd, tsf = "mcjI",
# symm = TRUE, dbounded = FALSE, lambda = seq(1, 3, by = 0.005),
# tau = c(.1, .5, .9))) - czasy inne
# summary(fit.rqt, conditional = FALSE, se = "nid") - INNE WARTOŚCI
# coef(tsrq2(score ~ gcse, data = chemsub, dbounded = TRUE,
# lambda = seq(0, 2, by = 0.1), delta = seq(0, 2, by = 0.1),
# tau = 0.9), all = TRUE) - object chemsub not found
# figure 6 - no source
# figure 7 - OK
# Werdykt : 2
# ------- 038 --------

# out1 <- est_multi_poly_between(S, k = k0, link = "global", multi = rbind(1:3, 4:6)), - syntax
# object k0 not found
# długo się robi

# ------- 039 --------
extract_code_from_html(files[10], overwrite = TRUE, console_char = "R>")
# werdykt: 3

# ------- 040 --------
extract_code_from_html(files[11], overwrite = TRUE, console_char = ">")
# summary(post.lasso.reg, all=FALSE) - INNE WARTOŚCI
# print(lasso.effect) - INNE WARTOŚCI
# summary(lasso.effect) - INNE WARTOŚCI
# confint(lasso.effect) -   INNE WARTOŚCI
# confint(lasso.effect, joint = TRUE) - INNE WARTOŚCI
# summary(lasso.reg, all=FALSE) - INNE WARTOŚCI
# joint.CI - INNE WARTOŚCI
# summary(lasso.IV.Z) - INNE WARTOŚCI
# summary(lasso.IV.XZ) - INNE WARTOŚCI
# ...
# Werdykt: 1
# -------- 041 ---------
extract_code_from_html(files[12], overwrite = TRUE, console_char = ">")

# figure 2 - OK
#norm.ss(alpha = 0.05, P = 0.90, side = 2, spec = c(0.990, 1.010),
#method = "DIR", hyper.par = list(mu.0 = 1.004, sig2.0 = 0.001)) - n Inf zamiast 5. Niewielki
# norm.OC(k = 4, alpha = c(0.01, 0.05, 0.10), P = NULL, n = 10:20,
# side = 2, method = "EXACT", m = 25) - długo działa
# Werdykt: 3
# -------- 042 ---------
# na plus: działa
# na minus: nie mam danych
# -------- 043 ---------
# dependency ‘units’ is not available for package ‘sf’
# -------- 044 ---------
# object theta not found - trzeba pokombinować
# dużo wykresów, mało kodu
# Werdykt: 2

# -------- 045 ---------
extract_code_from_html(files[16], overwrite = TRUE, console_char = "R>")
# funkcje generują ploty - niejasne, czy to zamierzone
# figure 2 - ok
# Werdykt: 3
# -------- 046 ---------
extract_code_from_html(files[17], overwrite = TRUE, console_char = "")
# surv.ps <- psdesign(data = fakedata, Z = Z, Y = Surv(time.obs, event.obs), S = S.obs,
# BIP = BIP, CPV = CPV, BSM = BSM) - Surv() not found. W tekście: requires survival package.
# binary.ps + integrate_parametric(S.1 ~ BIP + BSM + BSM^2)
# binary.ps <- psdesign(data = fakedata, Z = Z, Y = Y.obs, S = S.obs, BIP = BIP,
#                       BSM = BSM, age = age)
# binary.ps + integrate_parametric(S.1 ~ BIP + age)
# binary.ps <- binary.ps + risk_binary(model = Y ~ S.1 * Z, D = 50, risk = risk.logit)
# binary.ps - not found BSM, age, ...
# Werdykt: 1
# -------- 047 ----------

extract_code_from_html(files[18], overwrite = TRUE, console_char = ">")
# represent_enrichment(enrichment = list(Corrected = results[1:6,],
#                                        Uncorrected = results_uncorrected[1:6,]),
#                      plot = "heatmap", scale = "reverselog",
#                      low = "steelblue" , high ="white", na.value = "grey") - not found results_uncorrdected
# data_matrix <- navicell$readDatatable('DU145_data.txt') - no such file or directory, ...
# packages not available
# Werdykt: 1
# -------- 048 -----------
extract_code_from_html(files[19], overwrite = TRUE, console_char = ">")
# assuming, that files are in working directory - they aren't
# werdykt: 0

# --------- 049 -----------
# ADCFplot(Residuals, MaxLag = 18, main = "Wild Bootstrap", method = "Wild") - unused argument method
# t=scale(lseries, center = TRUE, scale = FALSE) - lseries not found
# t2 <- at^2 - at not found
# Werdykt: 1

# --------- 050 -----------
# install.packages("comf_0.1.6.tar.gz", repos=NULL, type="source") - !
# figure 1 - OK
# Werdykt: 2

# --------- 051 -----------
# rgdal not available

# --------- 052 -----------
extract_code_from_html(files[23], overwrite = TRUE, console_char = "R>")
# faccsex <- factor(csex) - no varialble csex
# piv.bsp <- npqr(formula = form.par, basis = basis.bsp, var = "cage", taus = taus,
# nderivs = 1, average = 1, print.taus = print.taus, B = B, uniform = TRUE) - 'data' is missing
# Werdykt: 0 - zmienne z datasetu nie są wyeksportowane

# --------- 053 -----------
extract_code_from_html(files[24], overwrite = TRUE, console_char = "")
# Package is not initialized, please call nmfgpu4R.init() first!
# rror in nmfgpu4R.init() : No CUDA toolkit detected on this system
# werdykt : 0
# --------- 054 -----------
extract_code_from_html(files[25], overwrite = TRUE, console_char = "")
# result1 <- psel(df, low(x) * low(y)) - 
# Evaluation of base preference low(x) does not have the same length as the data frame!
# Werdykt: 2