###########################################################################
### R Code for SimCorrMix paper: Includes code for examples in article and
### code for "Examples comparing the two simulation pathways".
### NOTE: Those examples take considerable time to run (several hours) and
### executing this script will run all cases sequentially.
###########################################################################

options(scipen = 999)

# Genetic mixture graphs (Figure 1)
par(cex = 1.75)
y <- seq(-7, 7, length = 100)
plot(y, dnorm(y, -5, 1), type="l", lty = 1, xlab = "Phenotype y",
  ylab = "Density")
lines(y, dnorm(y, -1, 1.5), lty = 2)
lines(y, dnorm(y, 4, 2), lty = 3)
text(x = c(-5, -1.25, 4), y = c(0.15, 0.15, 0.15), labels = c("AA", "Aa", "aa"))
y <- seq(-7, 5, length = 100)
plot(y, dnorm(y, -5, 1), type="l", lty = 1, xlab = "Phenotype y",
  ylab = "Density")
lines(y, dnorm(y, -1, 1.5), lty = 2)
text(x = c(-5, -1.25), y = c(0.15, 0.15), labels = c("AA/Aa", "aa"))

# calc_mixmoments example:
library("SimCorrMix")
B1 <- calc_theory("Beta", c(13, 11))
B2 <- calc_theory("Beta", c(13, 4))
mix_pis <- list(c(0.36, 0.48, 0.16), c(0.3, 0.7))
mix_mus <- list(c(-5, 1, 7), c(B1[1], B2[1]))
mix_sigmas <- list(c(sqrt(2), sqrt(3), sqrt(4)), c(B1[2], B2[2]))
mix_skews <- list(c(0, 0, 0), c(B1[3], B2[3]))
mix_skurts <- list(c(0, 0, 0), c(B1[4], B2[4]))
mix_fifths <- list(c(0, 0, 0), c(B1[5], B2[5]))
mix_sixths <- list(c(0, 0, 0), c(B1[6], B2[6]))
Nstcum <- calc_mixmoments(mix_pis[[1]], mix_mus[[1]], mix_sigmas[[1]],
  mix_skews[[1]], mix_skurts[[1]], mix_fifths[[1]], mix_sixths[[1]])
Nstcum
Bstcum <- calc_mixmoments(mix_pis[[2]], mix_mus[[2]], mix_sigmas[[2]],
  mix_skews[[2]], mix_skurts[[2]], mix_fifths[[2]], mix_sixths[[2]])
Bstcum

# rho_M1M2 example:
rho <- matrix(0.4, 6, 6)
# set correlation between components of the normal mixture variable to 0.1
rho[1:3, 1:3] <- matrix(0.1, 3, 3)
# set correlation between components of the beta mixture variable to 0
rho[4:5, 4:5] <- matrix(0, 2, 2)
diag(rho) <- 1

rho_M1M2(mix_pis, mix_mus, mix_sigmas, rho[1:3, 4:5])

# rho_M1Y example:
rho_M1Y(mix_pis[[1]], mix_mus[[1]], mix_sigmas[[1]], rho[1:3, 6])
rho_M1Y(mix_pis[[2]], mix_mus[[2]], mix_sigmas[[2]], rho[4:5, 6])

means <- c(Nstcum[1], Bstcum[1])
vars <- c(Nstcum[2]^2, Bstcum[2]^2)

seed <- 184
Sim1 <- corrvar(n = 10000, k_mix = 2, k_pois = 1, method = "Polynomial",
  means = means, vars = vars, mix_pis = mix_pis, mix_mus = mix_mus,
  mix_sigmas = mix_sigmas, mix_skews = mix_skews, mix_skurts = mix_skurts,
  mix_fifths = mix_fifths, mix_sixths = mix_sixths, lam = 5, p_zip = 0.1,
  rho = rho, seed = seed, use.nearPD = FALSE)
names(Sim1)
Sum1 <- summary_var(Y_comp = Sim1$Y_comp, Y_mix = Sim1$Y_mix,
  Y_pois = Sim1$Y_pois, means = means, vars = vars, mix_pis = mix_pis,
  mix_mus = mix_mus, mix_sigmas = mix_sigmas, mix_skews = mix_skews,
  mix_skurts = mix_skurts, mix_fifths = mix_fifths, mix_sixths = mix_sixths,
  lam = 5, p_zip = 0.1, rho = rho)
names(Sum1)
Sum1$rho_mix

# plot normal mixture variable PDF (Figure 2a)
plot_simpdf_theory(sim_y = Sim1$Y_mix[, 1], title = "", sim_size = 2,
  target_size = 2, fx = function(x) mix_pis[[1]][1] *
    dnorm(x, mix_mus[[1]][1], mix_sigmas[[1]][1]) + mix_pis[[1]][2] *
    dnorm(x, mix_mus[[1]][2], mix_sigmas[[1]][2]) + mix_pis[[1]][3] *
    dnorm(x, mix_mus[[1]][3], mix_sigmas[[1]][3]), lower = -10, upper = 10,
  legend.position = "none", axis.text.size = 30, axis.title.size = 30)
# plot zero-inflated Poisson variable (Figure 2b)
plot_simtheory(sim_y = Sim1$Y_pois[, 1], title = "", cont_var = FALSE,
  binwidth = 0.5, Dist = "Poisson", params = c(5, 0.1),
  legend.position = "none", axis.text.size = 30, axis.title.size = 30)

# comparison of simulated Beta mixture variable to theoretical distribution
# check for valid PDF's
Sim1$valid.pdf
# find critical value for alpha = 0.05
beta_fx <- function(x) mix_pis[[2]][1] * dbeta(x, 13, 11) +
  mix_pis[[2]][2] * dbeta(x, 13, 4)
beta_cfx <- function(x, alpha, fx = beta_fx) {
  integrate(function(x, FUN = fx) FUN(x), -Inf, x, subdivisions = 1000,
    stop.on.error = FALSE)$value - (1 - alpha)
}
y2_star <- uniroot(beta_cfx, c(0, 1), tol = 0.001, alpha = 0.05)$root
y2_star
# calculate cumulative probability
sim_cdf_prob(sim_y = Sim1$Y_mix[, 2], delta = y2_star)$cumulative_prob

# plot beta mixture variable PDF (Figure 3a)
plot_simpdf_theory(sim_y = Sim1$Y_mix[, 2], title = "", sim_size = 2,
  target_size = 2, fx = beta_fx, lower = 0, upper = 1,
  legend.position = c(0.4, 0.85), legend.text.size = 30,
  axis.text.size = 30, axis.title.size = 30)
# plot empirical cdf and show cumulative prob. up to y2_star (Figure 3b)
plot_sim_cdf(sim_y = Sim1$Y_mix[, 2], title = "", calc_cprob = TRUE,
  delta = y2_star, text.size = 30, axis.text.size = 30, axis.title.size = 30)

# correlation boundaries example:
marginal <- list(c(1/3, 2/3))
support <- list(c(0, 1, 2))
lam <- c(0.5, 1)
p_zip <- c(0.1, 0.2)
mu <- c(0.5, 1)
prob <- c(0.8, 0.6)
size <- prob * mu/(1 - prob)
p_zinb <- c(0.1, 0.2)

# invalid rho
rho <- matrix(-0.5, 10, 10)
# set correlation between components of the normal mixture variable to 0.1
rho[2:4, 2:4] <- matrix(0.1, 3, 3)
# set correlation between components of the beta mixture variable to 0
rho[5:6, 5:6] <- matrix(0, 2, 2)
diag(rho) <- 1
rownames(rho) <- colnames(rho) <- c("O1", "N_1", "N_2", "N_3", "B_1",
  "B_2", "P1", "P2", "NB1", "NB2")

validpar(k_cat = 1, k_mix = 2, k_pois = 2, k_nb = 2, method = "Polynomial",
  means = means, vars = vars, mix_pis = mix_pis, mix_mus = mix_mus,
  mix_sigmas = mix_sigmas, mix_skews = mix_skews, mix_skurts = mix_skurts,
  mix_fifths = mix_fifths, mix_sixths = mix_sixths, marginal = marginal,
  support = support, lam = lam, p_zip = p_zip, size = size, mu = mu,
  p_zinb = p_zinb, rho = rho)
valid1 <- validcorr(10000, k_cat = 1, k_mix = 2, k_pois = 2, k_nb = 2,
  method = "Polynomial", means = means, vars = vars, mix_pis = mix_pis,
  mix_mus = mix_mus, mix_sigmas = mix_sigmas, mix_skews = mix_skews,
  mix_skurts = mix_skurts, mix_fifths = mix_fifths, mix_sixths = mix_sixths,
  marginal = marginal, lam = lam, p_zip = p_zip, size = size, mu = mu,
  p_zinb = p_zinb, rho = rho, use.nearPD = FALSE, quiet = TRUE)
names(valid1)
valid2 <- validcorr2(10000, k_cat = 1, k_mix = 2, k_pois = 2, k_nb = 2,
  method = "Polynomial", means = means, vars = vars, mix_pis = mix_pis,
  mix_mus = mix_mus, mix_sigmas = mix_sigmas, mix_skews = mix_skews,
  mix_skurts = mix_skurts, mix_fifths = mix_fifths, mix_sixths = mix_sixths,
  marginal = marginal, lam = lam, p_zip = p_zip, size = size, mu = mu,
  p_zinb = p_zinb, rho = rho, use.nearPD = FALSE, quiet = TRUE)

###########################################################################
### Examples comparing the two simulation pathways
###########################################################################

###########################################################################
### Setup
###########################################################################
options(scipen = 999)
library("SimCorrMix")
library("data.table")
library("xtable")
library("ggplot2")

###########################################################################
### Function to create data.frame consisting of element-wise median (IQR)
### across data.frames in a list
###########################################################################
quantile_summary <- function(x_list, q1 = 0.25, q2 = 0.75, digits = 3) {
  as.data.frame(rbindlist(lapply(x_list, data.table, keep.rownames = TRUE))[,
    lapply(.SD, function(x) paste(round(median(x, na.rm = TRUE), digits), " (",
    round(quantile(x, probs = q1, na.rm = TRUE), digits), ", ",
    round(quantile(x, probs = q2, na.rm = TRUE), digits), ")", sep = "")),
    by = rn][, rn := NULL])
}

###############################################################################
### runID varies with correlation
### 1 = strong correlations; 2 = moderate correlations; 3 = weak correlations
###############################################################################
runID <- 1:3

# repetitions
rep <- 1000

# sample size
n <- 10000

# Continuous Mixture Distributions
B1 <- calc_theory("Beta", c(13, 11))
B2 <- calc_theory("Beta", c(13, 4))
mix_pis <- list(c(0.36, 0.48, 0.16), c(0.3, 0.7))
mix_mus <- list(c(-5, 1, 7), c(B1[1], B2[1]))
mix_sigmas <- list(c(sqrt(2), sqrt(3), sqrt(4)), c(B1[2], B2[2]))
mix_skews <- list(c(0, 0, 0), c(B1[3], B2[3]))
mix_skurts <- list(c(0, 0, 0), c(B1[4], B2[4]))
mix_fifths <- list(c(0, 0, 0), c(B1[5], B2[5]))
mix_sixths <- list(c(0, 0, 0), c(B1[6], B2[6]))
Nstcum <- calc_mixmoments(mix_pis[[1]], mix_mus[[1]], mix_sigmas[[1]],
  mix_skews[[1]], mix_skurts[[1]], mix_fifths[[1]], mix_sixths[[1]])
Bstcum <- calc_mixmoments(mix_pis[[2]], mix_mus[[2]], mix_sigmas[[2]],
  mix_skews[[2]], mix_skurts[[2]], mix_fifths[[2]], mix_sixths[[2]])
means <- c(Nstcum[1], Bstcum[1])
vars <- c(Nstcum[2]^2, Bstcum[2]^2)

# Ordinal Distribution
marginal <- list(c(1/3, 2/3))
support <- list(c(0, 1, 2))

###############################################################################
### Scenario A: Ordinal, Normal mixture, Beta mixture, 2 Poisson, and 2 NB
### variables
###############################################################################

# ZIP Distributions
lam <- c(0.5, 1)
p_zip <- c(0.1, 0.2)

# ZINB Distributions
mu <- c(0.5, 1)
prob <- c(0.8, 0.6)
size <- prob * mu/(1 - prob)
p_zinb <- c(0.1, 0.2)

for (i in 1:length(runID)) {
  # strong correlations
  if (runID[i] == 1) {
    rho <- matrix(0.7, 10, 10)
  }
  # moderate correlations
  if (runID[i] == 2) {
    rho <- matrix(0.5, 10, 10)
  }
  # weak correlations
  if (runID[i] == 3) {
    rho <- matrix(0.3, 10, 10)
  }
  # set correlation between components of the normal mixture variable to 0.1
  rho[2:4, 2:4] <- matrix(0.1, 3, 3)
  # set correlation between components of the beta mixture variable to 0
  rho[5:6, 5:6] <- matrix(0, 2, 2)
  diag(rho) <- 1
  rownames(rho) <- colnames(rho) <- c("O1", "N1", "N2", "N3", "B1",
    "B2", "P1", "P2", "NB1", "NB2")
  # correlation method 1
  seed <- 1
  Sim1 <- list()
  start <- Sys.time()
  for (r in 1:rep) {
    Sim1[[r]] <- corrvar(n = 10000, k_cat = 1, k_mix = 2, k_pois = 2, k_nb = 2,
      method = "Polynomial", means = means, vars = vars, mix_pis = mix_pis,
      mix_mus = mix_mus, mix_sigmas = mix_sigmas, mix_skews = mix_skews,
      mix_skurts = mix_skurts, mix_fifths = mix_fifths,
      mix_sixths = mix_sixths, marginal = marginal, support = support,
      lam = lam, p_zip = p_zip, size = size, mu = mu, p_zinb = p_zinb,
      rho = rho, seed = seed, use.nearPD = FALSE, quiet = TRUE)
    seed <- seed + 1
  }
  stop <- Sys.time()
  Time1 <- round(difftime(stop, start, units = "secs"), 8)
  # correlation method 2
  seed <- 1
  Sim2 <- list()
  start <- Sys.time()
  for (r in 1:rep) {
    Sim2[[r]] <- corrvar2(n = 10000, k_cat = 1, k_mix = 2, k_pois = 2,
      k_nb = 2, method = "Polynomial", means = means, vars = vars,
      mix_pis = mix_pis, mix_mus = mix_mus, mix_sigmas = mix_sigmas,
      mix_skews = mix_skews, mix_skurts = mix_skurts, mix_fifths = mix_fifths,
      mix_sixths = mix_sixths, marginal = marginal, support = support,
      lam = lam, p_zip = p_zip, size = size, mu = mu, p_zinb = p_zinb,
      rho = rho, seed = seed, use.nearPD = FALSE, quiet = TRUE)
    seed <- seed + 1
  }
  stop <- Sys.time()
  Time2 <- round(difftime(stop, start, units = "secs"), 8)
  # summarize results
  Sum1 <- list()
  Sum2 <- list()
  for (r in 1:rep) {
    Sum1[[r]] <- summary_var(Y_cat = Sim1[[r]]$Y_cat,
      Y_comp = Sim1[[r]]$Y_comp, Y_mix = Sim1[[r]]$Y_mix,
      Y_pois = Sim1[[r]]$Y_pois, Y_nb = Sim1[[r]]$Y_nb, means = means,
      vars = vars, mix_pis = mix_pis, mix_mus = mix_mus,
      mix_sigmas = mix_sigmas, mix_skews = mix_skews, mix_skurts = mix_skurts,
      mix_fifths = mix_fifths, mix_sixths = mix_sixths, marginal = marginal,
      lam = lam, p_zip = p_zip, size = size, mu = mu, p_zinb = p_zinb,
      rho = rho)
    Sum2[[r]] <- summary_var(Y_cat = Sim2[[r]]$Y_cat,
      Y_comp = Sim2[[r]]$Y_comp, Y_mix = Sim2[[r]]$Y_mix,
      Y_pois = Sim2[[r]]$Y_pois, Y_nb = Sim2[[r]]$Y_nb, means = means,
      vars = vars, mix_pis = mix_pis, mix_mus = mix_mus,
      mix_sigmas = mix_sigmas, mix_skews = mix_skews, mix_skurts = mix_skurts,
      mix_fifths = mix_fifths, mix_sixths = mix_sixths, marginal = marginal,
      lam = lam, p_zip = p_zip, size = size, mu = mu, p_zinb = p_zinb,
      rho = rho)
  }
  # create lists containing correlation errors
  Corr_error1 <- list()
  Corr_error2 <- list()
  for (r in 1:rep) {
    Corr_error1[[r]] <- as.data.frame(Sum1[[r]]$rho_calc - rho)
    rownames(Corr_error1[[r]]) <- colnames(Corr_error1[[r]]) <-
      colnames(rho)
    Corr_error2[[r]] <- as.data.frame(Sum2[[r]]$rho_calc - rho)
    rownames(Corr_error2[[r]]) <- colnames(Corr_error2[[r]]) <-
      colnames(rho)
  }
  # summarize correlation errors
  corr_error1 <- quantile_summary(Corr_error1)
  colnames(corr_error1) <- rownames(corr_error1) <- colnames(rho)
  corr_error2 <- quantile_summary(Corr_error2)
  colnames(corr_error2) <- rownames(corr_error2) <- colnames(rho)
  corr_errors <- matrix(1, nrow = 10, ncol = 10)
  for (k in 1:nrow(corr_errors)) {
    for (j in 1:ncol(corr_errors)) {
      if (k == j) corr_errors[k, j] <- "0"
      if (k < j) corr_errors[k, j] <- as.character(corr_error1[k, j])
      if (k > j) corr_errors[k, j] <- paste("\\color{blue}",
        as.character(corr_error2[k, j]), sep = " ")
    }
  }
  colnames(corr_errors) <- rownames(corr_errors) <- colnames(rho)
  if (runID[i] == 1) {
    SSum1 <- Sum1; SSum2 <- Sum2
    cap <- "Median (IQR) of correlation errors using correlation method 1
      (in black) and correlation method 2 (in blue) with strong correlations
      in scenario A."
    lab <- "table_Scorr_errorsA"
    saveRDS(Sim1, "SSim1.rds")
    saveRDS(Sim2, "SSim2.rds")
    saveRDS(Sum1, "SSum1.rds")
    saveRDS(Sum2, "SSum2.rds")
    saveRDS(Time1, "STime1.rds")
    saveRDS(Time2, "STime2.rds")
  }
  if (runID[i] == 2) {
    MSum1 <- Sum1; MSum2 <- Sum2
    cap <- "Median (IQR) of correlation errors using correlation method 1
      (in black) and correlation method 2 (in blue) with moderate correlations
      in scenario A."
    lab <- "table_Mcorr_errorsA"
    saveRDS(Sim1, "MSim1.rds")
    saveRDS(Sim2, "MSim2.rds")
    saveRDS(Sum1, "MSum1.rds")
    saveRDS(Sum2, "MSum2.rds")
    saveRDS(Time1, "MTime1.rds")
    saveRDS(Time2, "MTime2.rds")
  }
  if (runID[i] == 3) {
    WSum1 <- Sum1; WSum2 <- Sum2
    cap <- "Median (IQR) of correlation errors using correlation method 1
      (in black) and correlation method 2 (in blue) with weak correlations
      in scenario A."
    lab <- "table_Wcorr_errorsA"
    saveRDS(Sim1, "WSim1.rds")
    saveRDS(Sim2, "WSim2.rds")
    saveRDS(Sum1, "WSum1.rds")
    saveRDS(Sum2, "WSum2.rds")
    saveRDS(Time1, "WTime1.rds")
    saveRDS(Time2, "WTime2.rds")
  }
  # display all correlation errors for Tables 7-9 in Appendix
  print(xtable(corr_errors[, 1:5]), floating.environment = 'sidewaystable',
    sanitize.text.function=function(x){x})
  print(xtable(corr_errors[, 6:10],
    caption = cap, label = lab), floating.environment = 'sidewaystable',
    sanitize.text.function=function(x){x})
}

# boxplot graphs of correlations

# function to extract element [a, b] from mth element in list of lists for 6 lists
rho_ab <- function(l1, l2, l3, l4, l5, l6, m, a, b) {
  c(sapply(l1, function(x) x[[m]][a, b]),
    sapply(l2, function(x) x[[m]][a, b]),
    sapply(l3, function(x) x[[m]][a, b]),
    sapply(l4, function(x) x[[m]][a, b]),
    sapply(l5, function(x) x[[m]][a, b]),
    sapply(l6, function(x) x[[m]][a, b]))
}

# graphs of rho_Nmix,Bmix, rho_Nmix,O1, rho_Bmix,O1 (Figure 4)

# calculate approximate expected values for correlations with continuous
# mixture variables (Table 2)
rhoNBmix <- rhoNO <- rhoBO <- numeric(3)
for (i in 1:3) {
  if (i == 1) rho <- matrix(0.7, 10, 10)
  if (i == 2) rho <- matrix(0.5, 10, 10)
  if (i == 3) rho <- matrix(0.3, 10, 10)
  rho[2:4, 2:4] <- matrix(0.1, 3, 3)
  rho[5:6, 5:6] <- matrix(0, 2, 2)
  diag(rho) <- 1
  rownames(rho) <- colnames(rho) <- c("O1", "N1", "N2", "N3", "B1",
    "B2", "P1", "P2", "NB1", "NB2")
  rhoNBmix[i] <- rho_M1M2(mix_pis, mix_mus, mix_sigmas, rho[2:4, 5:6])
  rhoNO[i] <- rho_M1Y(mix_pis[[1]], mix_mus[[1]], mix_sigmas[[1]], rho[2:4, 1])
  rhoBO[i] <- rho_M1Y(mix_pis[[2]], mix_mus[[2]], mix_sigmas[[2]], rho[5:6, 1])
}
rhoNBmix; rhoNO; rhoBO

# create a data.frame of correlations
rho_NBmix <- rho_ab(SSum1, SSum2, MSum1, MSum2, WSum1, WSum2, 6, 2, 3)
rho_NO <- rho_ab(SSum1, SSum2, MSum1, MSum2, WSum1, WSum2, 6, 2, 1)
rho_BO <- rho_ab(SSum1, SSum2, MSum1, MSum2, WSum1, WSum2, 6, 3, 1)
corr <- factor(rep(c("1Strong", "2Moderate", "3Weak"), each = 2 * rep),
  labels = c("Strong", "Moderate", "Weak"))
Method <- as.factor(rep(c(rep(1, 1000), rep(2, 1000)), 3))
rho_data <- data.frame(corr, Method, rho_NBmix, rho_NO, rho_BO)
exp_rho <- data.frame(corr = factor(rep(c("1Strong", "2Moderate", "3Weak"),
  each = 2), labels = c("Strong", "Moderate", "Weak")),
  Method = factor(rep(c(1, 2), 3)), exp_NBmix = rep(rhoNBmix, each = 2),
  exp_NO = rep(rhoNO, each = 2), exp_BO = rep(rhoBO, each = 2))

theme_set(theme_gray(base_size = 30))
NBmix_plot <- ggplot(rho_data, aes(x = corr, y = rho_NBmix, fill = Method)) +
  geom_boxplot(outlier.size = 3, size = 1.5) +
  facet_wrap(~corr, scale = "free") +
  geom_hline(data = exp_rho, aes(yintercept = exp_NBmix), size = 1.5) +
  xlab("") + ylab(expression(rho[paste("Nmix", ",", "Bmix", sep = "")])) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none")
NBmix_plot
NO_plot <- ggplot(rho_data, aes(x = corr, y = rho_NO, fill = Method)) +
  geom_boxplot(outlier.size = 3, size = 1.5) +
  facet_wrap(~corr, scale = "free") +
  geom_hline(data = exp_rho, aes(yintercept = exp_NO), size = 1.5) +
  xlab("") + ylab(expression(rho[paste("Nmix", ",", "O1", sep = "")])) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none")
NO_plot
BO_plot <- ggplot(rho_data, aes(x = corr, y = rho_BO, fill = Method)) +
  geom_boxplot(outlier.size = 3, size = 1.5) +
  facet_wrap(~corr, scale = "free") +
  geom_hline(data = exp_rho, aes(yintercept = exp_BO), size = 1.5) +
  xlab("") + ylab(expression(rho[paste("Bmix", ",", "O1", sep = "")])) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none")
BO_plot

# graphs of P1, P2, NB1, NB2 correlations (Figure 5)

# create a data.frame of correlations
rho_P1P2 <- rho_ab(SSum1, SSum2, MSum1, MSum2, WSum1, WSum2, 9, 7, 8)
rho_P1NB1 <- rho_ab(SSum1, SSum2, MSum1, MSum2, WSum1, WSum2, 9, 7, 9)
rho_P1NB2 <- rho_ab(SSum1, SSum2, MSum1, MSum2, WSum1, WSum2, 9, 7, 10)
rho_P2NB1 <- rho_ab(SSum1, SSum2, MSum1, MSum2, WSum1, WSum2, 9, 8, 9)
rho_P2NB2 <- rho_ab(SSum1, SSum2, MSum1, MSum2, WSum1, WSum2, 9, 8, 10)
rho_NB1NB2 <- rho_ab(SSum1, SSum2, MSum1, MSum2, WSum1, WSum2, 9, 9, 10)

corr <- factor(rep(c("1Strong", "2Moderate", "3Weak"), each = 2 * rep),
  labels = c("Strong", "Moderate", "Weak"))
Method <- as.factor(rep(c(rep(1, 1000), rep(2, 1000)), 3))
rho_data <- data.frame(corr, Method, rho_P1P2, rho_P1NB1, rho_P1NB2,
  rho_P2NB1, rho_P2NB2, rho_NB1NB2)
exp_rho <- data.frame(corr = factor(rep(c("1Strong", "2Moderate", "3Weak"),
  each = 2), labels = c("Strong", "Moderate", "Weak")),
  Method = factor(rep(c(1, 2), 3)), rhos = c(0.7, 0.7, 0.5, 0.5, 0.3, 0.3))

theme_set(theme_gray(base_size = 40))
P1P2_plot <- ggplot(rho_data, aes(x = corr, y = rho_P1P2, fill = Method)) +
  geom_boxplot(outlier.size = 3, size = 1.5) +
  facet_wrap(~corr, scale = "free") +
  geom_hline(data = exp_rho, aes(yintercept = rhos), size = 1.5) +
  xlab("") + ylab(expression(rho[paste("P1", ",", "P2", sep = "")])) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none")
P1P2_plot
P1NB1_plot <- ggplot(rho_data, aes(x = corr, y = rho_P1NB1, fill = Method)) +
  geom_boxplot(outlier.size = 3, size = 1.5) +
  facet_wrap(~corr, scale = "free") +
  geom_hline(data = exp_rho, aes(yintercept = rhos), size = 1.5) +
  xlab("") + ylab(expression(rho[paste("P1", ",", "NB1", sep = "")])) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none")
P1NB1_plot
P1NB2_plot <- ggplot(rho_data, aes(x = corr, y = rho_P1NB2, fill = Method)) +
  geom_boxplot(outlier.size = 3, size = 1.5) +
  facet_wrap(~corr, scale = "free") +
  geom_hline(data = exp_rho, aes(yintercept = rhos), size = 1.5) +
  xlab("") + ylab(expression(rho[paste("P1", ",", "NB2", sep = "")])) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none")
P1NB2_plot
P2NB1_plot <- ggplot(rho_data, aes(x = corr, y = rho_P2NB1, fill = Method)) +
  geom_boxplot(outlier.size = 3, size = 1.5) +
  facet_wrap(~corr, scale = "free") +
  geom_hline(data = exp_rho, aes(yintercept = rhos), size = 1.5) +
  xlab("") + ylab(expression(rho[paste("P2", ",", "NB1", sep = "")])) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none")
P2NB1_plot
P2NB2_plot <- ggplot(rho_data, aes(x = corr, y = rho_P2NB2, fill = Method)) +
  geom_boxplot(outlier.size = 3, size = 1.5) +
  facet_wrap(~corr, scale = "free") +
  geom_hline(data = exp_rho, aes(yintercept = rhos), size = 1.5) +
  xlab("") + ylab(expression(rho[paste("P2", ",", "NB2", sep = "")])) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none")
P2NB2_plot
NB1NB2_plot <- ggplot(rho_data, aes(x = corr, y = rho_NB1NB2, fill = Method)) +
  geom_boxplot(outlier.size = 3, size = 1.5) +
  facet_wrap(~corr, scale = "free") +
  geom_hline(data = exp_rho, aes(yintercept = rhos), size = 1.5) +
  xlab("") + ylab(expression(rho[paste("NB1", ",", "NB2", sep = "")])) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none")
NB1NB2_plot

###########################################################################
### Scenario B: Ordinal, Normal mixture, Beta mixture, and 4 NB variables
###########################################################################
runID <- 1:3

# ZINB Distributions
mu <- c(0.5, 1, 50, 100)
prob <- c(0.8, 0.6, 0.4, 0.2)
size <- prob * mu/(1 - prob)
p_zinb <- c(0.1, 0.2, 0.1, 0.2)

for (i in 1:length(runID)) {
  # strong correlations
  if (runID[i] == 1) {
    rho <- matrix(0.7, 10, 10)
    # set correlation between NB1/NB2 and NB3/NB4 to 0.6
    rho[7:8, 9:10] <- rho[9:10, 7:8] <- 0.6
  }
  # moderate correlations
  if (runID[i] == 2) {
    rho <- matrix(0.5, 10, 10)
  }
  # weak correlations
  if (runID[i] == 3) {
    rho <- matrix(0.3, 10, 10)
  }
  # set correlation between components of the normal mixture variable to 0.1
  rho[2:4, 2:4] <- matrix(0.1, 3, 3)
  # set correlation between components of the beta mixture variable to 0
  rho[5:6, 5:6] <- matrix(0, 2, 2)
  diag(rho) <- 1
  rownames(rho) <- colnames(rho) <- c("O1", "N1", "N2", "N3", "B1",
    "B2", "NB1", "NB2", "NB3", "NB4")
  # correlation method 1
  seed <- 1
  Sim1 <- list()
  start <- Sys.time()
  for (r in 1:rep) {
    Sim1[[r]] <- corrvar(n = 10000, k_cat = 1, k_mix = 2, k_nb = 4,
      method = "Polynomial", means = means, vars = vars, mix_pis = mix_pis,
      mix_mus = mix_mus, mix_sigmas = mix_sigmas, mix_skews = mix_skews,
      mix_skurts = mix_skurts, mix_fifths = mix_fifths,
      mix_sixths = mix_sixths, marginal = marginal, support = support,
      size = size, mu = mu, p_zinb = p_zinb, rho = rho, seed = seed,
      use.nearPD = FALSE, quiet = TRUE)
    seed <- seed + 1
  }
  stop <- Sys.time()
  Time1 <- round(difftime(stop, start, units = "secs"), 8)
  # correlation method 2
  seed <- 1
  Sim2 <- list()
  start <- Sys.time()
  for (r in 1:rep) {
    Sim2[[r]] <- corrvar2(n = 10000, k_cat = 1, k_mix = 2, k_nb = 4,
      method = "Polynomial", means = means, vars = vars, mix_pis = mix_pis,
      mix_mus = mix_mus, mix_sigmas = mix_sigmas, mix_skews = mix_skews,
      mix_skurts = mix_skurts, mix_fifths = mix_fifths,
      mix_sixths = mix_sixths, marginal = marginal, support = support,
      size = size, mu = mu, p_zinb = p_zinb, rho = rho, seed = seed,
      use.nearPD = FALSE, quiet = TRUE)
    seed <- seed + 1
  }
  stop <- Sys.time()
  Time2 <- round(difftime(stop, start, units = "secs"), 8)
  # summarize results
  Sum1 <- list()
  Sum2 <- list()
  for (r in 1:rep) {
    Sum1[[r]] <- summary_var(Y_cat = Sim1[[r]]$Y_cat,
      Y_comp = Sim1[[r]]$Y_comp, Y_mix = Sim1[[r]]$Y_mix,
      Y_nb = Sim1[[r]]$Y_nb, means = means, vars = vars, mix_pis = mix_pis,
      mix_mus = mix_mus, mix_sigmas = mix_sigmas, mix_skews = mix_skews,
      mix_skurts = mix_skurts, mix_fifths = mix_fifths,
      mix_sixths = mix_sixths, marginal = marginal, size = size, mu = mu,
      p_zinb = p_zinb, rho = rho)
    Sum2[[r]] <- summary_var(Y_cat = Sim2[[r]]$Y_cat,
      Y_comp = Sim2[[r]]$Y_comp, Y_mix = Sim2[[r]]$Y_mix,
      Y_nb = Sim2[[r]]$Y_nb, means = means, vars = vars, mix_pis = mix_pis,
      mix_mus = mix_mus, mix_sigmas = mix_sigmas, mix_skews = mix_skews,
      mix_skurts = mix_skurts, mix_fifths = mix_fifths,
      mix_sixths = mix_sixths, marginal = marginal, size = size, mu = mu,
      p_zinb = p_zinb, rho = rho)
  }
  # create lists containing correlation errors
  Corr_error1 <- list()
  Corr_error2 <- list()
  for (r in 1:rep) {
    Corr_error1[[r]] <- as.data.frame(Sum1[[r]]$rho_calc - rho)
    rownames(Corr_error1[[r]]) <- colnames(Corr_error1[[r]]) <-
      colnames(rho)
    Corr_error2[[r]] <- as.data.frame(Sum2[[r]]$rho_calc - rho)
    rownames(Corr_error2[[r]]) <- colnames(Corr_error2[[r]]) <-
      colnames(rho)
  }
  # summarize correlation errors
  corr_error1 <- quantile_summary(Corr_error1)
  colnames(corr_error1) <- rownames(corr_error1) <- colnames(rho)
  corr_error2 <- quantile_summary(Corr_error2)
  colnames(corr_error2) <- rownames(corr_error2) <- colnames(rho)
  corr_errors <- matrix(1, nrow = 10, ncol = 10)
  for (k in 1:nrow(corr_errors)) {
    for (j in 1:ncol(corr_errors)) {
      if (k == j) corr_errors[k, j] <- "0"
      if (k < j) corr_errors[k, j] <- as.character(corr_error1[k, j])
      if (k > j) corr_errors[k, j] <- paste("\\color{blue}",
        as.character(corr_error2[k, j]), sep = " ")
    }
  }
  colnames(corr_errors) <- rownames(corr_errors) <- colnames(rho)
  if (runID[i] == 1) {
    SSum1 <- Sum1; SSum2 <- Sum2
    cap <- "Median (IQR) of correlation errors using correlation method 1
      (in black) and correlation method 2 (in blue) with strong correlations
      in scenario B."
    lab <- "table_Scorr_errorsB"
    saveRDS(Sim1, "SSim1_NB.rds")
    saveRDS(Sim2, "SSim2_NB.rds")
    saveRDS(Sum1, "SSum1_NB.rds")
    saveRDS(Sum2, "SSum2_NB.rds")
    saveRDS(Time1, "STime1_NB.rds")
    saveRDS(Time2, "STime2_NB.rds")
  }
  if (runID[i] == 2) {
    MSum1 <- Sum1; MSum2 <- Sum2
    cap <- "Median (IQR) of correlation errors using correlation method 1
      (in black) and correlation method 2 (in blue) with moderate correlations
      in scenario B."
    lab <- "table_Mcorr_errorsB"
    saveRDS(Sim1, "MSim1_NB.rds")
    saveRDS(Sim2, "MSim2_NB.rds")
    saveRDS(Sum1, "MSum1_NB.rds")
    saveRDS(Sum2, "MSum2_NB.rds")
    saveRDS(Time1, "MTime1_NB.rds")
    saveRDS(Time2, "MTime2_NB.rds")
  }
  if (runID[i] == 3) {
    WSum1 <- Sum1; WSum2 <- Sum2
    cap <- "Median (IQR) of correlation errors using correlation method 1
      (in black) and correlation method 2 (in blue) with weak correlations
      in scenario B."
    lab <- "table_Wcorr_errorsB"
    saveRDS(Sim1, "WSim1_NB.rds")
    saveRDS(Sim2, "WSim2_NB.rds")
    saveRDS(Sum1, "WSum1_NB.rds")
    saveRDS(Sum2, "WSum2_NB.rds")
    saveRDS(Time1, "WTime1_NB.rds")
    saveRDS(Time2, "WTime2_NB.rds")
  }
  # display all correlation errors for Table 10-12 in Appendix
  print(xtable(corr_errors[, 1:5]), floating.environment = 'sidewaystable',
    sanitize.text.function=function(x){x})
  print(xtable(corr_errors[, 6:10],
    caption = cap, label = lab), floating.environment = 'sidewaystable',
    sanitize.text.function=function(x){x})
}

# summarize variables for weak correlations
# summarize mixture variables (Tables 3 and 4)
target_mix <- WSum1[[1]]$target_mix
print(xtable(target_mix,
  caption = "Target distributions for continuous mixture variables.",
  label = "targetmix"), include.rownames = FALSE)

mix_sum <- lapply(WSum1, function(x) x[[4]])
mixsum <- quantile_summary(mix_sum, digits = 2)[, c(3:4, 8:11)]
print(xtable(mixsum[, 1:3]))
print(xtable(mixsum[, 4:6],
  caption = "Median (IQR) of simulated continuous mixture variables
  (from scenario B: weak correlations).",
  label = "mixsum"))

# summarize NB variables (Table 5)
nb_sum <- lapply(WSum1, function(x) x[[7]])
nbsum <- quantile_summary(nb_sum, digits = 2)[, c(3:12)]
print(xtable(nbsum[, 1:5]))
print(xtable(nbsum[, 6:10],
  caption = "Median (IQR) of simulated zero-inflated NB variables
  (from scenario B: weak correlations).",
  label = "nbsum"))

# graphs of NB1, NB2, NB3, NB4 correlations (Figure 6)

# create a data.frame of correlations
rho_NB1NB2 <- rho_ab(SSum1, SSum2, MSum1, MSum2, WSum1, WSum2, 8, 7, 8)
rho_NB1NB3 <- rho_ab(SSum1, SSum2, MSum1, MSum2, WSum1, WSum2, 8, 7, 9)
rho_NB1NB4 <- rho_ab(SSum1, SSum2, MSum1, MSum2, WSum1, WSum2, 8, 7, 10)
rho_NB2NB3 <- rho_ab(SSum1, SSum2, MSum1, MSum2, WSum1, WSum2, 8, 8, 9)
rho_NB2NB4 <- rho_ab(SSum1, SSum2, MSum1, MSum2, WSum1, WSum2, 8, 8, 10)
rho_NB3NB4 <- rho_ab(SSum1, SSum2, MSum1, MSum2, WSum1, WSum2, 8, 9, 10)

corr <- factor(rep(c("1Strong", "2Moderate", "3Weak"), each = 2 * rep),
  labels = c("Strong", "Moderate", "Weak"))
Method <- as.factor(rep(c(rep(1, 1000), rep(2, 1000)), 3))
rho_data <- data.frame(corr, Method, rho_NB1NB2, rho_NB1NB3, rho_NB1NB4,
  rho_NB2NB3, rho_NB2NB4, rho_NB3NB4)
# rhosA are targets for rho_NB1NB2, rho_NB3NB4
# rhosB are targets for rho_NB1NB3, rho_NB1NB4, rho_NB2NB3, rho_NB2NB4
exp_rho <- data.frame(corr = factor(rep(c("1Strong", "2Moderate", "3Weak"),
  each = 2), labels = c("Strong", "Moderate", "Weak")),
  Method = factor(rep(c(1, 2), 3)), rhosA = c(0.7, 0.7, 0.5, 0.5, 0.3, 0.3),
  rhosB = c(0.6, 0.6, 0.5, 0.5, 0.3, 0.3))

theme_set(theme_gray(base_size = 40))
NB1NB2_plot <- ggplot(rho_data, aes(x = corr, y = rho_NB1NB2, fill = Method)) +
  geom_boxplot(outlier.size = 3, size = 1.5) +
  facet_wrap(~corr, scale = "free") +
  geom_hline(data = exp_rho, aes(yintercept = rhosA), size = 1.5) +
  xlab("") + ylab(expression(rho[paste("NB1", ",", "NB2", sep = "")])) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none")
NB1NB2_plot
NB1NB3_plot <- ggplot(rho_data, aes(x = corr, y = rho_NB1NB3, fill = Method)) +
  geom_boxplot(outlier.size = 3, size = 1.5) +
  facet_wrap(~corr, scale = "free") +
  geom_hline(data = exp_rho, aes(yintercept = rhosB), size = 1.5) +
  xlab("") + ylab(expression(rho[paste("NB1", ",", "NB3", sep = "")])) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none")
NB1NB3_plot
NB1NB4_plot <- ggplot(rho_data, aes(x = corr, y = rho_NB1NB4, fill = Method)) +
  geom_boxplot(outlier.size = 3, size = 1.5) +
  facet_wrap(~corr, scale = "free") +
  geom_hline(data = exp_rho, aes(yintercept = rhosB), size = 1.5) +
  xlab("") + ylab(expression(rho[paste("NB1", ",", "NB4", sep = "")])) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none")
NB1NB4_plot
NB2NB3_plot <- ggplot(rho_data, aes(x = corr, y = rho_NB2NB3, fill = Method)) +
  geom_boxplot(outlier.size = 3, size = 1.5) +
  facet_wrap(~corr, scale = "free") +
  geom_hline(data = exp_rho, aes(yintercept = rhosB), size = 1.5) +
  xlab("") + ylab(expression(rho[paste("NB2", ",", "NB3", sep = "")])) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none")
NB2NB3_plot
NB2NB4_plot <- ggplot(rho_data, aes(x = corr, y = rho_NB2NB4, fill = Method)) +
  geom_boxplot(outlier.size = 3, size = 1.5) +
  facet_wrap(~corr, scale = "free") +
  geom_hline(data = exp_rho, aes(yintercept = rhosB), size = 1.5) +
  xlab("") + ylab(expression(rho[paste("NB2", ",", "NB4", sep = "")])) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none")
NB2NB4_plot
NB3NB4_plot <- ggplot(rho_data, aes(x = corr, y = rho_NB3NB4, fill = Method)) +
  geom_boxplot(outlier.size = 3, size = 1.5) +
  facet_wrap(~corr, scale = "free") +
  geom_hline(data = exp_rho, aes(yintercept = rhosA), size = 1.5) +
  xlab("") + ylab(expression(rho[paste("NB3", ",", "NB4", sep = "")])) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none")
NB3NB4_plot

rm(list = ls())
