
# LOAD DATA
data(cor_06)
# SET SEED FOR REPRODUCIBILITY
set.seed(294271)
# CREATE CHARACTER VECTORS REQUIRED FOR FUNCTION
cands <- c("pct_breitenbucher","pct_montanez","pct_spiegel", "pct_skipworth")
race_group2 <- c("~ pct_latino", "~ pct_other")
table_names <- c("EI: Pct Lat", "EI: Pct Other")
# RUN EI GENERALIZED FUNCTION
results <- ei_est_gen(cand_vector=cands, race_group = race_group2,
total = "totvote", data = cor_06, table_names = table_names)
# LOOK AT TABLE OF RESULTS
results
# CHECK TO MAKE SURE DATA SUMS TO 1 FOR EACH PRECINCT
with(cor_06, pct_latino + pct_other)
with(cor_06, pct_breitenbucher + pct_montanez + pct_spiegel + pct_skipworth)
# SET SEED FOR REPRODUCIBILITY
set.seed(124271)
#RxC GENERATE FORMULA
form <- formula(cbind(pct_breitenbucher,pct_montanez,
pct_spiegel, pct_skipworth) ~ cbind(pct_latino, pct_other))
# RUN EI:RxC MODEL
ei_bayes <- ei.reg.bayes(form, data = cor_06, sample = 10000, truncate = TRUE)
# CREATE TABLE NAMES
table_names <- c("RxC: Pct Lat", "RxC: Pct Other")
# TABLE CREATION
ei_bayes_res <- bayes_table_make(ei_bayes, cand_vector = cands, table_names = table_names)
# LOOK AT TABLE OF RESULTS
ei_bayes_res
table_names <- c("Good: Pct Lat", "Good: Pct Other")
good <- goodman_generalize(cands, race_group2, "totvote", cor_06, table_names)
good
ei_rc_combine <- ei_rc_good_table(results, ei_bayes_res,
groups = c("Latino", "Other"))
ei_rc_combine@data
ei_rc_g_combine <- ei_rc_good_table(results, ei_bayes_res, good,
groups = c("Latino", "Other"), include_good = TRUE)
ei_rc_g_combine
# PLOT COMPARISON -- adjust the axes labels slightly
plot(ei_rc_combine, cex.axis = .5, cex.lab = .7)
−10 −5 0 5 10</div>
Latino: EI Diff Comparison
RxC − EI
Candidates</div>
pct_breitenbucher
pct_montanez
pct_spiegel
pct_skipworth
Total</div>
−10 −5 0 5 10</div>
Other: EI Diff Comparison
RxC − EI
Candidates</div>
pct_breitenbucher
pct_montanez
pct_spiegel
pct_skipworth
Total</div>
