# cvcrand: a Package for Covariate-constrained Randomization and the Clustered Permutation Test for Cluster Randomization Trials
# by Hengshi Yu, Fan Li, John A. Gallis and Elizabeth L. Turner

# Examples
library(cvcrand)

# Constrained randomization design: increasing up-to-date immunization rates
Design_result <- cvrall(clustername = Dickinson_design$county,
                        balancemetric = "l2",
                        x = data.frame(Dickinson_design[ , c("location", "inciis",
                                                             "uptodateonimmunizations", "hispanic", "incomecat")]),
                        ntotal_cluster = 16,
                        ntrt_cluster = 8,
                        categorical = c("location", "incomecat"),
                        savedata = "dickinson_constrained.csv",
                        bhist = TRUE,
                        cutoff = 0.1,
                        seed = 12345)

# the balance metric used
Design_result$balancemetric

# the allocation scheme from constrained randomization
Design_result$allocation

# the histogram of the balance score with respect to the balance metric
Design_result$bscores

# the statement about how many clusters to be randomized to the intervention and the control arms respectively
Design_result$assignment_message

# the statement about how to get the whole randomization space to use in constrained randomization
Design_result$scheme_message

# the statement about the cutoff in the constrained space
Design_result$cutoff_message

# the statement about the selected scheme from constrained randomization
Design_result$choice_message

# the data frame containing the allocation scheme, the clustername as well as the original data frame of covariates
Design_result$data_CR

# the descriptive statistics for all the variables by the two arms from the selected scheme
Design_result$baseline_table

# the cluster pair descriptive, which is useful for valid randomization check
Design_result$cluster_coin_des

# the overall allocation summary
Design_result$overall_allocations


# Stratified constrained randomization design: increasing up-to-date immunization rates

# Stratification on location, with constrained randomization on other specified covariates
Design_stratified_result1 <- cvrall(clustername = Dickinson_design$county,
                                    balancemetric = "l2",
                                    x = data.frame(Dickinson_design[ , c("location", "inciis", 
                                                                         "uptodateonimmunizations", 
                                                                         "hispanic", "incomecat")]),
                                    ntotal_cluster = 16,
                                    ntrt_cluster = 8,
                                    categorical = c("location", "incomecat"),
                                    weights = c(1000, 1, 1, 1, 1),
                                    cutoff = 0.1,
                                    seed = 12345) 

# An alternative and equivalent way to stratify on location
Design_stratified_result2 <- cvrall(clustername = Dickinson_design$county,
                                    balancemetric = "l2",
                                    x = data.frame(Dickinson_design[ , c("location", "inciis",
                                                                         "uptodateonimmunizations", 
                                                                         "hispanic", "incomecat")]),
                                    ntotal_cluster = 16,
                                    ntrt_cluster = 8,
                                    categorical = c("location", "incomecat"),
                                    stratify = "location",
                                    cutoff = 0.1,
                                    seed = 12345, 
                                    check_validity = TRUE)



Design_stratified_result1$baseline_table
Design_stratified_result2$baseline_table

# covariate-by-covariate constrained randomization
# change the categorical variable of interest to have numeric representation
Dickinson_design_numeric <- Dickinson_design
Dickinson_design_numeric$location = (Dickinson_design$location == "Rural") * 1

Design_cov_result <- cvrcov(clustername = Dickinson_design_numeric$county,
                            x = data.frame(Dickinson_design_numeric[ , c("location", "inciis", 
                                                                         "uptodateonimmunizations", 
                                                                         "hispanic", "income")]),
                            ntotal_cluster = 16,
                            ntrt_cluster = 8,
                            constraints = c("s5", "mf.5", "any", "any", "mf0.4"), 
                            categorical = c("location"),
                            savedata = "dickinson_cov_constrained.csv",
                            seed = 12345, 
                            check_validity = TRUE)
# the allocation scheme from constrained randomization
Design_cov_result$allocation

# the statement about how many clusters to be randomized to the intervention and the control arms respectively
Design_cov_result$assignment_message

# the statement about how to get the whole randomization space to use in constrained randomization
Design_cov_result$scheme_message

# the data frame containing the allocation scheme, the clustername as well as the original data frame of covariates
Design_cov_result$data_CR

# the descriptive statistics for all the variables by the two arms from the selected scheme
Design_cov_result$baseline_table

# the cluster pair descriptive, which is useful for valid randomization check
Design_cov_result$cluster_coin_des

# the overall allocation summary
Design_cov_result$overall_allocations

# Clustered permutation test: increasing up-to-date immunization rates
Analysis_result <- cptest(outcome = Dickinson_outcome$outcome,
                          clustername = Dickinson_outcome$county,
                          z = data.frame(Dickinson_outcome[ , c("location", "inciis",
                                                                "uptodateonimmunizations", "hispanic", "incomecat")]), 
                          cspacedatname = "dickinson_constrained.csv",                                 
                          outcometype = "binary",                                                      
                          categorical = c("location","incomecat"))

Analysis_result

