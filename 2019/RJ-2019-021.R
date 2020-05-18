### Codes used in Section "Application" ----------------------------------------


### Load ConvergenceClubs package ---
library(ConvergenceClubs)


### Load GDP data ---
data("GDP")


## Filter data
logGDP <- log(GDP[,-1])
filteredGDP <- apply(logGDP, 1,
                     function(x){ mFilter::hpfilter(x, freq=400, type="lambda")$trend })
filteredGDP <- data.frame(Countries = GDP[,1], t(filteredGDP), stringsAsFactors=FALSE )
colnames(filteredGDP) <- colnames(GDP)

# Filtered data are available in the package
data(filteredGDP)



### Clustering procedure ---

### log-t test over all units
H <- computeH(filteredGDP[,-1], quantity = "H")
round(estimateMod(H, time_trim=1/3, HACmethod = "FQSB"), 3)


### Cluster Countries using GDP from year 1970 to year 2003, with 2003 as reference year
clubs <- findClubs(filteredGDP, dataCols=2:35, unit_names = 1, refCol=35,
                   time_trim=1/3, cstar=0, HACmethod = 'FQSB')

class(clubs)
str(clubs, give.attr=FALSE)


summary(clubs)


## Print results
print(clubs)
# or just
clubs



# Plot Transition Paths for all regions in each club and average Transition Paths
# for all clubs
plot(clubs)

# Plot only clubs 1:4
plot(clubs, avgTP = FALSE, nrows = 4, ncols = 2)

# Plot Only average Transition Paths of each clubs
plot(clubs, clubs=NULL, avgTP = TRUE, legend=TRUE)



# Merge clusters using Phillips and Sul (2009) method
mclubs <- mergeClubs(clubs, mergeMethod='PS')
summary(mclubs)





