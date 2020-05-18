library(diverse)
data("pantheon")
str(pantheon)
# 'data.frame': 119 obs. of 3 variables:
# $ Country : Factor w/ 10 levels "Canada","Chile",..: 10 5 4 9 2 8 7 6 3 1 ...
# $ Occupation: Factor w/ 52 levels "Actor","Architect",..: 40 40 40 40 40 40 40 40 40 40
# $ Value : int 6 2 8 5 10 9 17 36 38 10 ...
str(scidat)
# num [1:10, 1:27] 3507 35351 15603 1346 4158 ...
# - attr(*, "dimnames")=List of 2
# ..$ : chr [1:10] "Argentina" "China" "Germany" "Hungary" ...
# ..$ : chr [1:27] "Agricultural and Biological Sciences" "Arts and Humanities"...
str(geese)
# num [1:4, 1:11] 274 10788 4786 39273 247 ...
# - attr(*, "dimnames")=List of 2
# ..$ : chr [1:4] "Little Grebe" "Crested Grebe" "Mute Swan" "Greylag Goose"
# ..$ : chr [1:11] "1996" "1997" "1998" "1999" ...
library(pheatmap)
colfunc <- colorRampPalette(c("deepskyblue4", "deepskyblue", "cyan"))
plot_mat <- function(data)
pheatmap(data, colfunc(100), cluster_rows = FALSE, cluster_cols = FALSE)
col_l <- names(sort(colSums(values(scidat)))) #order
row_l <- names(sort(rowSums(values(scidat)), decreasing = TRUE))
plot_mat(values(scidat)[row_l,col_l])
plot_mat(values(scidat, norm = 'p')[row_l,col_l])
plot_mat(values(scidat, norm = 'rca')[row_l,col_l])
plot_mat(values(scidat, norm = 'rca', filter = 1)[row_l,col_l])
variety(data = pantheon)
# variety
# Canada 27
# China 24
# ...
# Uruguay 4
# Vietnam 4
#using function values() to normalize the dataset
scidat_rca_fil <- values(data = scidat, norm = 'rca', filter = 1)
variety(scidat_rca_fil)
# variety
# United States 17
# Germany 16
# ...
# China 10
# Mexico 9
ubiquity(data = pantheon)
# ubiquity
# Politician 10
# Writer 8
# Soccer.Player 6
# ...
# Referee 1
# Wrestler 1
round(diversity(data = pantheon, type = c('hhi', 'gs', 'b','ev')), 3)
# HHI gini.simpson gini.simpson.C gini.simpson.R blau.index evenness
# Canada 0.372 0.628 0.372 2.689 0.628 0.843
# Chile 0.133 0.867 0.133 7.538 0.867 0.959
# ...
# Uruguay 0.235 0.765 0.235 4.263 0.765 0.820
# Vietnam 0.139 0.861 0.139
bal <- balance(geese, category_row = TRUE) #note the function balance
barplot(t(bal), legend = TRUE, xlab = "Years", ylab = "Proportions",
col=c("darkblue","blue","sky blue", "light blue") )
bp <- diversity(geese, type = 'bp', category_row = TRUE)
plot(bp$berger.parker.I~rownames(bp), xlab = "Years",
ylab = "Berger-Parker Index of Diversity", pch = 19, col = "brown")
diversity(data = geese, type = c('e','ev','s','bp'), category_row = TRUE)
# entropy evenness simpson.D simpson.I simpson.R berger.parker.D berger.parker.I
# 1996 0.7993160 0.5765846 0.5534977 0.4465023 1.806692 0.7124871 1.403534
# 1997 0.7764028 0.5600563 0.5674638 0.4325362 1.762227 0.7247953 1.379700
# ...
# 2005 0.5790954 0.4177290 0.7160910 0.2839090 1.396471 0.8392823 1.191494
# 2006 0.5633026 0.4063369 0.7245616 0.2754384 1.380145 0.8446653 1.183901
diversity(pantheon, type=c("v","hcdt","hn","re"), q=0)[1,]
# variety hcdt.entropy hill.numbers renyi.entropy
# Canada 27 27 27 3.295837
diversity(pantheon, type=c("e","re","hcdt", "hn"), q=1)[2,]
# entropy renyi.entropy hcdt.entropy hill.numbers
# Chile 1.626709 1.626709 1.626709 5.087107
diversity(pantheon, type=c("hcdt","gs","hn"), q=2)[3,]
# hcdt.entropy gini.simpson gini.simpson.C gini.simpson.R hill.numbers
# China 0.8168554 0.8168554 0.1831446 5.460167 5.460167
adj <- dis_categories(data = scidat, method = 'cosine')
adj[adj > 0.015] <- 0 #filter
library(igraph)
g <- graph.adjacency(adjmatrix = adj, mode = 'undirected', weighted = TRUE)
totals <- colSums(values(scidat))
V(g)$size = log(totals[match(V(g)$name, names(totals))], base = 2) - 9
fc <- fastgreedy.community(g); colors <- rainbow(max(membership(fc)))
V(g)$color = colors[membership(fc)]
set.seed(67)
g$layout <- layout.fruchterman.reingold(g)
plot.igraph(g, vertex.label.cex = 0.9, vertex.label.font = 0,
vertex.label.family = 'Helvetica', vertex.label.color='black', asp = FALSE)
round(dis_entities(scidat, method = 'cosine'), 2)
# Argentina China Germany Hungary Iran Mexico Singapore...
# Argentina 0.00 0.32 0.09 0.07 0.17 0.04 0.25
# China 0.32 0.00 0.20 0.19 0.06 0.18 0.06
# Germany 0.09 0.20 0.00 0.02 0.07 0.05 0.10
# Hungary 0.07 0.19 0.02 0.00 0.07 0.03 0.11
# Iran 0.17 0.06 0.07 0.07 0.00 0.07 0.05
# Mexico 0.04 0.18 0.05 0.03 0.07 0.00 0.13
# ...
scidat_rca_fil <- values(scidat, norm = 'rca', filter = 1)
disparity(scidat_rca_fil)
# disparity.sum disparity.mean
# Argentina 121.12704 0.3450913
# China 54.86895 0.1563218
# ...
# Spain 147.35440 0.4198131
# United States 190.86552 0.5437764
scidat_rca_fil <- values(scidat, norm = 'rca', filter = 1)
diversity(data = scidat_rca_fil, type = c('rao', 'rs') ,alpha=0.7, beta = 0.3, method = 'cosine')
# rao.stirling rao
# rao rao.stirling
# Argentina 0.1526983 7.072576
# China 0.1346935 4.814975
# ...
# Spain 0.2137356 12.842799
# United States 0.1874783 12.864261
set.seed(99)
synt_ind <- sim_individuals(n_categ=50, size=100000,
category_prefix='ctg', type='log-normal', mean=0.507, sd=1.183)
hist(table(synt_ind), breaks = 30, xlab = "Values of abundance",
probability = TRUE, main = NULL)
lines(density(table(synt_ind)), col="red")
library(fitdistrplus)
f <- fitdist(as.vector(table(synt_ind)), "lnorm")
x = rlnorm(50, mean=f$estimate['meanlog'][[1]], sd = f$estimate['sdlog'][[1]])
lines(density(x), col="blue", lwd=2)
legend("topright",legend = c('Empirical', 'Fitted'), col = c("red", "blue"), lty=1)
head(synt_ind)
# [1] "ctg49" "ctg3" "ctg41" "ctg49" "ctg4" "ctg25"
sim_ent <- sim_entity(n_categ=200, values=sample(1:1000, replace=TRUE))
plot(sim_ent$Value, ylab = "Value of abundance", xlab="Categories")
head(sim_ent)
# Category Value
# 1 1 757
# 2 2 124
# ...
n_entities <- 1500
v_values <- sample(10:5000, size= n_entities, replace=TRUE)
v_n_categ <- sample(1:100, size = n_entities, replace=TRUE)
data_set <- sim_dataset(n_categ = v_n_categ, values= v_values,
category_prefix = "C", category_random = TRUE)
pheatmap(values(data_set),cluster_rows = FALSE, cluster_cols = FALSE,
show_rownames = FALSE, show_colnames = FALSE)
head(data_set)
# ...
system.time(data_set <- sim_dataset(n_categ = v_n_categ, values= v_values,
category_prefix = "C", entity_prefix = "E"))
# user system elapsed
# 29.590 5.245 34.871
system.time(diversity(data_set, type=c("e")))
# user system elapsed
# 0.019 0.001 0.021
system.time(diversity(data_set, type=c("rs")))
# user system elapsed
# 2.478 0.206 2.697
library(entropart)
abundance <- to_entropart(data_set)
mc <- MetaCommunity(abundance)
Richness(mc$Ps);Shannon(mc$Ps)
d_1 <- diversity(scidat, type="e")
d_2 <- diversity(scidat_2, type="e")
cor.test(d_1[,1], d_2[,1])
# Pearson's product-moment correlation
# data: d_1[, 1] and d_2[, 1]
# t = 3.7171, df = 8, p-value = 0.005896
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.3330683 0.9496172
# sample estimates:
# cor
# 0.795807
