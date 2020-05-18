as.one.file(files,
filename = "allthreads.txt",
list = "rdevel")
forest <- makeforest(month = "allthreads")
head(forest)
# emailID threadID
# [1,] "1" "1"
# [2,] "2" "2"
# [3,] "3" "2"
# [4,] "4" "3"
# [5,] "5" "2"
# [6,] "6" "3"
# author
# [1,] "ggrothendieck at gmail.com (Gabor..."
# [2,] "huber at ebi.ac.uk (Wolfgang Hube..."
# [3,] "ripley at stats.ox.ac.uk (Prof Br..."
# [4,] "dfaden at gmail.com (David Faden)..."
# [5,] "ripley at stats.ox.ac.uk (Prof Br..."
# [6,] "ripley at stats.ox.ac.uk (Prof Br..."
# subjects
# [1,] "[Rd] Wish List"
# [2,] "[Rd] Error from wilcox.test"
# [3,] "[Rd] Error from wilcox.test"
# [4,] "[Rd] setting the seed in standalo..."
# [5,] "[Rd] Error from wilcox.test"
# [6,] "[Rd] setting the seed in standalo..."
# content
# [1,] "In: trunk/src/library/base/ma..."
# [2,] "On 1/3/2008 9:03 AM, Gabor Grothe..."
# [3,] "What it is trying is % env R_DEF..."
# [4,] "On 1/4/08, Prof Brian Ripley <rip..."
# [5,] "Full_Name: Hendrik Weisser Versio..."
# [6,] "hendrik.weisser at gmx.net wrote:..."
network <- adjacency(createedges(forest))
network[1:2, 1:2]
# Gabor G. Brian R.
# Gabor G. 130 29
# Brian R. 19 250
network <- adjacency(createedges(
forest,
subjectfilter = "lattice"))
twomode <- adjacency(centrality.edgelist(
terms, apply.to = "subjects"))
shrink(twomode, keep = people,
values = "min")
# Figure 1
gplot.snatm(network, vertex.col = "white",
vertex.border = "grey",
label = rownames(network),
boxed.labels = FALSE,
label.pos = 5,
label.cex =
((sna::degree(network)+1)^0.45)*0.25,
gmode = "graph", edge.lwd = 0.1,
edge.col = "grey")
# Figure 2
# See how to get peoplelist in the snatm demo.
people <- which(is.element(rownames(twomode)
,unique(peoplelist)))
labelcol <- rep(rgb(0,0,1,0.75),dim(twomode)[1])
labelcol[people] <- "red"
gplot.snatm(twomode, gmode = "graph",
vertex.col = "white", vertex.cex = 1,
label = rownames(twomode),
label.col = labelcol,
label.cex =
(sna::degree(twomode)^0.25)*0.35,
label.pos = 5, edge.lwd = 0.1,
boxed.labels = FALSE,
vertex.border = "white",edge.col = "grey")
# http://www-nlpir.nist.gov/projects/duc/
# de/servlets/DerivateServlet/Derivate-8825/
# http://mfwallace.googlepages.com/jawbone.
# angela.bohn@gmail.com
# kurt.hornik@wu-wien.ac.at
# patrick.mair@wu.ac.at
# Number of answers (log scale)</div>
