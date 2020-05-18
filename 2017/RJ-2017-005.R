library('alineR')
aline("k@rampu", "l@m@ntUN")
# [1] 0.4864198
aline(c("batu", "k@rampuU"), c("batU", "l@m@ntUN") )
[1] 0.0000000 0.4864198
aline( w1, w2, sim = FALSE, m1 = NULL, m2 = NULL, mark = FALSE, alignment = FALSE, ...)
# multiple language comparisons
word.lists <- rbind(c("baqa", NA, "anax"), c("haqa", "dodo", "anar"),
c("abut", "suli", "oan"))
glosses <- colnames(word.lists) <- c("akar", "alir_me", "anak")
languages <- rownames(word.lists) <- c("language.1", "language.2", "language.3")
word.lists
akar alir_me anak
language.1 "baqa" NA "anax"
language.2 "haqa" "dodo" "anar"
language.3 "abut" "suli" "oan"
# dim empty matrices: a (ALINE scores), and n (a counter)
n <- matrix(0, nrow = length(languages), ncol = length(languages),
dimnames = list(languages, languages))
a <- n
# nested loops for calculating the mean ALINE Distances for multiple languages and glosses
for(i in 1:length(glosses)){ # loop glosses
for(j in 1:length(languages)){ # outer language loop
for(k in 1:length(languages)){ # inner language loop
if(j >= k){
x <- word.lists[j, i] # first word to compare
y <- word.lists[k, i] # second word to compare
if( !is.na(x) & !is.na(y) ) { # skip if missing data
a[j, k] <- a[j, k] + aline(x, y) # ALINE Distance
n[j, k] <- n[j, k] + 1 # increment counter
}
}
}
}
}
as.dist(a / n) # distance matrix composed of mean ALINE Distances
# language.1 language.2
# language.2 0.3500000
# language.3 0.3869697 0.5479798
show.map()
# IPA Aline U.Val A.Val
# 1 32
# 2 B bS 66 98 83
# 3 O oF 79 111 70
# 4 a a 97 97
# 5 b b 98 98
# ...
# 102 N 8319 78
encode.ALINE("diŋŋira", "dinnira")
# diŋŋira dinnira
# "digNgNira" "dinnira"
Invalid character: Èin ÈmlatT
˜
˜
[1] 0.07647059 0.10810811
aline("watu", "dat", alignment = TRUE, sim = TRUE)
# pair1
# w1 watu
# w2 dat
# scores 50
# a1 | - w a t | u
# a2 | d - a t | -
aline("watu", "dat", sim = TRUE) # returns similarity score for comparison
#  [1] 50
align <- raw.alignment(c("watu", "dat"))
cat(align[[3]], align[[4]], sep = "\n")
# | - w a t | u
# | d - a t | -
s <- ALINE.segments(align)
s
# [1] 0 0 15 35
sum(s)
# [1] 50
aline(c("batu", "k@rampuU"), c("batU", "l@m@ntUN"))
# [1] 0.0000000 0.4864198
aline(c("batu", "k@rampuU"), c("batU", "l@m@ntUN"), Place = 10)
# [1] 0.0000000 0.4567901
opts <- c(61, 92, 51, 26, 54, 38, 20, 40, 31, 38, 66, 72, 60) # feature weights
names(opts) <- names(formals(raw.alignment)[-1]) # add feature names
args <- c(list(w1 = c("batu", "k@rampuU"), w2 = c("batu", "l@m@ntUN")), opts)
do.call("aline", args)
[1] 0.0000000 0.5604938
training_wordlists <- rbind(c("hamate", "kanabu", "delu"), c("pameti", "penaPo", "telu"))
training_wordlists
# [,1] [,2] [,3]
# [1,] "hamate" "kanabu" "delu"
# [2,] "pameti" "penaPo" "telu"
training_set <- generate.training(raw.data = training_wordlists, search.size = 10,
table = TRUE)
linguist_determinations <- c(2, 1, 2)
optimal_set <- optimize.features(set = training_set, ranking = linguist_determinations,
num = 200, step = 50, replication = 3)
optimal_set
# [1] 69 41 47 12 40 65 71 43 48 20 54 76 51
# | p - a m e - t i | - | p e - - n a P - - | o | t - e l u |
# | p - a m e - t i | | p e - n a P - - | o | t e l u |
# | p a m e - t i | - | p e - n a P - o |
# | p a m e - t i | | p e - n a P o | -
optimal_set <- optimize.features(set = training_set, ranking = linguist_determinations,
num = 200, step = 50, replication = 3, list = TRUE)
features.plot(optimal_set) # not shown, but see Figure 3.
reps <- 4
MultiOptResult <- matrix(NA, nrow = reps, ncol = 13)
for (i in 1:reps){
MultiOptResult[i,] <- optimize.features(set = training_set,
ranking = linguist_determinations, num = 200, step = 50, replication = 3)
}
round(apply(MultiOptResult, 2, FUN = median)) # optimized feature weights
[1] 53 20 32 22 63 70 52 68 47 71 42 47 70
# ...replicate using parallelization (OSX/linux)
library(doMC)
registerDoMC(cores = 4)
reps <- 4
MultiOptResult <- foreach(i = 1:reps, .combine = rbind) %dopar% {
optimize.features(set = training_set, ranking = linguist_determinations, num = 200,
step = 50, replication = 3)
}
opts <- round(apply(MultiOptResult, 2, FUN = median)) # optimized feature weights
names(opts) <- names(formals(raw.alignment)[-1])
opts
[1] 61 92 51 26 54 38 20 40 31 38 66 72 60
list1 <- c("batu", "k@rampuU")
list2 <- c("batu", "l@m@ntUN")
aline(list1, list2)
[1] 0.0000000 0.4864198 # with default feature weights
args <- c(list(w1 = list1, w2 = list2), opts) # construct nested list for do.call()
do.call("aline", args)
[1] 0.0000000 0.5506173 # optimized Aline distances
