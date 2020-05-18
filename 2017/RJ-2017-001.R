col_types <- c(rep("integer", 8), "character", "integer", "character",
rep("integer", 5), "character", "character",
rep("integer", 4), "character", rep("integer", 6))
system.time(read.csv("2008.csv", colClasses = col_types))[["elapsed"]]
# [1] 68.257
system.time(readBin("2008.csv", "raw",
file.info("2008.csv")[["size"]]))[["elapsed"]]
# [1] 0.493
system.time(dstrsplit(readAsRaw("2008.csv"), sep = ",",
col_types = col_types))[["elapsed"]]
# [1] 14.087
system.time(read.csv.raw("2008.csv", colClasses = col_types))[["elapsed"]]
# [1] 14.251
out_file <- file("airline_mm.csv", "wb")
for (data_file in sprintf("%04d.csv", 1988:2008)) {
df <- read.csv.raw(data_file, col_types = col_types)
df$DayOfWeek <- factor(df$DayOfWeek, levels = 1:7)
df$Month <- factor(df$Month, levels = 1:12)
df$DepTime <- sprintf("%04d", df$DepTime)
df$DepTime <- as.numeric(substr(df$DepTime, 1, 2)) * 60 +
as.numeric(substr(df$DepTime, 3, 4))

mf <- model.frame(ArrDelay ~ DayOfWeek + DepTime + DepDelay + Month, df)
mm <- cbind(model.response(mf), model.matrix(mf, df))
rownames(mm) <- NULL
writeBin(as.output(mm, sep = ","), out_file)
}
mm_names <- colnames(mm)
close(out_file)
ne_chunks <- chunk.apply("airline_mm.csv",
function(x) {
mm <- mstrsplit(x, sep = ",", type= " numeric")
colnames(mm) <- mm_names
list(xtx = crossprod(mm[, -1]),
xty = crossprod(mm[, -1], mm[, 1, drop = FALSE]))
}, CH.MERGE = list)
xtx <- Reduce("+", Map(function(x) x$xtx, ne_chunks))
xty <- Reduce("+", Map(function(x) x$xty, ne_chunks))
qr.solve(xtx, xty)
# [,1]
# (Intercept) 0.5564085990
# DayOfWeek2 0.5720431343
# DayOfWeek3 0.8480978666
# DayOfWeek4 1.2436976583
# DayOfWeek5 1.0805744488
# DayOfWeek6 -1.2235684080
# DayOfWeek7 -0.9883340887
# DepTime 0.0003022008
# DepDelay 0.9329374752
# Month2 0.2880436452
# Month3 -0.2198123852
# ...
# ●
# ●
# ●
# ●
# ●
# ●
# ●
# ●
# ●
# ●
# ●
# ●
# ●
# ●
# ●
# ●
# ●
# ●</div>
# 100
# 200
# 300
# 400
# 2 4 6
# Number of Cores
# Time (in seconds)
# Function
# One Reader Multi−Process
# Multi−Readers Multi−Process
# Pipeline Parallel</div>
# b
