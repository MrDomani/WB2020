# setParameter()
data(pop, package = "sparkTable")
pop_ges <- pop$value[pop$variable == "Insgesamt"]
sline <- newSparkLine(values = pop_ges)
slineIQR <- newSparkLine(values = pop_ges, showIQR = TRUE)
export(object = sline, outputType = "pdf",
filename = file.path("figures", "first-sl"))
export(object = slineIQR, outputType = "pdf",
filename = file.path("figures", "first-sl-IQR"))
v1 <- as.numeric(table(rpois(50, 2))); v2 <- rnorm(50)
sbar <- newSparkBar(values = v1, barSpacingPerc = 5)
sbox <- newSparkBox(values = v2, boxCol = c("black", "darkblue"))
shist <- newSparkHist(values = v2, barCol = c("black", "darkgreen", "black"))
export(object = sbar, outputType = "pdf",
filename = file.path("figures", "first-bar"))
export(object = sbox, outputType = "pdf",
filename = file.path("figures", "first-box"))
export(object = shist, outputType = "pdf",
filename = file.path("figures", "first-hist"))
sline2 <- setParameter(sline, type = "lineWidth", value = 2)
sline2 <- setParameter(sline2, type = "pointWidth", value = 5)
export(object = sline2, outputType = "pdf",
filename = file.path("figures", "second-sl"))
# req uire ( spark Table )
# sl <- newS parkLi ne ( values = rnorm (25) , lineW idth = .18 , p ointW idth = .4 ,
# width = .4 , hei ght = .08)
# export ( sl , outp utTyp e = " png " , filename = " spa rkLin e ")
# This is a spar kline in clud ed in the ![ first S parkLi n e ]( s park Line . png )
# text ...
# load data (in long format)
data(AT_Soccer, package = "sparkTable")
# first three observations to see the structure of the data
head(AT_Soccer, 3)
# team time points wl goaldiff shotgoal getgoal
# 1 Rapid 1 1 0 0 2 2
# 2 Rapid 2 3 1 4 4 0
# 3 Rapid 3 3 1 2 4 2
# prepare content
content <- list(
function(x) { sum(x) },
function(x) { round(sum(x), 2) },
function(x) { round(sum(x), 2) },
newSparkLine(lineWidth = 2, pointWidth = 6), newSparkBar()
)
names(content) <- c("Points", "ShotGoal", "GetGoal", "GoalDiff", "WinLose")

# set variables
vars <- c("points", "shotgoal", "getgoal", "goaldiff", "wl")

# create the sparkTable object
stab <- newSparkTable(dataObj = AT_Soccer, tableContent = content, varType = vars)
export(stab, outputType = "tex", filename = file.path("figures", "first-stab"),
graphNames = file.path("figures", "first-stab"))
data(popEU, package = "sparkTable")
data(debtEU, package = "sparkTable")
data(coordsEU, package = "sparkTable")
popEU <- popEU[popEU$country %in% coordsEU$country, ]
debtEU <- debtEU[debtEU$country %in% coordsEU$country, ]
EU <- cbind(popEU, debtEU[, -1])
head(EU, 3)
# country B1999 B2000 B2001 B2002 B2003 B2004 B2005
# 1 BE 10213752 10239085 10263414 10309725 10355844 10396421 10445852
# 2 BG 8230371 8190876 8149468 7891095 7845841 7801273 7761049
# 3 CZ 10289621 10278098 10266546 10206436 10203269 10211455 10220577
# B2006 B2007 B2008 B2009 B2010 X1999 X2000 X2001 X2002
# 1 10511382 10584534 10666866 10753080 10839905 113.7 107.9 106.6 103.5
# 2 7718750 7679290 7640238 7606551 7563710 77.6 72.5 66.0 52.4
# 3 10251079 10287189 10381130 10467542 10506813 16.4 18.5 24.9 28.2
# X2003 X2004 X2005 X2006 X2007 X2008 X2009 X2010
# 1 98.5 94.2 92.1 88.1 84.2 89.6 96.2 96.8
# 2 44.4 37.0 27.5 21.6 17.2 13.7 14.6 16.2
# 3 29.8 30.1 29.7 29.4 29.0 30.0 35.3 38.5
EUlong <- reshapeExt(EU, idvar = "country", v.names = c("pop", "debt"),
varying = list(2:13, 14:25), geographicVar = "country", timeValues = 1999:2010)
head(EUlong, 2)
# $BE
# country time pop debt
# BE.1 BE 1999 10213752 113.7
# BE.2 BE 2000 10239085 107.9
# BE.3 BE 2001 10263414 106.6
# BE.4 BE 2002 10309725 103.5
# BE.5 BE 2003 10355844 98.5
# BE.6 BE 2004 10396421 94.2
# BE.7 BE 2005 10445852 92.1
# BE.8 BE 2006 10511382 88.1
# BE.9 BE 2007 10584534 84.2
# BE.10 BE 2008 10666866 89.6
# BE.11 BE 2009 10753080 96.2
# BE.12 BE 2010 10839905 96.8
# $BG
# country time pop debt
# BG.1 BG 1999 8230371 77.6
# BG.2 BG 2000 8190876 72.5
# BG.3 BG 2001 8149468 66.0
# BG.4 BG 2002 7891095 52.4
# BG.5 BG 2003 7845841 44.4
# BG.6 BG 2004 7801273 37.0
# BG.7 BG 2005 7761049 27.5
# BG.8 BG 2006 7718750 21.6
# BG.9 BG 2007 7679290 17.2
# BG.10 BG 2008 7640238 13.7
# BG.11 BG 2009 7606551 14.6
# BG.12 BG 2010 7563710 16.2
l <- newSparkLine(lineWidth = 3, pointWidth = 10)
content <- list(function(x) { "Population:" }, l, function(x) {"Debt:" }, l)
varType <- c(rep("pop", 2), rep("debt", 2))
xGeoEU <- newGeoTable(EUlong, content, varType, geographicVar = "country",
geographicInfo = coordsEU)
export(xGeoEU, outputType = "tex", graphNames = file.path("figures", "out1"),
filename = file.path("figures", "testEUT"), transpose = TRUE)
# ```{r , echo = TRUE }
# ```
