install.packages("rnrfa")
install.packages("devtools")
devtools::install_github("cvitolo/rnrfa")
library(rnrfa)
allStations <- catalogue()
# Option a: from OS grid reference to WGS84
osg_parse(gridRef = "NC581062", CoordSystem = "WGS84")
# Option b: from OS grid reference to BNG
osg_parse(gridRef = "NC581062", CoordSystem = "BNG")
# Define a bounding box.
bbox <- list(lonMin = -3.76, latMin = 52.43, lonMax = -3.67, latMax = 52.48)
# Filter stations based on bounding box.
someStations <- catalogue(bbox)
# Map
library(ggmap)
library(ggrepel)
ggmap(m) + geom_point(data = someStations, aes(x = lon, y = lat),
col = "red", size = 3, alpha = 0.5) +
geom_text_repel(data = someStations, aes(x = lon, y = lat, label = name),
size = 3, col = "red")
# Select stations with more than 100 years of recordings.
s100Y <- catalogue(minRec = 100, all = FALSE)
# Print s100Y to the screen.
s100Y
# id name river catchmentArea lat lon
# 636 38001 Lee at Feildes Weir Lee 1036 51.76334 0.01277874
# 665 39001 Thames at Kingston Thames 9948 51.41501 -0.30887638
# 1130 55032 Elan at Caban Dam Elan 184 52.26907 -3.57239164
stationsWye <- catalogue(columnName = "haName", columnValue = "Wye (Hereford)")
stations1KM <- catalogue(columnName = "catchmentArea", columnValue = "<1")
catalogue(bbox, columnName = "haName", columnValue = "Wye (Hereford)",
minRec = 50, all = FALSE)
# id name river catchmentArea lat lon
# 6 55008 Wye at Cefn Brwyn Wye 10.6 52.43958 -3.724108
flow <- gdf(id = "54090")
# http://nrfaapps.ceh.ac.uk/nrfa/xml/waterml2?db=nrfa_public&stn=54090&dt=gdf
# Get gauged daily flow for station 54090.
flow <- gdf(id = "54090")
# Convert to csv.
write.csv(as.data.frame(flow), "flowDF.csv", quote = FALSE)
rain <- cmr(id = "54090", metadata = TRUE)
data <- rain$data
meta <- rain$meta
plot(data, main = paste(meta$variable, "-", meta$stationName),
xlab = "", ylab = meta$units)
plot_rain_flow(id = "54090")
library(microbenchmark)
library(parallel)
cl <- makeCluster(getOption("cl.cores", 9))
microbenchmark(# sequential requests
gdf(id = someStations$id, metadata = FALSE, cl = NULL),
# concurrent requests
gdf(id = someStations$id, metadata = FALSE, cl = cl), times = 10)
stopCluster(cl)
# http://www.metoffice.gov.uk/climate/uk/interesting/2011_spring
