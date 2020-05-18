mapDevice() #create world map shaped window
mapBubbles(dF=getMap(),nameZSize="POP2005",nameZColour="REGION",colourPalette="rainbow",oceanCol="lightblue",landCol="wheat")



library(rworldmap)
data(countryExData)
sPDF <- joinCountryData2Map( countryExData
,joinCode = "ISO3"
,nameJoinColumn = "ISO3V10")
mapDevice() #create world map shaped window
mapBubbles(dF=getMap()
,nameZSize="POP2005"
,nameZColour="REGION"
,colourPalette="rainbow"
,oceanCol="lightblue")
data(gridExData)
mapDevice() #create world map shaped window
mapGriddedData(gridExData)
identifyCountries(getMap()
,nameColumnToPlot="POP2005")
data(countryExData)
country2Region(countryExData
,nameDataColumn="CLIMATE"
,joinCode="ISO3"
,nameJoinColumn="ISO3V10"
,regionType="Stern"
,FUN="mean")
# Australasia 56.92000
# Caribbean 65.20000
# Central America 76.11250
# Central Asia 56.18000
# East Asia 69.18462
# Europe 73.87619
# North Africa 71.00000
# North America 62.70000
# South America 77.01818
# South Asia 77.22000
# South+E Africa 75.79474
# West Africa 78.68421
# West Asia 49.62000
data(countryExData)
mapDevice() #create world map shaped window
mapByRegion(countryExData
,nameDataColumn="CLIMATE"
,joinCode="ISO3"
,nameJoinColumn="ISO3V10"
,regionType="Stern"
,FUN="mean")
data(countryRegions)
sPDF <- joinCountryData2Map( countryRegions
,joinCode = "ISO3"
,nameJoinColumn = "ISO3")
mapDevice() #create world map shaped window
mapCountryData(sPDF[which(sPDF$LDC=='LDC'),]
,nameColumnToPlot="POP2005")
,ifelse(dF$LifeSat > 7.0,'green'
,'amber' ))
ifelse(dF$HLY < 33,'red'
,ifelse(dF$HLY > 52.5,'green'
,'amber' ))
library(classInt)
library(RColorBrewer)
#getting smallexample data and joining to a map
data(countryExData)
sPDF <- joinCountryData2Map(countryExData
,joinCode = "ISO3"
,nameJoinColumn = "ISO3V10"
,mapResolution = "coarse")
#getting class intervals
classInt <- classIntervals( sPDF[["EPI"]]
,n=5, style = "jenks")
catMethod = classInt[["brks"]]
#getting colours
colourPalette <- brewer.pal(5,'RdPu')
#plot map
mapDevice() #create world map shaped window
mapParams <- mapCountryData(sPDF
,nameColumnToPlot="EPI"
,addLegend=FALSE
,catMethod = catMethod
,colourPalette=colourPalette )
#adding legend
do.call(addMapLegend
,c(mapParams
,legendLabels="all"
,legendWidth=0.5
,legendIntervals="data"
,legendMar = 2))
inFile <- 'hpi2_0edited2.csv'
dF <- read.csv(inFile,header=TRUE,as.is=TRUE)
sPDF <- joinCountryData2Map(dF
, joinCode='NAME'
, nameJoinColumn='country'
, verbose='TRUE')
#categorise component indices
dF$LifeSatcolour <-
ifelse(dF$LifeSat < 5.5,'red'
,'amber' ))
dF$LifeExpcolour <-
ifelse(dF$LifeExp < 60,'red'
,ifelse(dF$LifeExp > 75,'green'
dF$HLYcolour <-
dF$Footprintcolour <-
ifelse(dF$Footprint > 8.4,'blood red'
,ifelse(dF$Footprint > 4.2,'red'
,ifelse(dF$Footprint < 2.1,'green'
,'amber' )))
#count red, amber , greens per country
numReds<-
(as.numeric(dF$Footprintcolour=='red')
+as.numeric(dF$LifeExpcolour=='red')
+as.numeric(dF$LifeSatcolour=='red'))
numAmbers<-
(as.numeric(dF$Footprintcolour=='amber')
+as.numeric(dF$LifeExpcolour=='amber')
numGreens<-
(as.numeric(dF$Footprintcolour=='green')
+as.numeric(dF$LifeSatcolour=='green'))
#calculate HPI colour per country
dF$HPIcolour <-
ifelse(dF$Footprintcolour=='blood red'
| numReds>1,6
,ifelse(numReds==1,5
,ifelse(numGreens==1 & numAmbers==2,3
,ifelse(numGreens==2 & numAmbers==1,2
,ifelse(numGreens==3,1
,NA))))))
#join data to map
sPDF <- joinCountryData2Map(dF
+as.numeric(dF$LifeSatcolour=='amber'))
+as.numeric(dF$LifeExpcolour=='green')
,ifelse(numAmbers==3,4
,joinCode="NAME"
,nameJoinColumn="country")
#set colours
colourPalette <- c('palegreen'
,'yellow'
,'orange'
,'orangered'
,'darkred')
#plot map
mapDevice() #create world map shaped window
mapParams <- mapCountryData(sPDF
,nameColumnToPlot='HPIcolour'
,catMethod='categorical'
,colourPalette=colourPalette
,addLegend=FALSE
,mapTitle='Happy Planet Index')
#changing legendText
mapParams$legendText <-
c('2 good, 1 middle'
,'1 good, 2 middle'
,'3 middle'
,'1 poor'
,'2 poor or footprint v.poor')
#add legend
do.call( addMapLegendBoxes
, c(mapParams
,x='bottom'
,title="HPI colour"))
inFile1 <- 'Koeppen-Geiger-ASCII.txt'
#read in data which is as lon,lat,catID
dF<-read.table(inFile1,header=TRUE,as.is=TRUE)
#convert to sp SpatialPointsDataFrame
coordinates(dF) = c("Lon", "Lat")
# promote to SpatialPixelsDataFrame
gridded(dF) <- TRUE
# promote to SpatialGridDataFrame
sGDF = as(dF, "SpatialGridDataFrame")
#plotting map
mapDevice() #create world map shaped window
mapParams <- mapGriddedData(sGDF
,catMethod='categorical'
,addLegend=FALSE)
#adding formatted legend
do.call(addMapLegendBoxes
,c(mapParams
,cex=0.8
,ncol=10
,x='bottom'
,title='Koeppen-Geiger Climate Zones'))
org/cgi-bin/downl/ar4_nc/tas/HADCM3_SRA1B_1_
offset = c(min(nc$dim$longitude$vals)
offset = offset + cellsize/2
cells.dim = c(nc$dim$longitude$len
gt <- GridTopology(cellcentre.offset = offset
, cellsize = cellsize
tas.html
# creating gridTopology from the netCDF metadata
,min(nc$dim$latitude$vals))
, abs(diff(nc$dim$latitude$vals[1:2])))
# add cellsize/2 to offset
# to convert from lower left referencing to centre
,nc$dim$latitude$len )
#reading in colour palette
#as.is=T stops conversion to factors
#which otherwise messes up colours
tst <- read.csv('paletteSaved.csv',as.is=TRUE)
#plotting map
mapDevice() #create world map shaped window
#tst$x passes the palette as a vector
mapParams <- mapGriddedData(sGDF
,catMethod='categorical'
,addLegend=FALSE
,colourPalette=tst$x)
#adding legend
do.call(addMapLegendBoxes
,c(mapParams
,cex=0.8
,ncol=3
,x='bottomleft'
,title='Koeppen-Geiger Climate Zones'))
library(ncdf)
#the downloaded file
inFile <-
memory.limit(4000) #set memory limit to max
nc = open.ncdf(inFile, write=FALSE)
file ... has 4 dimensions:
time Size: 12
latitude Size: 73
longitude Size: 96
bounds Size: 2
------------------------
file ... has 4 variables
float climatological_bounds[bounds,time]
float latitude_bounds[bounds,latitude]
float longitude_bounds[bounds,longitude]
float
air_temperature_anomaly[longitude,latitude,time]
ncArray =
get.var.ncdf(nc,'air_temperature_anomaly')
cellsize = c( abs(diff(nc$dim$longitude$vals[1:2]))
southandy@gmail.com
mapDevice()
#creating a vector to classify the data
catMethod=seq(from=-5,to=19,by=2)
#creating a colourPalette for all plots
#-ve blue, 0 white, +ve yellow to red
colourPalette=c('blue','lightblue','white'
,brewer.pal(9,'YlOrRd'))
#looping for each month
for( zDim in 1 : nc$dim$time$len ){
#reading the values for this month
ncMatrix <- ncArray[,,zDim]
#to get the image up the right way
#this reverses the y values but not the x ones
gridVals <-data.frame(att=as.vector(ncMatrix2))
#creating a spatialGridDataFrame
#plotting the map and getting params for legend
mapParams <- mapGriddedData( sGDF
,nameColumnToPlot='att'
,catMethod=catMethod
,colourPalette=colourPalette
,addLegend=FALSE )
do.call(addMapLegend
,c(mapParams
,legendLabels="all"
,legendWidth=0.5
,legendMar = 3))
title(paste('month :',zDim))#adding brief title
ncMatrix2 <-ncMatrix[ ,nc$dim$latitude$len:1 ]
sGDF <-SpatialGridDataFrame(gt, data=gridVals)
#adding formatted legend
outputPlotType = 'png'
savePlot(paste("ipccAirAnomalyMonth",zDim,sep='')
} #end of month loop
close.ncdf(nc) #closing the ncdf file
,type=outputPlotType)
web/packages/rworldmap/vignettes/rworldmap.
web/packages/rworldmapvignettes/rworldmapFAQ.
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
●
POP2005
0
6.56e+08
1.31e+09
REGION
0
2
9
19
142
150</div>
'HADCM3_SRA1B_1_tas-change_2046-2065.cyto180.nc'
BIODIVERSITY</div>
EPI
