calcAnchors(method="CITRA-MCB")
◦
aoi <- createAoi(topleft = c(272955, 6085705),
bottomright = c(288195, 6073195), EPSG = 32719)
csvfile <- system.file("extdata", "apples.csv", package = "water")
MTLfile <- system.file("extdata", "L7.MTL.txt", package = "water")
WeatherStation <- read.WSdata(WSdata=csvfile,
date.format = "%d/%m/%Y",
lat = -35.42222, long = -71.38639,
elev = 201, height = 2.2,
MTL = MTLfile)
image.DN <- L7_Talca
DEM <- DEM_Talca
Energy.Balance <- METRIC.EB(image.DN = image.DN,
plain = FALSE, DEM = DEM,
WeatherStation = WeatherStation, ETp.coef = 1.2,
MTL = MTLfile, sat = "L7",
thermalband = image.DN$thermal.low)
pixel X Y Ts LAI type
1 139253 282420 -3922830 323.1587 0.13 hot
2 121566 274710 -3921780 310.0151 4.40 cold
surface.model <-METRICtopo(DEM)
solar.angles.r <- solarAngles(surface.model = surface.model,
WeatherStation = WeatherStation, MTL = MTLfile)
Rs.inc <- incSWradiation(surface.model = surface.model,
solar.angles = solar.angles.r,
WeatherStation = WeatherStation)
image.TOAr <- calcTOAr(image.DN = image.DN, sat = "L7", MTL = MTLfile,
incidence.rel = solar.angles.r\$incidence.rel)
image.SR <- calcSR(image.TOAr = image.TOAr, sat = "L7",
surface.model = surface.model,
incidence.hor = solar.angles.r\$incidence.hor,
WeatherStation = WeatherStation, ESPA = FALSE)
albedo <- albedo(image.SR=image.SR, coeff="Tasumi")
LAI <- LAI(method="metric2010", image=image.TOAr, L=0.1)
Ts <- surfaceTemperature(LAI = LAI, sat = "L7",
thermalband = image.DN\$thermal.low,
WeatherStation = WeatherStation)
Rl.out <- outLWradiation(LAI = LAI, Ts = Ts)
Rl.inc <- incLWradiation(WeatherStation, DEM = surface.model\$DEM,
solar.angles = solar.angles.r, Ts = Ts)
Rn <- netRadiation(LAI, albedo, Rs.inc, Rl.inc, Rl.out)
G <- soilHeatFlux(image = image.SR, Ts = Ts, albedo = albedo,
Rn = Rn, LAI = LAI)
Z.om <- momentumRoughnessLength(LAI = LAI, mountainous = TRUE,
method = "short.crops",
surface.model = surface.model)
hot.and.cold <- calcAnchors(image = image.TOAr, Ts = Ts,
LAI = LAI, plots = FALSE, albedo = albedo,
Z.om = Z.om, n = 1,
anchors.method = "CITRA-MCB",
deltaTemp = 5, verbose = FALSE)
H <- calcH(anchors = hot.and.cold, Ts = Ts, Z.om = Z.om,
WeatherStation = WeatherStation, ETp.coef = 1.05,
Z.om.ws = 0.0018, DEM = DEM, Rn = Rn, G = G, verbose = TRUE)
ET_WS <- dailyET(WeatherStation = WeatherStation, ET = "ETr")
ET.24 <- ET24h(Rn = Rn, G = G, H = H\$H,
Ts = Ts, WeatherStation = WeatherStation,
ETr.daily = ET_WS)
