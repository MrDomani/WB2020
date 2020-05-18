library(shadow)

# Figure 4
location = rgeos::gCentroid(build)
park_location = raster::shift(location, y = 20, x = -8)
park = rgeos::gBuffer(park_location, width = 12)
angles = seq(0, 359, 5)
sun = mapply(
  shadow:::.sunLocation,
  sun_az = angles,
  MoreArgs = list(
    location = location,
    sun_elev = 10
    )
)
rays = mapply(
  ray,
  MoreArgs = list(from = location),
  to = sun
  )
rays$makeUniqueIDs = TRUE
rays = do.call(rbind, rays)
build_outline = as(build, "SpatialLinesDataFrame")
inter = rgeos::gIntersection(build_outline, rays)
opar = par(mar = rep(1, 4))
plot(build)
plot(rays, add = TRUE, col = "yellow")
plot(inter, add = TRUE, col = "red")
plot(location, add = TRUE)
par(opar)

# Figure 5
opar = par(mfrow = c(1, 2)) 
sun_az = seq(270, 90, by = -5)
sun_elev = seq(0, 90, by = 5)
solar_pos = expand.grid(sun_az = sun_az, sun_elev = sun_elev)
solar_pos$coef = coefDirect(type = "facade", facade_az = 180, solar_pos = as.matrix(solar_pos))[1, ]
coef = reshape2::acast(solar_pos, sun_az ~ sun_elev, value.var = "coef")
image(
  180 - sun_az, sun_elev, coef,
  col = rev(heat.colors(10)),
  breaks = seq(0, 1, 0.1),
  asp = 1,
  xlab = "Facade azimuth - Sun azimuth (°)",
  ylab = "Sun elevation (°)",
  main = "Facade",
  axes = FALSE
)
box()
axis(1, at = seq(-90, 90, 30))
axis(2, at = seq(-90, 90, 30))
contour(180 - sun_az, sun_elev, coef, add = TRUE)
solar_pos$coef = coefDirect(type = "roof", facade_az = 180, solar_pos = as.matrix(solar_pos))[1, ]
coef = reshape2::acast(solar_pos, sun_az ~ sun_elev, value.var = "coef")
image(
  180 - sun_az, sun_elev, coef,
  col = rev(heat.colors(10)),
  breaks = seq(0, 1, 0.1),
  asp = 1,
  xlab = "Facade azimuth - Sun azimuth (°)",
  ylab = "Sun elevation (°)",
  main = "Roof",
  axes = FALSE
)
box()
axis(1, at = seq(-90, 90, 30))
axis(2, at = seq(-90, 90, 30))
contour(180 - sun_az, sun_elev, coef, add = TRUE)
par(opar)

library(shadow)
library(raster)
library(rgeos)
location = gCentroid(build)
park_location = shift(location, y = 20, x = -8)
park = gBuffer(park_location, width = 12)

# Figure 6
opar = par(mar = rep(0, 4))
plot(build, col = "lightgrey")
text(gCentroid(build, byid = TRUE), build$BLDG_HT)
plot(park, col = "lightgreen", add = TRUE)
par(opar)

location = gCentroid(build)
time = as.POSIXct(
  x = "2004-12-24 13:30:00",
  tz = "Asia/Jerusalem"
)
location_geo = spTransform(
  x = location,
  CRSobj = "+proj=longlat +datum=WGS84"
)
library(maptools)
solar_pos = solarpos(
  crds = location_geo,
  dateTime = time
)
solar_pos
h = shadowHeight(
  location = location,
  obstacles = build,
  obstacles_height_field = "BLDG_HT",
  solar_pos = solar_pos
)
h
shadowHeight(
  location = location,
  obstacles = build,
  obstacles_height_field = "BLDG_HT",
  time = time
)

# Figure 7
sun = shadow:::.sunLocation(
  location = location,
  sun_az = solar_pos[1, 1],
  sun_elev = solar_pos[1, 2]
)
sun_ray = ray(from = location, to = sun)
build_outline = as(build, "SpatialLinesDataFrame")
inter = gIntersection(build_outline, sun_ray)
opar = par(mar = rep(1, 4))
plot(build)
plot(sun_ray, add = TRUE, lwd = 3, col = "yellow")
plot(location, add = TRUE)
text(location, paste(round(h, 2), "m"), pos = 3)
plot(inter, add = TRUE, col = "red")
text(gCentroid(build, byid = TRUE), build$BLDG_HT)
par(opar)

ext = as(extent(build) + 50, "SpatialPolygons")
r = raster(ext, res = 2)
proj4string(r) = proj4string(build)
height_surface = shadowHeight(
  location = r,
  obstacles = build,
  obstacles_height_field = "BLDG_HT",
  solar_pos = solar_pos,
  parallel = 5
)

# Figure 8
opar = par(mar = rep(1, 4))
plot(height_surface, col = grey(seq(0.9, 0.2, -0.01)), axes = FALSE, box = TRUE)
contour(height_surface, add = TRUE)
plot(build, add = TRUE, border = "red")
text(location, paste(round(h, 2), "m"), halo = TRUE, pos = 3, col = "red", font = 2)
plot(sun_ray, add = TRUE, lwd = 3, col = "yellow")
plot(inter, add = TRUE, col = "red")
plot(location, add = TRUE)
text(gCentroid(build, byid = TRUE), build$BLDG_HT)
par(opar)

grid = surfaceGrid(
  obstacles = build,
  obstacles_height_field = "BLDG_HT",
  res = 2
)
head(grid)
tail(grid)
head(coordinates(grid))
s = inShadow(
  location = grid,
  obstacles = build,
  obstacles_height_field = "BLDG_HT",
  solar_pos = solar_pos
)
dim(s)

# Figure 9
library(plot3D)
opar = par(mar = rep(0, 4))
scatter3D(
  x = coordinates(grid)[, 1],
  y = coordinates(grid)[, 2],
  z = coordinates(grid)[, 3],
  theta = 55,
  colvar = s[, 1],
  col = c("yellow", "grey"),
  pch = 16,
  scale = FALSE,
  colkey = FALSE,
  cex = 1.1
)
scatter3D(
  x = coordinates(grid)[, 1],
  y = coordinates(grid)[, 2],
  z = coordinates(grid)[, 3],
  theta = 55,
  col = "black",
  pch = 1,
  lwd = 0.1,
  scale = FALSE,
  colkey = FALSE,
  cex = 1.1,
  add = TRUE
)
par(opar)

time2 = as.POSIXct(
  x = "2004-06-24 09:30:00",
  tz = "Asia/Jerusalem"
)
solar_pos2 = solarpos(
  crds = location_geo,
  dateTime = time2
)
time2 = as.POSIXct(
  x = "2004-06-24 09:30:00",
  tz = "Asia/Jerusalem"
)
solar_pos2 = solarpos(
  crds = location_geo,
  dateTime = time2
)
solar_pos2
footprint = shadowFootprint(
  obstacles = build,
  obstacles_height_field = "BLDG_HT",
  solar_pos = solar_pos2
)
park_shadow = gIntersection(park, footprint)
shade_prop = gArea(park_shadow) / gArea(park)
shade_prop

# Figure 10
opar = par(mar = rep(0, 4))
plot(footprint, col = adjustcolor("lightgrey", alpha.f = 0.5))
plot(build, col = "darkgrey", add = TRUE)
plot(park, col = "lightgreen", add = TRUE)
plot(park_shadow, col = adjustcolor("darkgreen", alpha.f = 0.5), add = TRUE)
text(gCentroid(park_shadow), round(shade_prop, 2), halo = TRUE, font = 2, cex = 1.5, col = "red")
par(opar)

time_seq = seq(
  from = as.POSIXct("2004-06-24 03:30:00", tz = "Asia/Jerusalem"),
  to = as.POSIXct("2004-06-24 22:30:00", tz = "Asia/Jerusalem"),
  by = "1 hour"
)
solar_pos_seq = solarpos(
  crds = location_geo,
  dateTime = time_seq
)
shadow_props = rep(NA, nrow(solar_pos_seq))
for(i in 1:nrow(solar_pos_seq)) {
  if(solar_pos_seq[i, 2] < 0) shadow_props[i] = 1 else {
      footprint =
        shadowFootprint(
          obstacles = build,
          obstacles_height_field = "BLDG_HT",
          solar_pos = solar_pos_seq[i, , drop = FALSE]
          )
    park_shadow = gIntersection(park, footprint)
    if(is.null(park_shadow))
      shadow_props[i] = 0
    else
      shadow_props[i] = gArea(park_shadow) / gArea(park)
    }
}

# Figure 11
plot(
  time_seq,
  shadow_props,
  xlab = "Time",
  ylab = "Shaded proportion",
  type = "b"
)
text(
  x = time_seq[7]+4000, y = shadow_props[7] + 0.07,
  label = round(shadow_props[7], 2),
  col = "red", font = 2
)

s = SVF(
  location = location,
  obstacles = build,
  obstacles_height_field = "BLDG_HT"
)
s

svf_start = Sys.time()
svf_surface = SVF(
  location = r,
  obstacles = build,
  obstacles_height_field = "BLDG_HT",
  parallel = 5
)
svf_end = Sys.time()
svf_time = svf_end - svf_start

# Figure 12
opar = par(mar = rep(1, 4))
plot(svf_surface, axes = FALSE, box = TRUE) 
contour(svf_surface, add = TRUE)
plot(build, border = "red", add = TRUE)
plot(location, add = TRUE)
text(location, round(s, 3), halo = TRUE, pos = 3, col = "red", font = 2)
par(opar)

head(tmy, 10)
solar_pos = as.matrix(tmy[, c("sun_az", "sun_elev")])
head(solar_pos)

rad_start = Sys.time()
rad = radiation(
  grid = grid,
  obstacles = build,
  obstacles_height_field = "BLDG_HT",
  solar_pos = solar_pos,
  solar_normal = tmy$solar_normal,
  solar_diffuse = tmy$solar_diffuse,
  parallel = 5
)
rad_end = Sys.time()
rad_time = rad_end - rad_start
head(rad)

# Figure 13
opar = par(mfrow=c(3, 1), oma = c(5,4,0,0) + 0.1,
          mar = c(0,0,3,3) + 0.1)
scatter3D(
  x = coordinates(grid)[, 1],
  y = coordinates(grid)[, 2],
  z = coordinates(grid)[, 3],
  colvar = rad$direct / 1000,
  scale = FALSE,
  theta = 55,
  pch = 20,
  cex = 1.35,
  clab = expression(paste("kWh / ", m^2)),
  main = "Direct radiation"
)
scatter3D(
  x = coordinates(grid)[, 1],
  y = coordinates(grid)[, 2],
  z = coordinates(grid)[, 3],
  colvar = rad$diffuse / 1000,
  scale = FALSE,
  theta = 55,
  pch = 20,
  cex = 1.35,
  clab = expression(paste("kWh / ", m^2)),
  main = "Diffuse radiation"
)
scatter3D(
  x = coordinates(grid)[, 1],
  y = coordinates(grid)[, 2],
  z = coordinates(grid)[, 3],
  colvar = rad$total / 1000,
  scale = FALSE,
  theta = 55,
  pch = 20,
  cex = 1.35,
  clab = expression(paste("kWh / ", m^2)),
  main = "Total radiation"
)
par(opar)
