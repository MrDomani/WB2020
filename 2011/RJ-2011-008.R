library(grid)
grid.raster(matrix(colors()[1:100], ncol=10),
interpolate=FALSE)
grid.raster(as.raster(1:10, max=11))
grid.raster(1:10/11, interpolate=FALSE)
grid.raster(matrix(colors()[1:100], ncol=10))
download.file("http://cran.r-project.org/Rlogo.jpg",
"Rlogo.jpg")
library(ReadImages)
logo <- read.jpeg("Rlogo.jpg")
par(mar=rep(0, 4))
plot(logo)

x <- y <- seq(-4*pi, 4*pi, len=27)
r <- sqrt(outer(x^2, y^2, "+"))
z <- cos(r^2)*exp(-r/6)
image <- (z - min(z))/diff(range(z))

step <- diff(x)[1]
xrange <- range(x) + c(-step/2, step/2)
yrange <- range(y) + c(-step/2, step/2)
plot(x, y, ann=FALSE,
xlim=xrange, ylim=yrange,
xaxs="i", yaxs="i")
rasterImage(image,
xrange[1], yrange[1],
xrange[2], yrange[2],
interpolate=FALSE)
x <- c(0.00, 0.40, 0.86, 0.85, 0.69, 0.48,
0.54, 1.09, 1.11, 1.73, 2.05, 2.02)
library(lattice)
barchart(1:12 ~ x, origin=0, col="white",
panel=function(x, y, ...) {
panel.barchart(x, y, ...)
grid.raster(logo, x=0, width=x, y=y,
default.units="native",
just="left",
height=unit(2/37,
"npc"))
})
z <- matrix(runif(500*500), ncol=500)
pdf("image.pdf")
image(z, col=grey(0:99/100))
dev.off()
pdf("gridraster.pdf")
grid.raster(z, interp=FALSE)
dev.off()
file.info("image.pdf", "gridraster.pdf")["size"]
# size
# image.pdf 14893004
# gridraster.pdf 1511027
# height=unit(2/37,
system.time({
for (i in 1:10) {
image(z, col=grey(0:99/100))
}
})
# user system elapsed
# 42.017 0.188 42.484
system.time({
for (i in 1:10) {
grid.newpage()
grid.raster(z, interpolate=FALSE)
}
})
# user system elapsed
# 2.013 0.081 2.372
barchart(1:12 ~ x, origin=0, col="white", panel=function(x, y, ...) { panel.barchart(x, y, ...) grid.raster(t(1:10/11), x=0, width=x, y=y, default.units="native", just="left", height=unit(2/37, "npc")) })
library(maps)
par(mar=rep(0, 4))
map(region="Spain", col="black", fill=TRUE)
mask <- grid.cap()
library(png)
espana <- readPNG("1000px-Flag_of_Spain.png")
espanaRaster <- as.raster(espana)
espanaRaster[mask != "black"] <- "transparent"
par(mar=rep(0, 4))
map(region="Spain")
grid.raster(espanaRaster, y=1, just="top")
map(region="Spain", add=TRUE)
# //developer.r-project.org/Raster/raster-RFC.
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
# x
# 1:12
# 1
# 2
# 3
# 4
# 5
# 6
# 7
# 8
# 9
# 10
# 11
# 12
# 0.0 0.5 1.0 1.5 2.0</div>
# Sepal.Length</div>
# 4.5 5.5 6.5 7.5</div>
# 2.0 3.0 4.0</div>
# Sepal.Width</div>
# Petal.Length</div>
# 1 2 3 4 5 6 7</div>
# 0.5 1.5 2.5</div>
# Petal.Width</div>
# −10 −5 0 5 10
# −10 −5 0 5 10</div>
# (b)
