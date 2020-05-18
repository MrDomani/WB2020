library(ggplot2)
# background for various plots later
bg <- ggplot(NULL, aes(x = 1, y = 1)) + ylim(0.8, 1.2) +
theme(axis.title = element_blank(), axis.ticks = element_blank(),
axis.text = element_blank())

if(capabilities("cairo")) {
png("builtin-1.png", 672, 384, type = "cairo", res = 96)
txt1 <- annotate("text", 1, 1, label = "A sample of\nDejaVu Sans Mono",
family = "DejaVu Sans Mono", size = 15)
print(bg + txt1)
dev.off()

cairo_pdf("builtin-2.pdf", 7, 4)
# http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html
# http://cairographics.org/
txt2 <- annotate("text", 1, 1, label = "A sample of\nDejaVu Serif Italic",
family = "DejaVu Serif", fontface = "italic", size = 15)
print(bg + txt2)
dev.off()
}
if(.Platform$OS.type == "windows") {
windowsFonts(century = "Century Gothic")
png("builtin-3.png", 672, 384, res = 96)
txt3 <- annotate("text", 1, 1, label = "A sample of\nCentury Gothic",
family = "century", size = 20)
print(bg + txt3)
dev.off()
}
# http://www.ghostscript.com/
library(Cairo)

CairoFonts(regular = "Liberation Sans:style=Regular",
italic = "Liberation Serif:style=Italic")

CairoPDF("Cairo-1.pdf", 7, 4)
txt4 <- annotate("text", 1, 1.1, label = "A sample of Liberation Sans", size = 12)
txt5 <- annotate("text", 1, 0.9, label = "A sample of Liberation Serif",
fontface = "italic", size = 12)
print(bg + txt4 + txt5)
dev.off()
if(.Platform$OS.type == "windows") {
CairoPDF("Cairo-2.pdf", 7, 4)
txt6 <- annotate("text", 1, 1.1, label = "A sample of Constantia",
family = "Constantia", size = 12)
txt7 <- annotate("text", 1, 0.9, label = "A sample of Lucida Console",
family = "Lucida Console", size = 10)
print(bg + txt6 + txt7)
dev.off()
}
library(extrafont)
## Run once
font_import()
loadfonts()
library(extrafont)
pdf("extrafont-1-unembedded.pdf", 7, 4)
txt8 <- annotate("text", 1, 1.1, label = "A sample of Ubuntu Light",
family = "Ubuntu Light", size = 12)
txt9 <- annotate("text", 1, 0.9, label = "A sample of Ubuntu Condensed",
family = "Ubuntu Condensed", size = 12)
print(bg + txt8 + txt9)
dev.off()
embed_fonts("extrafont-1-unembedded.pdf", outfile = "extrafont-1.pdf")
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.05/bin/gswin32c.exe")
# http://CRAN.R-project.org/package=extrafont/README.html
# http://www.freetype.org/
font.add("myxkcd", regular = dest)
library(showtext)
dest <- file.path(tempdir(), "xkcd.ttf")
download.file("http://simonsoftware.se/other/xkcd.ttf", dest, mode = "wb")
if(.Platform$OS.type == "windows") {
font.add("consolas", regular = "consola.ttf", bold = "consolab.ttf",
italic = "consolai.ttf", bolditalic = "consolaz.ttf")
font.families()
}
# [1] "sans" "serif" "mono" "wqy-microhei"
# [5] "myxkcd" "consolas"
head(font.families.google(), 10)
# [1] "ABeeZee" "Abel" "Abril Fatface"
# [4] "Aclonica" "Acme" "Actor"
# [7] "Adamina" "Advent Pro" "Aguafina Script"
# [10] "Akronim"
font.add.google("Lato", "lato")
font.families()
# [1] "sans" "serif" "mono" "wqy-microhei"
# [5] "myxkcd" "consolas" "lato"
# http://xkcd.com/
# http://www.google.com/fonts
# http://stackoverflow.com/questions/9917049/inserting-an-image-to-ggplot2
library(png)
library(grid)
library(ggplot2)
## download and read an image
dest <- file.path(tempdir(), "pic.png")
download.file("http://china-r.org/img/China-R-Logo-trans.png", dest, mode = "wb")
g <- rasterGrob(readPNG(dest), interpolate = TRUE)
## load font and plot
font.add.google("Lato", "lato")
ttl <- "\u6b22\u8fce\u5173\u6ce8\u4e2d\u56fd\u0052\u8bed\u8a00\u4f1a\u8bae"
plt <- ggplot(NULL, aes(x = 1, y = 1)) + xlim(73, 135) + ylim(17, 54) +
annotation_custom(g, xmin = 73, xmax = 135, ymin = 17, ymax = 54) +
annotate("text", -Inf, -Inf, label = "http://china-r.org", size = 8,
family = "lato", fontface = "italic", hjust = -0.1, vjust = -1) +
coord_fixed() + ggtitle(ttl) + theme_grey(base_size = 20) +
theme(axis.title = element_blank(),
plot.title = element_text(family = "wqy-microhei"))

showtext.opts(dpi = 96)
showtext.auto()

ggsave("showtext-1.png", plt, width = 8.75, height = 5, dpi = 96)
## load font
dest <- file.path(tempdir(), "xkcd.ttf")
download.file("http://simonsoftware.se/other/xkcd.ttf", dest, mode = "wb")
font.add("myxkcd", regular = dest)

pdf("showtext-2.pdf", 7, 3)

set.seed(0)
# http://wenq.org/en
p <- runif(1)
showtext.begin()
op <- par(family = "myxkcd", mar = c(0.1, 0.1, 3.1, 1.1))
pie(c(1 - p, p), cex = 1.2, labels = c("Those who understand\nbinary",
                                       "Those who don't"),
col = c("#F8766D", "#00BFC4"), border = NA, radius = 0.9)
box()
par(op)
showtext.end()
title("There are 10 types of people in the world", font.main = 4)

dev.off()
# We first do some setup work...
# library(knitr)
# library(showtext)
# showtext.opts(dpi = 72)
# opts_chunk$set(fig.width = 7, fig.height = 7, dpi = 72)
# Then register a font from Google Fonts.
# font.add.google("Lobster", "lobster")
# Finally we create some fancy plot.
# plot(1, pch = 16, cex = 3)
# text(1, 1.1, "A fancy dot", family = "lobster", col = "steelblue", cex = 3)
# http://www.rstudio.com/products/RStudio/
# yixuanq@gmail.com
# R function
# Is showtext
# enabled?
# Default device
# function
# function
# Text displayed
# on graphics window
# NO
# YES
# Locate font
# Extract
# glyph
# information
# Bitmap
# or vector
# graphics?
# Default device
# function
# Default device
# function
# Bitmap
# Vector
# Details</div>
# text()
# text()
# text()
# raster()
# path()
# A sample of
# DejaVu Serif Italic</div>
# A sample of Liberation Sans
# A sample of Liberation Serif</div>
# A sample of Constantia
# A sample of Lucida Console</div>
# A sample of Ubuntu Light
# A sample of Ubuntu Condensed</div>
# showtext device
