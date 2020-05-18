install.packages('tigris')
library(tigris)
us_states <- states()
plot(us_states)
us_states_20m <- states(cb = TRUE, resolution = "20m")
ri <- us_states[us_states$NAME == "Rhode Island", ]
ri_20m <- us_states_20m[us_states_20m$NAME == "Rhode Island", ]
plot(ri)
plot(ri_20m, border = "red", add = TRUE)
kw_roads <- roads("HI", "Kalawao")
plot(kw_roads)
rails
tribal_subdivisions_national
fw_zips <- zctas(cb = TRUE, starts_with = "761")
plot(fw_zips)
> tigris_type(kw_roads)
[1] "road"
maui_roads <- roads("HI", "Maui")
kw_roads$county <- "Kalawao"
maui_roads$county <- "Maui"
maui_kw_roads <- rbind_tigris(kw_roads, maui_roads)
plot(maui_kw_roads, col = c("red", "black")[as.factor(maui_kw_roads$county)])
us_states <- unique(fips_codes$state)[1:51]
continental_states <- us_states[!us_states %in% c("AK", "HI")]
us_pumas <- rbind_tigris(
lapply(
continental_states, function(x) {
pumas(state = x, cb = TRUE)
}
)
)
plot(us_pumas)
df <- read.csv("http://personal.tcu.edu/kylewalker/data/txlege.csv",
stringsAsFactors = FALSE)
districts <- state_legislative_districts("TX", house = "lower", cb = TRUE)
txlege <- geo_join(districts, df, "NAME", "District")
txlege$color <- ifelse(txlege$Party == "R", "red", "blue")
plot(txlege, col = txlege$color)
legend("topright", legend = c("Republican", "Democrat"),
fill = c("red", "blue"))
org/directory/
library(dplyr)
library(stringr)
library(readr)
# Read in the IRS data
zip_data <- "https://www.irs.gov/pub/irs-soi/13zpallnoagi.csv"
df <- read_csv(zip_data) %>%
mutate(zip_str = str_pad(as.character(ZIPCODE), width = 5,
side = "left", pad = "0"),
incpr = A02650 / N02650) %>%
select(zip_str, incpr)
library(tigris)
library(sp)
# Write function to get ZCTAs for a given metro
get_zips <- function(metro_name) {
zips <- zctas(cb = TRUE)
metros <- core_based_statistical_areas(cb = TRUE)
# Subset for specific metro area
# (be careful with duplicate cities like "Washington")
my_metro <- metros[grepl(sprintf("^%s", metro_name),
metros$NAME, ignore.case = TRUE), ]
# Find all ZCTAs that intersect the metro boundary
metro_zips <- over(my_metro, zips, returnList = TRUE)[[1]]
my_zips <- zips[zips$ZCTA5CE10 %in% metro_zips$ZCTA5CE10, ]
# Return those ZCTAs
return(my_zips)
}
library(tmap)
rds <- primary_roads()
dfw <- get_zips("Dallas")
dfw_merged <- geo_join(dfw, df, "ZCTA5CE10", "zip_str")
tm_shape(dfw_merged, projection = "+init=epsg:26914") +
tm_fill("incpr", style = "quantile", n = 7, palette = "Greens", title = "") +
tm_shape(rds, projection = "+init=epsg:26914") +
tm_lines(col = "darkgrey") +
tm_layout(bg.color = "ivory",
title = "Average income by zip code \n(in $1000s US), Dallas-Fort Worth",
title.position = c("right", "top"), title.size = 1.1,
legend.position = c(0.85, 0), legend.text.size = 0.75,
legend.width = 0.2) +
tm_credits("Data source: US Internal Revenue Service",
position = c(0.002, 0.002))
