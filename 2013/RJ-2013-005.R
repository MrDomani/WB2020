# OSM-XML
# osmar
# get_osm_data()
# get_osm_data()
# as_igraph()
# summary()
# find()
# subset()
# get_osm()
library("osmar")
# Loading required package: XML
# Loading required package: RCurl
# Loading required package: bitops
# Loading required package: gtools
# Loading required package: geosphere
# Loading required package: sp
# Attaching package: 'osmar'
# The following object(s) are masked
# from 'package:utils':
# find
src <- osmsource_api()
get_osm(node(18961430), source = src)
# osmar object
# 1 nodes, 0 ways, 0 relations
get_osm(way(3810479), source = src)
# osmar object
# 0 nodes, 1 ways, 0 relations
get_osm(way(3810479), source = src, full = TRUE)
# osmar object
# 11 nodes, 1 ways, 0 relations
bb <- center_bbox(174.76778, -36.85056, 700, 700)
ua <- get_osm(bb, source = src)
ua
# osmar object
# 2427 nodes, 428 ways, 70 relations
summary(ua$nodes)
# osmar$nodes object
# 2427 nodes, 771 tags
# ..$attrs data.frame:
# id, lat, lon, user, uid, visible, version,
# changeset, timestamp
# ..$tags data.frame:
# id, k, v
# Bounding box:
# lat lon
# min -36.85661 174.7627
# max -36.84472 174.7753
# Key-Value contingency table:
# Key Value Freq
# 1 addr:city Auckland 101
# 2 addr:street Queen Street 61
# 3 addr:country NZ 40
# 4 addr:postcode 1010 39
# 5 comment Determined via Keypa... 29
# 6 addr:street Symonds Street 27
# 7 highway traffic_signals 23
# 8 addr:street Lorne Street 19
# 9 highway bus_stop 15
# 10 amenity cafe 11
# osmar object
# 1321 nodes, 253 ways, 0 relations
ts_ids <- find(ua, node(tags(v == "traffic_signals")))
ts_ids
# [1] 25769635 25769637 25769641 ...
bs_ids <- find(ua, node(tags(v %agrep% "busstop")))
bs_ids
# [1] 678301119 737159091 1318401034 ...
hw_ids <- find(ua, way(tags(k == "highway")))
hw_ids <- find_down(ua, way(hw_ids))
str(hw_ids)
# List of 3
# $ node_ids : num [1:1321] 25769641 ...
# $ way_ids : num [1:253] 4309608 ...
# $ relation_ids: NULL
ts <- subset(ua, node_ids = ts_ids)
ts
# osmar object
# 25 nodes, 0 ways, 0 relations
bs <- subset(ua, node_ids = bs_ids)
bs
# osmar object
# 15 nodes, 0 ways, 0 relations
hw <- subset(ua, ids = hw_ids)
hw
plot(ua)
plot_ways(hw, add = TRUE, col = "green")
plot_nodes(ts, add = TRUE, col = "red")
plot_nodes(bs, add = TRUE, col = "blue")
args(as_sp)
# function(obj, what = c("points", "lines", "polygons"),
# crs = osm_crs(), simplify = TRUE)
# NULL
bg_ids <- find(ua, way(tags(k == "building")))
bg_ids <- find_down(ua, way(bg_ids))
bg <- subset(ua, ids = bg_ids)
# osmar object
bg_poly <- as_sp(bg, "polygons")
bg
# 991 nodes, 110 ways, 0 relations
spplot(bg_poly, c("version"))
hw_line <- as_sp(hw, "lines")
bs_points <- as_sp(bs, "points")
bus_ids <- find(ua, relation(tags(v == "bus")))
bus <- lapply(bus_ids,
function(i) {
raw <- get_osm(relation(i), full = TRUE)
as_sp(raw, "lines")
})
plot(bg_poly, col = "gray")
plot(hw_line, add = TRUE, col = "green")
hways <- find_down(muc, way(hways))
hways_muc <- subset(muc, ids = hways)
hways_muc
plot(bs_points, add = TRUE, col = "blue")
for ( i in seq(along = bus) ) {
plot(bus[[i]], add = TRUE, col = "blue")
}
library("osmar")
url <- "http://osmar.r-forge.r-project.org/"
file <- "muenchen.osm.gz"
download.file(sprintf("%s%s", url, file), file)
system("gzip -d muenchen.osm.gz")
src <- osmsource_osmosis(file = "muenchen.osm")
muc_bbox <- center_bbox(11.575278, 48.137222, 3000, 3000)
muc <- get_osm(muc_bbox, src)
muc
# osmar object
# 13713 nodes, 3156 ways, 76 relations
hways_muc <- subset(muc, way_ids = find(muc, way(tags(k == "highway"))))
hways <- find(hways_muc, way(tags(k == "name")))
# osmar object
# 3889 nodes, 995 ways, 0 relations
hway_start <- subset(muc, node(hway_start_node))
hway_start_node <- local({
id <- find(muc, node(tags(v == "Sendlinger Tor")))[1]
find_nearest_node(muc, id, way(tags(k == "highway")))
})
hway_end_node <- local({
id <- find(muc, node(attrs(lon > 11.59 & lat > 48.150)))[1]
find_nearest_node(muc, id, way(tags(k == "highway")))
})
hway_end <- subset(muc, node(hway_end_node))
plot_nodes(muc, col = "gray")
plot_ways(hways_muc, add = TRUE)
plot_nodes(hways_muc, add = TRUE, col = "black")
plot_nodes(hway_start, add = TRUE, col = "red")
plot_nodes(hway_end, add = TRUE, col = "blue")
library("igraph0")
gr_muc <- as_igraph(hways_muc)
summary(gr_muc)
w <- match(node_ids, route_muc$ways$refs$ref)
route_muc$ways$refs$id[w]
})
# Vertices: 2381
# Edges: 2888
# Directed: TRUE
# No graph attributes.
node_ids <- route_muc$nodes$attrs$id
way_ids <- local({
# Vertex attributes: name.
# Edge attributes: weight, name.
route <- get.shortest.paths(gr_muc,
from = as.character(hway_start_node),
to = as.character(hway_end_node))[[1]]
route_nodes <- as.numeric(V(gr_muc)[route]$name)
route_ids <- find_up(hways_muc, node(route_nodes))
route_muc <- subset(hways_muc, ids = route_ids)
route_muc
# osmar object
# 101 nodes, 83 ways, 0 relations
plot_nodes(route_muc, add = TRUE, col = "green")
plot_ways(route_muc, add = TRUE, col = "green")
way_names <- local({
n <- subset(route_muc$ways$tags, k == "name")
n[match(way_ids, n$id), "v"]
})
node_coords <- route_muc$nodes$attrs[, c("lon", "lat")]
node_dirs <- local({
n <- nrow(node_coords)
from <- 1:(n-1)
to <- 2:n

cbind(dist = c(0,
distHaversine(node_coords[from, ], node_coords[to, ])),
bear = c(0,
bearing(node_coords[from, ],
node_coords[to, ])))
})
route_details <- data.frame(way_names, node_dirs)
route_details$cdist <- cumsum(route_details$dist)
route_details$dir <- compass(route_details$bear)
head(route_details)
# way_names dist bear cdist dir
# 1 Sendlinger-Tor-Platz 0 0 0 N
# 2 Wallstraße 65 62 65 ENE
# 3 Herzog-Wilhelm-Straße 29 75 94 ENE
# 4 Oberanger 10 78 104 ENE
# 5 Oberanger 69 94 173 E
# 6 Nikolaus-Gradl-Weg 25 76 198 ENE
# manuel.eugster@stat.uni-muenchen.de
# tho.schlesinger@googlemail.com
