# http://ec.europa.eu/eurostat/data/database
# http://ropengov.github.io/eurostat
# http://ec.europa.eu/eurostat/data/database
install.packages("eurostat")
library("eurostat")
query <- search_eurostat("road accidents", type = "table")
query$code[[1]]
# [1] "tsdtr420"
query$title[[1]]
# [1] "People killed in road accidents"
dat <- get_eurostat(id = "tsdtr420", time_format = "num")
countries <- c("UK", "SK", "FR", "PL", "ES", "PT")
t1 <- get_eurostat("tsdtr420", filters = list(geo = countries))
ggplot(t1, aes(x = time, y = values, color = geo, group = geo, shape = geo)) +
geom_point(size = 4) + geom_line() + theme_bw() +
ggtitle("Road accidents") + xlab("Year") + ylab("Victims (n)") +
theme(legend.position = "none") +
ggrepel::geom_label_repel(data = t1 %>% group_by(geo) %>% na.omit() %>%
filter(time %in% c(min(time), max(time))), aes(fill = geo, label = geo), color = "white")
# http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-
# statistical-units
# http://ec.europa.eu/eurostat/en/web/products-datasets/-/TGS00026
# http://ec.europa.eu/eurostat/web/nuts/overview
# Load the required libraries
library(eurostat)
library(dplyr)
library(ggplot2)
# Download and manipulate tabular data
get_eurostat("tgs00026", time_format = "raw") %>%
# Subset to year 2005 and NUTS-3 level
dplyr::filter(time == 2005, nchar(as.character(geo)) == 4) %>%
# Classify the values the variable
dplyr::mutate(cat = cut_to_classes(values)) %>%
# Merge Eurostat data with geodata from Cisco
merge_eurostat_geodata(data = ., geocolumn = "geo", resolution = "60",
output_class = "df", all_regions = TRUE) %>%
# Plot the map
ggplot(data = ., aes(long, lat, group = group)) +
geom_polygon(aes(fill = cat), colour = alpha("white", 1/2), size = .2) +
scale_fill_manual(values = RColorBrewer::brewer.pal(n = 5, name = "Oranges")) +
labs(title = "Disposable household income") +
coord_map(project = "orthographic", xlim = c(-22, 34), ylim = c(35, 70)) +
theme_minimal() +
guides(fill = guide_legend(title = "EUR per Year",
title.position = "top", title.hjust = 0))
data(efta_countries)
