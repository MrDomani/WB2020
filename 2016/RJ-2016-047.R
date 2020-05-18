library(ACSNMineR)
length(ACSN_maps)
# [1] 6
names(ACSN_maps)
# [1] "Apoptosis" "CellCycle" "DNA_repair" "EMT_motility" "Master"
# [6] "Survival"
genes_test
# [1] "ATM" "ATR" "CHEK2" "CREBBP" "TFDP1" "E2F1" "EP300"
# [8] "HDAC1" "KAT2B" "GTF2H1" "GTF2H2" "GTF2H2B"
library(ACSNMineR)
results <- enrichment(genes_test)
dim(results)
# [1] 8 9
results[3,]
# module module_size nb_genes_in_module
# V161 CellCycle:E2F1 19 12
# genes_in_module
# V161 ATM ATR CHEK2 CREBBP TFDP1 E2F1 EP300 HDAC1 KAT2B GTF2H1 GTF2H2 GTF2H2B
# universe_size nb_genes_in_universe p.value p.value.corrected test
# V161 2237 12 3.735018e-21 2.353061e-19 greater
enrichment(genes_test,correction_multitest = FALSE)
# # heatmap
represent_enrichment(enrichment = list(Corrected = results[1:6,],
 Uncorrected = results_uncorrected[1:6,]),
 plot = "heatmap", scale = "reverselog",
 low = "steelblue" , high ="white", na.value = "grey")
# # barplot
# represent_enrichment(enrichment = list(Corrected = results[1:6,],
# Uncorrected = results_uncorrected[1:6,]),
# plot = "bar", scale = "reverselog",
# nrow = 1)
# CellCycle
# CellCycle:APOPTOSIS_ENTRY
# CellCycle:CYCLINB
# CellCycle:E2F1
# CellCycle:E2F1_TARGETS
# CellCycle:E2F4
# DNA_repair
# DNA_repair:G1_S_CHECKPOINT
# DNA_repair:G2_M_CHECKPOINT
# Corrected
# Uncorrected
# Modules
# 1e−16
# 1e−11
# 1e−06
# p−values</div>
# 1e−15
# 1e−10
# 1e−05
# 1e+00
# CellCycle
# CellCycle:APOPTOSIS_ENTRY
# CellCycle:E2F1
# DNA_repair
# DNA_repair:G1_S_CHECKPOINT
# DNA_repair:G2_M_CHECKPOINT
# module
# p.values
# 1e−17
# 1e−12
# 1e−07
# 1e−02
# CellCycle
# CellCycle:APOPTOSIS_ENTRY
# CellCycle:CYCLINB
# CellCycle:E2F1
# CellCycle:E2F4
# CellCycle:E2F1_TARGETS
# module
# p.values</div>
# a short RNaviCell script example
# load RNaviCell library
library(RNaviCell)
# create a NaviCell object and launch a server session
# this will automatically open a browser on the client
navicell <- NaviCell()
navicell$launchBrowser()
# import a gene expression matrix and
# send the data to the NaviCell server
# NB: the data_matrix object is a regular R matrix
data_matrix <- navicell$readDatatable('DU145_data.txt')
navicell$importDatatable("mRNA expression data", "DU145", data_matrix)
# set data set and sample for heat map representation
navicell$heatmapEditorSelectSample('0','data')
navicell$heatmapEditorSelectDatatable('0','DU145')
navicell$heatmapEditorApply()
# load all necessary packages
library(breastCancerMAINZ)
library(Biobase)
library(limma)
library(ACSNMineR)
library(hgu133a.db)
library(RNaviCell)
# load data and extract expression and phenotype data
data(mainz)
eset <- exprs(mainz)
pdat <- pData(mainz)
# Create list of genes differentially expressed between ER positive and
# ER negative samples using moderated t-test statistics
design <- model.matrix(~factor(pdat$er == '1'))
lmFit(eset, design) -> fit
eBayes(fit) -> ebayes
toptable(ebayes, coef=2,n=25000) -> tt
which(tt$adj < 0.05) -> selection
rownames(tt[selection,]) -> probe_list
mget(probe_list, env = hgu133aSYMBOL) -> symbol_list
symbol_list <- as.character(symbol_list)
# calculate enrichment in ACSN maps
enrichment(symbol_list) -> results
dim(results)
[1] 8 9
# Import MSigDB canonical pathways and calculate enrichment on this database
mtsig <- format_from_gmt('c2.cp.v5.0.symbols.gmt')
enrichment(symbol_list, maps = mtsig)
# Select ER negative samples and calculate mean expression values
apply(eset[probe_list,pdat$er == 0],1,mean) -> er_minus_mean
names(er_minus_mean) <- symbol_list
er_minus_mean <- as.matrix(er_minus_mean)
colnames(er_minus_mean) <- c('exp')
# create a NaviCell session, import the expression matrix on the map and create
# heatmaps to represent the data points.
navicell <- NaviCell()
navicell$proxy_url <- "https://acsn.curie.fr/cgi-bin/nv_proxy.php"
navicell$map_url <- "https://acsn.curie.fr/navicell/maps/acsn/master/index.php"
navicell$launchBrowser()
navicell$importDatatable("mRNA expression data", "GBM_exp", er_minus_mean)
navicell$heatmapEditorSelectSample('0','exp')
navicell$heatmapEditorSelectDatatable('0','GBM_exp')
navicell$heatmapEditorApply()
library(RNaviCell)
# Create a NaviCell object, point it to the ACSN master map and launch
# a session.
navicell <- NaviCell()
navicell$proxy_url <- "https://acsn.curie.fr/cgi-bin/nv_proxy.php"
navicell$map_url <- "https://acsn.curie.fr/navicell/maps/acsn/master/index.php"
navicell$launchBrowser()
# Read the GBM data file and import it into the session.
mat <- navicell$readDatatable('gbm.txt')
navicell$importDatatable("Mutation data", "GBM", mat)
# set datatable and sample names for the glyph editor
navicell$drawingConfigSelectGlyph(1, TRUE)
navicell$glyphEditorSelectSample(1, "categ")
navicell$glyphEditorSelectShapeDatatable(1, "GBM")
navicell$glyphEditorSelectColorDatatable(1, "GBM")
navicell$glyphEditorSelectSizeDatatable(1, "GBM")
navicell$glyphEditorApply(1)
# set color, shape and size parameters for glyphs
navicell$unorderedConfigSetDiscreteShape("GBM", "sample", 0, 1)
navicell$unorderedConfigSetDiscreteShape("GBM", "sample", 1, 5)
navicell$unorderedConfigApply("GBM", "shape")
navicell$unorderedConfigSetDiscreteColor("GBM", "sample", 0, "398BC3")
navicell$unorderedConfigSetDiscreteColor("GBM", "sample", 1, "CC5746")
navicell$unorderedConfigApply("GBM", "color")
navicell$unorderedConfigSetDiscreteSize("GBM", "sample", 0, 4)
navicell$unorderedConfigSetDiscreteSize("GBM", "sample", 1, 14)
navicell$unorderedConfigApply("GBM", "size")
