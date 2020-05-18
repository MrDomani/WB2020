
import.genotypes(geno, event.type = "variant", color = "Darkgreen")
import.MAF(file, sep = "\t", is.TCGA = TRUE)
import.GISTIC(x)
cbio.query(cbio.study = NA, cbio.dataset = NA, cbio.profile = NA, genes)
show(x, view = 10)
as.genotypes(x)
keysToNames(x, matrix)
as.genes(x, types = NA)
as.events(x, genes = NA, types = NA)
as.alterations(x, new.type = "Alteration", new.color = "khaki")
as.samples(x)
as.types(x, genes = NA)
as.models(x, models = names(x$model)))
as.adj.matrix(x, events = as.events(x), models = names(x$model), type = "fit")
as.patterns(x)
as.confidence(x, conf)
nevents(x, genes = NA, types = NA)
ngenes(x, types = NA)
npatterns(x)
nsamples(x)
ntypes(x)
is.compliant(x,
err.fun = "[ERR]",
stage = !(all(is.null(x$stages)) || all(is.na(x$stages))))
consolidate.data(x, print = FALSE)
TCGA.multiple.samples(x)
TCGA.remove.multiple.samples(x)
delete.gene(x, gene)
delete.samples(x, samples)
delete.type(x, type)
delete.pattern(x, type)
merge.events(x, ..., new.event, new.type, event.color)
merge.types(x, ..., new.type = "new.type", new.color = "khaki")
ebind(...)
sbind(...)
intersect.datasets(x, y, intersect.genomes = TRUE)
rename.gene(x, old.name, new.name)
rename.type(x, old.name, new.name)
change.color(x, type, new.color)
events.selection(x, filter.freq = NA, filter.in.names = NA,filter.out.names = NA)
samples.selection(x, samples)
ssplit(x, clusters, idx = NA)
export.nbs.input(x, map_hugo_entrez, file = "tronco_to_nbs.mat")
export.mutex(x,
filename = "to_mutex",
filepath = "./",
label.mutation = "SNV",
label.amplification = list("High-level Gain"),
label.deletion = list("Homozygous Loss"))
import.mutex.groups(file, fdr = 0.2, display = TRUE)
tronco.caprese(data, lambda = 0.5, do.estimation = FALSE, silent = FALSE)
tronco.capri(data,
command = "hc",
regularization = c("bic", "aic"),
do.boot = TRUE,
nboot = 100,
pvalue = 0.05,
min.boot = 3,
min.stat = TRUE,
boot.seed = NULL,
do.estimation = FALSE,
silent = FALSE)
remove.cycles(adj.matrix,
weights.temporal.priority,
weights.matrix,
not.ordered,
hypotheses = NA,
silent)
hypothesis.add(data,
pattern.label,
lifted.pattern,
pattern.effect = "*",
pattern.cause = "*")
hypothesis.add.homologous(x,
pattern.cause = "*",
pattern.effect = "*",
genes = as.genes(x),
FUN = OR)
hypothesis.add.group(x,
FUN,
group,
pattern.cause = "*",
pattern.effect = "*",
dim.min = 2,
dim.max = length(group),
min.prob = 0)
tronco.bootstrap(reconstruction,
type = "non-parametric",
nboot = 100,
verbose = FALSE)
oncoprint(x)
oncoprint.cbio(x)
annotate.description(x, label)
annotate.stages(x, stages, match.TCGA.patients = FALSE).
genes.table.report(x,
name,
dir = getwd(),
maxrow = 33,
font = 10,
height = 11,
width = 8.5,
fill = "lightblue")
genes.table.plot(x, name, dir = getwd())
> library(TRONCO)
> data(aCML)
> hide.progress.bar <<- TRUE
> show(aCML)
Description: CAPRI - Bionformatics aCML data.
Dataset: n=64, m=31, |G|=23.
Events (types): Ins/Del, Missense point, Nonsense Ins/Del, Nonsense point.
Colors (plot): darkgoldenrod1, forestgreen, cornflowerblue, coral.
Events (10 shown):
gene 4 : Ins/Del TET2
gene 5 : Ins/Del EZH2
gene 6 : Ins/Del CBL
gene 7 : Ins/Del ASXL1
gene 29 : Missense point SETBP1
gene 30 : Missense point NRAS
gene 31 : Missense point KRAS
gene 32 : Missense point TET2
gene 33 : Missense point EZH2
gene 34 : Missense point CBL
Genotypes (10 shown):
gene 4 gene 5 gene 6 gene 7 gene 29 gene 30 gene 31 gene 32 gene 33 gene 34
patient 1 0 0 0 0 1 0 0 0 0 0
patient 2 0 0 0 0 1 0 0 0 0 1
patient 3 0 0 0 0 1 1 0 0 0 0
patient 4 0 0 0 0 1 0 0 0 0 1
patient 5 0 0 0 0 1 0 0 0 0 0
patient 6 0 0 0 0 1 0 0 0 0 0
> as.events(aCML)
type event
gene 4 "Ins/Del" "TET2"
gene 5 "Ins/Del" "EZH2"
gene 6 "Ins/Del" "CBL"
gene 7 "Ins/Del" "ASXL1"
gene 29 "Missense point" "SETBP1"
gene 30 "Missense point" "NRAS"
gene 31 "Missense point" "KRAS"
gene 32 "Missense point" "TET2"
gene 33 "Missense point" "EZH2"
...
gene 88 "Nonsense point" "TET2"
gene 89 "Nonsense point" "EZH2"
gene 91 "Nonsense point" "ASXL1"
gene 111 "Nonsense point" "CSF3R"
> as.genes(aCML)
[1] "TET2" "EZH2" "CBL" "ASXL1" "SETBP1" "NRAS" "KRAS" "IDH2" "SUZ12"
[10] "SF3B1" "JARID2" "EED" "DNMT3A" "CEBPA" "EPHB3" "ETNK1" "GATA2" "IRAK4"
[19] "MTA2" "CSF3R" "KIT" "WT1" "RUNX1"
Missense point SETBP1
patient 1 1
patient 2 1
patient 3 1
...
patient 12 1
patient 13 1
patient 14 1
patient 15 0
patient 16 0
patient 17 0
...
patient 62 0
patient 63 0
patient 64 0
> alterations = events.selection(as.alterations(aCML), filter.freq = .05)
*** Aggregating events of type(s) {Ins/Del, Missense point, Nonsense Ins/Del, Nonsense point}
in a unique event with label "Alteration".
Dropping event types Ins/Del, Missense point, Nonsense Ins/Del, Nonsense point for 23 genes.
*** Binding events for 2 datasets.
*** Events selection: #events=23, #types=1 Filters freq|in|out = \{TRUE, FALSE, FALSE\}
Minimum event frequency: 0.05 (3 alterations out of 64 samples).
Selected 7 events.
Selected 7 events, returning.
> oncoprint(alterations, font.row = 12, cellheight = 20, cellwidth = 4)
*** Oncoprint for ""
with attributes: stage=FALSE, hits=TRUE
Sorting samples ordering to enhance exclusivity patterns.
> hypo = events.selection(aCML,
filter.in.names = c(as.genes(alterations),
gene.hypotheses))
*** Events selection: #events=31, #types=4 Filters freq|in|out = \{FALSE, TRUE, FALSE\}
[filter.in] Genes hold: TET2, EZH2, CBL, ASXL1, SETBP1 ... [10/14 found].
Selected 17 events, returning.
> oncoprint(hypo,
gene.annot = list(priors = gene.hypotheses),
sample.id = T,
font.row = 12,
font.column = 5,
cellheight = 20,
cellwidth = 4)
*** Oncoprint for "CAPRI - Bionformatics aCML data (selected events)"
with attributes: stage=FALSE, hits=TRUE
Sorting samples ordering to enhance exclusivity patterns.
Annotating genes with RColorBrewer color palette Set1 .
Error in hypothesis.add(hypo, "NRAS or KRAS", OR("NRAS", "KRAS")) :
[ERR] Pattern duplicates Pattern NRAS xor KRAS.
> oncoprint(events.selection(hypo,
font.row = 12,
cellheight = 20,
cellwidth = 4)
*** Events selection: #events=18, #types=4 Filters freq|in|out = \{FALSE, TRUE, FALSE\}
[filter.in] Genes hold: KRAS, NRAS ... [2/2 found].
Selected 2 events, returning.
*** Oncoprint for ""
with attributes: stage=FALSE, hits=TRUE
Sorting samples ordering to enhance exclusivity patterns.
Error in hypothesis.add(hypo, "SF3B1 or ASXL1", OR("SF3B1", OR("ASXL1")), :
[ERR] Pattern duplicates Pattern SF3B1 xor ASXL1.
type event
gene 4 "Ins/Del" "TET2"
gene 32 "Missense point" "TET2"
gene 88 "Nonsense point" "TET2"
cellheight = 20, cellwidth = 4)
*** Events selection: #events=21, #types=4 Filters freq|in|out = \{FALSE, TRUE, FALSE\}
[filter.in] Genes hold: TET2, IDH2 ... [2/2 found].
Selected 4 events, returning.
*** Oncoprint for ""
with attributes: stage=FALSE, hits=TRUE
Sorting samples ordering to enhance exclusivity patterns.
> hypo = hypothesis.add.homologous(hypo)
*** Adding hypotheses for Homologous Patterns
Genes: TET2, EZH2, CBL, ASXL1, CSF3R
Function: OR
Cause: *
Effect: *
Hypothesis created for all possible gene patterns.
> oncoprint(hypo,
gene.annot = list(priors = gene.hypotheses),
sample.id = T,
font.row = 10,
font.column = 5,
cellheight = 15,
cellwidth = 4)
*** Oncoprint for "CAPRI - Bionformatics aCML data (selected events)"
with attributes: stage=FALSE, hits=TRUE
Sorting samples ordering to enhance exclusivity patterns.
Annotating genes with RColorBrewer color palette Set1 .
> model = tronco.capri(hypo, boot.seed = 12345, nboot = 10)
*** Checking input events.
*** Inferring a progression model with the following settings.
Dataset size: n = 64, m = 26.
Algorithm: CAPRI with "bic, aic" regularization and "hc" likelihood-fit strategy.
Random seed: 12345.
Bootstrap iterations (Wilcoxon): 10.
exhaustive bootstrap: TRUE.
p-value: 0.05.
minimum bootstrapped scores: 3.
*** Bootstraping selective advantage scores (prima facie).
Evaluating "temporal priority" (Wilcoxon, p-value 0.05)
Evaluating "probability raising" (Wilcoxon, p-value 0.05)
*** Loop detection found loops to break.
Removed 26 edges out of 68 (38%)
*** Performing likelihood-fit with regularization bic.
*** Performing likelihood-fit with regularization aic.
The reconstruction has been successfully completed in 00h:00m:02s
> tronco.plot(model,
fontsize = 13,
scale.nodes = .6,
regularization = "bic",
height.logic = 0.25,
legend.cex = .5,
pathways = list(priors = gene.hypotheses),
label.edge.size = 5)
*** Expanding hypotheses syntax as graph nodes:
*** Rendering graphics
Nodes with no incoming/outgoing edges will not be displayed.
Annotating nodes with pathway information.
Annotating pathways with RColorBrewer color palette Set1 .
Adding confidence information: tp, pr, hg
RGraphviz object prepared.
Plotting graph and adding legends.
> model.boot = tronco.bootstrap(model, nboot = 10)
Executing now the bootstrap procedure, this may take a long time...
Expected completion in approx. 00h:00m:03s
*** Using 7 cores via "parallel"
*** Reducing results
Performed non-parametric bootstrap with 10 resampling and 0.05 as pvalue
for the statistical tests.
> tronco.plot(model.boot,
fontsize = 13,
scale.nodes = 0.6,
regularization = "bic",
height.logic = 0.25,
legend.cex = 0.5,
pathways = list(priors = gene.hypotheses),
label.edge.size = 10)
*** Expanding hypotheses syntax as graph nodes:
*** Rendering graphics
Nodes with no incoming/outgoing edges will not be displayed.
Annotating nodes with pathway information.
Annotating pathways with RColorBrewer color palette Set1 .
Adding confidence information: npb
RGraphviz object prepared.
Plotting graph and adding legends.
> model.boot.caprese = tronco.bootstrap(tronco.caprese(hypo))
*** Checking input events.
*** Inferring a progression model with the following settings.
Dataset size: n = 64, m = 17.
Algorithm: CAPRESE with shrinkage coefficient: 0.5.
The reconstruction has been successfully completed in 00h:00m:00s
Executing now the bootstrap procedure, this may take a long time...
Expected completion in approx. 00h:00m:00s
Performed non-parametric bootstrap with 100 resampling and 0.5
as shrinkage parameter.
> tronco.plot(model.boot.caprese,
fontsize = 13,
scale.nodes = 0.6,
height.logic = 0.25,
legend.cex = 0.5,
pathways = list(priors = gene.hypotheses),
label.edge.size = 10,
legend.pos = "top")
*** Expanding hypotheses syntax as graph nodes:
*** Rendering graphics
Nodes with no incoming/outgoing edges will not be displayed.
Annotating nodes with pathway information.
Annotating pathways with RColorBrewer color palette Set1 .
Adding confidence information: npb
RGraphviz object prepared.
Plotting graph and adding legends.
