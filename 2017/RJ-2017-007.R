clusters(load_clusters(system.file("extdata/clusters_example.txt.gz", package = "MDplot"),
names=c("wild-type","mut1","mut2",
"mut3","mut4","mut5")),
clustersNumber=9,main="MDplot::clusters()",ylab="# configurations")
clusters_ts(load_clusters_ts(system.file("extdata/clusters_example.txt.gz", package = "MDplot"),
lengths=c(4000,4000,4000,4000,4000,4000),
names=c("wild-type","mut1","mut2",
"mut3","mut4","mut5")),
clustersNumber=7,main="MDplot::clusters_ts() example",
timeUnit="ns",snapshotsPerTimeInt=100)
layout(matrix(1:3, nrow=1), widths=c(0.33,0.33,0.33))
dssp(load_dssp(system.file("extdata/dssp_example.txt.gz", package = "MDplot")),
main="plotType=dots",showResidues=c(1,35))
dssp(load_dssp(system.file("extdata/dssp_example.txt.gz", package = "MDplot")),
main="plotType=curves",plotType="curves",showResidues=c(1,35))
dssp(load_dssp(system.file("extdata/dssp_example.txt.gz", package = "MDplot")),
main="plotType=bars",plotType="bars",showResidues=c(1,35))
dssp_ts(load_dssp_ts(system.file("extdata/dssp_example.txt.gz", package = "MDplot")),printLegend=TRUE,
main="MDplot::dssp_ts()",timeUnit="ns",
snapshotsPerTime=1000)
hbond(load_hbond("inst/extdata/hbond_example.txt.gz"),
main="MDplot::hbond()",donorRange=c(0,65))
hbond_ts(timeseries=load_hbond_ts("inst/extdata/hbond_ts_example.txt.gz"),
summary=load_hbond("inst/extdata/hbond_example.txt.gz"),
main="MDplot::hbond_ts()",acceptorRange=c(22,75),
hbondIndices=list(c(0,24)),plotOccurences=TRUE,timeUnit="ns",
snapshotsPerTimeInt=100,printNames=TRUE,namesToSingle=TRUE,
printAtoms=TRUE)
noe(load_noe(files=c("inst/extdata/noe_example_1.txt.gz",
"inst/extdata/noe_example_2.txt.gz")),
main="MDplot::noe()")
ramachandran(load_ramachandran("inst/extdata/ramachandran_example.txt.gz"),
heatFun="log",plotType="sparse",xBins=90,yBins=90,
main="ramachandran() (plotType=sparse)",
plotContour=TRUE)
ramachandran(load_ramachandran("inst/extdata/ramachandran_example.txt.gz"),
heatFun="norm",plotType="fancy",xBins=90,yBins=90,
main="ramachandran() (plotType=fancy)",
printLegend=TRUE)
rmsd(load_rmsd(c("inst/extdata/rmsd_example_1.txt.gz",
"inst/extdata/rmsd_example_2.txt.gz")),
printLegend=TRUE,names=c("WT","mut"),main="MDplot::rmsd()")
rmsd_average(rmsdInput=list(load_rmsd("inst/extdata/rmsd_example_1.txt.gz" ),
load_rmsd("inst/extdata/rmsd_example_2.txt.gz")),
maxYAxis=0.375,main="MDplot::rmsd_average()")
rmsf(load_rmsf(c("inst/extdata/rmsf_example_1.txt.gz",
"inst/extdata/rmsf_example_2.txt.gz")),
printLegend=TRUE,names=c("WT","mut"),range=c(1,75),
main="MDplot::rmsf()")
TIcurve(load_TIcurve(c("inst/extdata/TIcurve_fb_forward_example.txt.gz",
"inst/extdata/TIcurve_fb_backward_example.txt.gz")),
invertedBackwards=TRUE, main="MDplot::TIcurve()")
timeseries(load_timeseries(c("inst/extdata/timeseries_example_1.txt.gz",
"inst/extdata/timeseries_example_2.txt.gz")),
main="MDplot::timeseries()",
names=c("fluc1","fluc2"),
snapshotsPerTimeInt=100)
xrmsd(load_xrmsd("inst/extdata/xrmsd_example.txt.gz"),
printLegend=TRUE,main="MDplot::xrmsd()")
#!/bin/bash
# clusters
Rscript MDplot_bash.R clusters files=../extdata/clusters_example.txt.gz \
title="Cluster analysis" size=900,900 \
outformat=tiff outfile=clusters.tiff \
clustersNumber=7 \
names=WT,varA,varB,varC2,varD3,varE4
# xrmsd
Rscript MDplot_bash.R xrmsd files=../extdata/xrmsd_example.txt.gz title="XRMSD" \
size=1100,900 outformat=pdf outfile=XRMSD.pdf \
xaxisRange=75,145
# ramachandran
Rscript MDplot_bash.R ramachandran files=../extdata/ramachandran_example.txt.gz \
title="Ramachandran plot" size=1400,1400 resolution=175 \
outformat=tiff outfile=ramachandran.tiff angleColumns=1,2 \
bins=75,75 heatFun=norm printLegend=TRUE plotType=fancy
MDplot::clusters_ts() example</div>
populations</div>
61.1 %
11.7 %
7.4 %
5.7 %
1.8 %
1.8 %
1.5 %
1 2 3 4 5 6 7
10 20 30 40
wild−type
mut1
mut2
mut3
mut4
mut5
time [ns]</div>
1 2 3 4 5 6 7 8 9
clusters
# configurations
0 10000 20000 30000 40000</div>
trajectories
wild−type
mut1
mut2
mut3
mut4
mut5</div>
MDplot::clusters()
