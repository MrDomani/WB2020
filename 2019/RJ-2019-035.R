 #####################################################################
 #:: This script is used to test our BINCOR R package 
 #####################################################################
 #:: Material for BINCOR package and paper: 
 #:: BINCOR: an R package to estimate the correlation between two 
 #:: unevenly spaced climate time series. By J. M. Polanco-Martínez, 
 #:: M. A. Medina-Elizalde, M.F. Sánchez Goñi and M. Mudelsee
 #:: Submitted to review in The R Journal 
 #:: mar feb  6 09:10:56 CET 2018
 #####################################################################
 #:: By Josué M. Polanco-Martínez (josue.m.polanco@gmail.com) 
 #####################################################################

 # Load the packages
 library(BINCOR)
 library(pracma) 
 
 #####################################################################
 #:: Example 1: (ENSO vs. NHSST)
 #####################################################################

 # Load the time series under analysis: Example 1  
 data(ENSO)
 data(NHSST)

 #####################################################################
 # Figure 1 
 # Compute the binned time series though our bin_cor function
 bincor.tmp <- bin_cor(ENSO.dat, NHSST.dat, FLAGTAU=3, "output_ENSO_NHSST.tmp")
 binnedts <- bincor.tmp$Binned_time_series

 # Applying our plot_ts function 
 # "Screen"
 plot_ts(ENSO.dat, NHSST.dat, binnedts[,1:2], binnedts[,c(1,3)], "ENSO-Nino3",
"SST NH Mean", colts1=1, colts2=2, colbints1=3, colbints2=4, device="screen")

 # PDF format
 plot_ts(ENSO.dat, NHSST.dat, binnedts[,1:2], binnedts[,c(1,3)], "ENSO-Nino3",
 "SST NH Mean", colts1=1, colts2=2, colbints1=3, colbints2=4, device="pdf",
 Hpdf=6, Wpdf=9, resfig=300, ofilename="plot_ts_RAW_BIN_enso_sst")

 #####################################################################
 # Figure 2 
 # Compute the scatterplot by means of our function cor_ts 
 # screen (scatterplot) and Pearson
 cor_ts(binnedts[,1:2], binnedts[,c(1,3)], "ENSO-Nino3", "SST NH Mean",
 KoCM="pearson", rmltrd="y")

 # PDF format (scatterplot) and Pearson
 cor_ts(binnedts[,1:2], binnedts[,c(1,3)], "ENSO-Nino3", "SST NH Mean",
 KoCM="pearson", rmltrd="y", device="pdf", Hpdf=6, Wpdf=9, resfig=300,
 ofilename="scatterplot_ENSO_SST")

 #####################################################################
 #:: Example 2 (MD04-2845 vs. MD95-2039)
 #####################################################################
 # Load the time series under analysis
 data(MD04_2845_siteID31)
 data(MD95_2039_siteID32)

 #####################################################################
 # Figure 4
 # Compute the binned time series though our bin_cor function
 bincor.tmp <- bin_cor(ID31.dat, ID32.dat, FLAGTAU=3, "salida_ACER_ABRUPT.tmp")
 binnedts   <- bincor.tmp$Binned_time_series

 # To avoid NA's values
 bin_ts1 <- na.omit(bincor.tmp$Binned_time_series[,1:2])
 bin_ts2 <- na.omit(bincor.tmp$Binned_time_series[,c(1,3)]) 
 
 # Applying our plot_ts function 
 # "Screen"
 plot_ts(ID31.dat, ID32.dat, bin_ts1, bin_ts2, "MD04-2845 (Temp. forest)", 
 "MD95-2039 (Temp. forest )", colts1=1, colts2=2, colbints1=3, colbints2=4,
 device="screen")

 # PDF format
 plot_ts(ID31.dat, ID32.dat, bin_ts1, bin_ts2, "MD04-2845 (Temp. forest)",
 "MD95-2039 (Temp. forest )", colts1=1, colts2=2, colbints1=3, colbints2=4,
 device="pdf", Hpdf=6, Wpdf=9, resfig=300, ofilename="ts_ACER_ABRUPT")
 
 #####################################################################
 # Figure 5 
 # Applying our ccf_ts function 
 # "Screen"
 ccf_ts(bin_ts1, bin_ts2, RedL=TRUE, rmltrd="y") 

 # PDF format
 ccf_ts(bin_ts1, bin_ts2, RedL=TRUE, rmltrd="y", device="pdf", Hpdf=6,
 Wpdf=9, resfig=300, ofilename="ccf_ID31_ID32_res") 
  

