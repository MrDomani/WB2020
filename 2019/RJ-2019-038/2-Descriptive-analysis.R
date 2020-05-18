
## LOADING DATASET ##
data(CIBMTR)

## DEFINING "OBSERVED OUTCOME CATEGORY"
CIBMTR$group[simCIBMTR$event1==1 & simCIBMTR$event2==1] = "both"
CIBMTR$group[simCIBMTR$event1==1 & simCIBMTR$event2==0] = "aGVHD only"
CIBMTR$group[simCIBMTR$event1==0 & simCIBMTR$event2==1] = "death only"
CIBMTR$group[simCIBMTR$event1==0 & simCIBMTR$event2==0] = "cens for both"
# Changing the order of levels
CIBMTR$group <- factor(CIBMTR$group, levels=c("both","aGVHD only","death only","cens for both"))

# Total and percentage subjects by group (outcome category)
print(c(dim(CIBMTR)[1],100,round(table(CIBMTR$group)/dim(CIBMTR)[1]*100,1)))

# Total and percentage subjects by group and gender
print(cbind(apply(table(CIBMTR$sexP, CIBMTR$group),1,sum),round(apply(table(CIBMTR$sexP, CIBMTR$group),1,sum)/dim(CIBMTR)[1]*100,1),round(table(CIBMTR$sexP, CIBMTR$group)/apply(table(CIBMTR$sexP, CIBMTR$group),1,sum)*100,1)))

# Total and percentage subjects by group and age
print(cbind(apply(table(CIBMTR$ageP, CIBMTR$group),1,sum),round(apply(table(CIBMTR$ageP, CIBMTR$group),1,sum)/dim(CIBMTR)[1]*100,1),round(table(CIBMTR$ageP, CIBMTR$group)/apply(table(CIBMTR$ageP, CIBMTR$group),1,sum)*100,1)))

# Total and percentage subjects by group and disease type
print(cbind(apply(table(CIBMTR$dType, CIBMTR$group),1,sum),round(apply(table(CIBMTR$dType, CIBMTR$group),1,sum)/dim(CIBMTR)[1]*100,1),round(table(CIBMTR$dType, CIBMTR$group)/apply(table(CIBMTR$dType, CIBMTR$group),1,sum)*100,1)))

# Total and percentage subjects by group and disease status
print(cbind(apply(table(CIBMTR$dStatus, CIBMTR$group),1,sum),round(apply(table(CIBMTR$dStatus, CIBMTR$group),1,sum)/dim(CIBMTR)[1]*100,1),round(table(CIBMTR$dStatus, CIBMTR$group)/apply(table(CIBMTR$dStatus, CIBMTR$group),1,sum)*100,1)))

# Total and percentage subjects by group and HLA compatibility
print(cbind(apply(table(CIBMTR$donorGrp, CIBMTR$group),1,sum),round(apply(table(CIBMTR$donorGrp, CIBMTR$group),1,sum)/dim(CIBMTR)[1]*100,1),round(table(CIBMTR$donorGrp, CIBMTR$group)/apply(table(CIBMTR$donorGrp, CIBMTR$group),1,sum)*100,1)))

remove(CIBMTR)