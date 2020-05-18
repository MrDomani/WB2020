library(StarBioTrek)

# GET DATA

# pathway data
lipid<-getKEGGdata(KEGG_path="Lip_met")
colnames(lipid)

# network data
getNETdata(network="SHpd")


# Analyzing pathways
list_path_gene<-GE_matrix(DataMatrix=tumo[,1:2],pathway=path)
str(list_path_gene)


# Pathway summary indexes
score_mean<-average(dataFilt=tumo,pathway=path)
score_st_dev<-st_dv(DataMatrix=tumo,pathway=path)


# Dissimilarity distances: Pathway cross-talk indexes
score_euc_dista<-euc_dist_crtlk(dataFilt=tumo,pathway=path)
cross_talk<-ds_score_crtlk(dataFilt=tumo,pathway=path)


# Integration data: Integration between pathways and network
m<-path_net(pathway=path,data=netw)
n<-list_path_net(lista_net=m,pathway=path)


# Analyzing networks and pathways: Implemented algorithms

# get pathways of lipid metabolism from KEGG database
path_lip<-getKEGGdata(KEGG_path="Lip_met")



# create a measure of pathway cross-talk (i.e. euclidean distance) between pairwise
# of pathways starting from gene expression data (i.e.TCGA) with in the columns the
#samples and in the rows the genes
score_euc_dist_Lip_met<-euc_dist_crtlk(dataFilt=Data_CANCER_normUQ_filt,path_lip)

# split samples' TCGA ID into normal and tumor groups
tumo<-SelectedSample(Dataset=Data_CANCER_normUQ_filt,typesample="tumor")
norm<-SelectedSample(Dataset=Data_CANCER_normUQ_filt,typesample="normal")


# divide the dataset in 60/100 for training and 40/100 for testing
nf <- 60
# a support vector machine is applied
res_class<-svm_classification(TCGA_matrix=score_euc_dist_Lip_met,nfs=nf,
                              normal=colnames(norm), tumour=colnames(tumo))


better_perf<-select_class(auc.df=res_class,cutoff=0.80)

# Driver genes for each pathway
i<-IPPI(patha=path,netwa=netw)

# Case studies
# Pathway cross-talk network in breast cancer

patha<-getKEGGdata(KEGG_path="KEGG_path")
cross_talk_score<-ds_score_crtlk(dataFilt=Data_CANCER_normUQ_filt,pathway=patha)

cross_talk_score<-cross_talk_score[complete.cases(cross_talk_score), ]

nf <- 60
res_class<-svm_classification(cross_talk_score[1:100,],nfs=nf,normal=colnames(norm),tumour=colnames(tumo))



# second case studies 

patha<-getKEGGdata(KEGG_path="KEGG_path")

# for Physical interactions
PHint<-getNETdata(network="PHint")
# for Co-localization
COloc<-getNETdata(network="COloc")
# for Genetic interactions
gGENint<-getNETdata(network="GENint")
# for Pathway interactions
PATH<-getNETdata(network="PATH")
# for Shared_protein_domains
SHpd<-getNETdata(network="SHpd")


e<-IPPI(patha=patha,netwa=PHint)

