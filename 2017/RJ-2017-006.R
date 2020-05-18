library(deSolve)
START <- 0; FINISH <- 50; STEP <- 0.01;
simtime <- seq(START, FINISH, by=STEP)
stocks <- c(sPotentialAdopters=99999,sAdopters=1)
auxs <- c(aTotalPopulation=100000, aContact.Rate=3, aInfectivity=0.15)
model <- function(time, stocks, auxs){
with(as.list(c(stocks, auxs)),{
aBeta <- aContact.Rate * aInfectivity/ aTotalPopulation # Eqn (3)
aRho <- aBeta * sAdopters # Eqn (7)
fAR <- sPotentialAdopters * aRho # Eqn (8)
dPA_dt <- -fAR # Eqn (1)
dA_dt <- fAR # Eqn (2)
return (list(c(dPA_dt, dA_dt),
AR=fAR, Rho=aRho))
})
}
o<-data.frame(ode(y=stocks, times=simtime, func = model,
parms=auxs, method="euler"))
library(deSolve)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
TotalPopulation<-100000
name.reg<-c("R1","R2","R3","R4","R5",
"R6","R7","R8","R9","R10")
row.reg<-c(1,1,1,1,1,2,2,2,2,2)
col.reg<-c(1,2,3,4,5,1,2,3,4,5)
pop.reg<-c(0.10,0.01,0.21,0.08,0.17,
0.04,0.05,0.25,0.03,0.06)
sp<-data.frame(Regions=name.reg,
Row=row.reg,
Col=col.reg,
Pop=pop.reg*TotalPopulation)
normal.contacts<-runif(10,pop.reg*40,pop.reg*60)
infectivity<-runif(10,0.01,0.025)
names(infectivity)<-c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10")
ALPHA<-1.00
dm <- as.matrix(dist(sp[c("Col","Row")]))
cr <- t(normal.contacts*(dm+1)^-ALPHA)
ec <- t(t(cr)*infectivity)
beta <- ec/sp$Pop
START<-0; FINISH<-50; STEP<-0.01;
NUM_REGIONS<-10; NUM_STOCKS_PER_REGION <-2
simtime <- seq(START, FINISH, by=STEP)
stocks <- c(PA_R1=sp$Pop[1], PA_R2=sp$Pop[2], PA_R3=sp$Pop[3],
PA_R4=sp$Pop[4], PA_R5=sp$Pop[5], PA_R6=sp$Pop[6],
PA_R7=sp$Pop[7], PA_R8=sp$Pop[8]-1, PA_R9=sp$Pop[9],
PA_R10=sp$Pop[10], AD_R1=0, AD_R2=0,
AD_R3=0, AD_R4=0, AD_R5=0,
AD_R6=0, AD_R7=0, AD_R8=1,
AD_R9=0, AD_R10=0
model <- function(time, stocks, auxs){
with(as.list(stocks),{
states<-matrix(stocks,
nrow=NUM_REGIONS,
ncol=NUM_STOCKS_PER_REGION)
PotentialAdopters <- states[,1]
Adopters <- states[,2]
Rho <- beta %*% Adopters # Eqn (11)
AR <- Rho * PotentialAdopters # Eqn (9)
dPA_dt <- -AR # Based on Eqn(1)
dAD_dt <- AR # Based on Eqn(2)
TotalPopulation <- sum(stocks)
TotalPotentialAdopters <- sum(PotentialAdopters)
TotalAdopters <- sum(Adopters)
return (list(c(dPA_dt, dAD_dt),AR_R=AR,
TP=TotalPopulation,
TPA=TotalPotentialAdopters,
TAD=TotalAdopters))
})
}
o<-data.frame(ode(y=stocks, times=simtime, func = model,
parms=NULL, method="euler"))
o1<-o[seq(from=1, to=length(simtime),by=1/STEP),]
tidy<-melt(o1,id.vars = "time")
names(tidy)<-c("Time","Variable","Value")
ar<-filter(tidy,grepl("AR_",Variable))
ad<-filter(tidy,grepl("AD_",Variable))
p1<-ggplot(ar,aes(x=Time,y=Value,color=Variable)) +
geom_line() + geom_point()+
ylab("Adoption Rate") + xlab("Time (Weeks)")
p2<-ggplot(ad,aes(x=Time,y=Value,color=Variable,group=Variable)) +
geom_line() +
geom_point()+
ylab("Adopters") +
xlab("Time (Weeks)")
Z
Z
q
−α
∑
