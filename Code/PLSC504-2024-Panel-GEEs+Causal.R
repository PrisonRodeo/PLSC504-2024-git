#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Preliminaries...                                   ####
#
# PLSC 504 -- Fall 2024
#
# Non-Continuous Response Panel Data models
# (including GEEs), plus causal inference with
# panel data...
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.:                                    ####
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","haven","psych","sandwich","countrycode","wbstats",
     "lme4","plm","gtools","boot","plyr","dplyr","texreg","statmod",
     "plm","tibble","pscl","naniar","ExPanDaR","stargazer","prais",
     "nlme","tseries","pcse","panelView","performance","pgmm",
     "countrycode","jstable","dynpanel","OrthoPanels","peacesciencer",
     "corrplot","rgenoud","dotwhisker","pglm","glmmML","bife","censReg",
     "geepack","fixest","did","Synth","texreg","cobalt","did",
     "modelsummary","marginaleffects")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(paste0("Package count: ",i)))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 18-20 times until you get "Package count = 48"
#
# ALSO:
# Download panelAR from the archive, then:
# install.packages("Downloads/panelAR_0.1.tar.gz", 
#                 type= "source", 
#                 repos= NULL)
library(panelAR)

# Options:

options(scipen = 40) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# setwd("~/Foo/Bar/Etc/")
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Example: WDI (again) plus data on wars and things... ####
#
# Pull the WDI data...

WDI<-read_csv("https://github.com/PrisonRodeo/PLSC504-2024-git/raw/main/Data/WDI-24.csv")

# Add a "Cold War" variable:

WDI$ColdWar <- with(WDI,ifelse(Year<1990,1,0))

# Keep a numeric year variable (for -panelAR-):

WDI$YearNumeric<-WDI$Year

# log GDP Per Capita:

WDI$lnGDPPerCap<-log(WDI$GDPPerCapita)

# ... and log Net Aid Received:

WDI$lnNetAidReceived<-log(WDI$NetAidReceived)

# summary(WDI)
#
# Add some political data... (note: this is why
# you loaded the -peacesciencer- package above):

create_stateyears(system="gw",
                  subset_years=c(1960:2021)) %>%
  add_ccode_to_gw() %>%
  add_ucdp_acd(type="intrastate", only_wars = FALSE) %>%
  add_ucdp_onsets() %>%
  add_democracy() -> DF

DF$Year<-DF$year
DF$ISO3<-countrycode(DF$gwcode,"gwn","iso3c")
nc<-ncol(DF)
nd<-nc-1
ne<-nc-2 # kludgey af

DF<-DF[,c(nc,nd,1:ne)] # order variables
DF<-DF[order(DF$ISO3,DF$Year),] # sort data

# Merge:

Data<-merge(WDI,DF,by=c("ISO3","Year"),
            all=TRUE)

Data<-Data[order(Data$ISO3,Data$Year),] # sort
rm(DF) # clean up

# Zap some missingness...

Data<-Data[is.na(Data$ISO3)==FALSE,]

# Make the data a panel dataframe:

Data<-pdata.frame(Data,index=c("ISO3","Year"))

# #=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# # Simulation: unit effects and binary outcomes  ####
#
# This code is commented out; it shows (via simulation)
# how different types of unit-effects models for binary
# variables do and do not work. 
#
# Active code resumes around line #263...
# 
# set.seed(7222009)
# 
# reps<-100
# N<-100
# T<-100
# NT<-N*T
# 
# MSlogit<-matrix(data=NA,nrow=reps,ncol=3)
# FElogit<-matrix(data=NA,nrow=reps,ncol=3)
# MSClogit<-matrix(data=NA,nrow=reps,ncol=3)
# FEClogit<-matrix(data=NA,nrow=reps,ncol=3)
# FECprobit<-matrix(data=NA,nrow=reps,ncol=3)
# REBs<-matrix(data=NA,nrow=reps,ncol=3)
# 
# for(i in 1:reps){
#   
#   alpha <- rnorm(N)
#   alphas <- rep(alpha, 100)
#   X <- rnorm(NT) # Uncorrelated X
#   XCorr <- 0.5*alphas + 0.5*rnorm(NT) # X, alpha correlated
#   D <- rbinom(NT,1,0.5) # binary predictor
#   Ystar <- 0 + 1*X + 1*D + alphas # latent Y, Cov(X,alpha)=0
#   YCstar <- 0 + 1*XCorr + 1*D + alphas # latent Y, Cov(X,alpha)>0
#   
#   Y <- rbinom(NT,1,plogis(Ystar))
#   YC <-  rbinom(NT,1,plogis(YCstar))
#   YPC <-  rbinom(NT,1,pnorm(YCstar))
#   
#   fool<-glm(Y~X+D,family="binomial")
#   foolFE<-glm(Y~X+D+as.factor(alphas),family="binomial")
#   foolC<-glm(YC~XCorr+D,family="binomial")
#   foolCFE<-glm(YC~XCorr+D+as.factor(alphas),family="binomial")
#   probitFEC<-glm(YPC~XCorr+D+as.factor(alphas),
#                  family="binomial"(link="probit"))
#   RE<-glmmML(YC~XCorr+D, family="binomial",
#              cluster=as.factor(alphas))
#   
#   MSlogit[i,]<-fool$coefficients[1:3]
#   FElogit[i,]<-foolFE$coefficients[1:3]
#   MSClogit[i,]<-foolC$coefficients[1:3]  
#   FEClogit[i,]<-foolCFE$coefficients[1:3]  
#   FECprobit[i,]<-probitFEC$coefficients[1:3]
#   REBs[i,]<-RE$coefficients[1:3]
#   
# }
# 
# pdf("BinaryPanelSimsUncorrAlphas.pdf",6,5)
# par(mar=c(4,4,2,2))
# plot(density(MSlogit[,2]),xlim=c(0.6,1.1),lwd=2,
#      lty=3,col="red",main="",ylim=c(0,17),
#      xlab="Estimated Betas (True value = 1.0)")
# lines(density(FElogit[,2]),lwd=2)
# abline(v=1,lty=2,lwd=2)
# legend("topleft",lwd=2,col=c("black","red"),lty=c(1,3),
#        legend=c("Fixed Effects","No Fixed Effects"),
#        bty="n")
# dev.off()
# 
# pdf("BinaryPanelSimsCorrAlphas.pdf",6,5)
# par(mar=c(4,4,2,2))
# plot(density(MSClogit[,2]),xlim=c(0.6,2.1),ylim=c(0,10),
#      lwd=2,lty=3,col="red",main="",
#      xlab="Estimated Betas (True value = 1.0)")
# lines(density(FEClogit[,2]),lwd=2)
# abline(v=1,lty=2,lwd=2)
# legend("topright",lwd=2,col=c("black","red"),lty=c(1,3),
#        legend=c("Fixed Effects","No Fixed Effects"),
#        bty="n")
# dev.off()
# 
# pdf("BinaryPanelSimsCorrProbitRE.pdf",6,5)
# par(mar=c(4,4,2,2))
# plot(density(REBs[,2]),xlim=c(0.8,1.4),ylim=c(0,10),
#      lwd=2,lty=3,col="red",main="",
#      xlab="Estimated Betas (True value = 1.0)")
# lines(density(FEClogit[,2]),lwd=2)
# abline(v=1,lty=2,lwd=2)
# legend("topright",lwd=2,col=c("black","red"),lty=c(1,3),
#        legend=c("Fixed Effects Logit","Random Effects Logit"),
#        bty="n")
# dev.off()
# 
# # Now; What if we do that last bit with T=5?
# 
# set.seed(7222009)
# 
# reps<-100
# N<-100
# T<-5
# NT<-N*T
# 
# MSlogit<-matrix(data=NA,nrow=reps,ncol=3)
# FElogit<-matrix(data=NA,nrow=reps,ncol=3)
# MSClogit<-matrix(data=NA,nrow=reps,ncol=3)
# FEClogit<-matrix(data=NA,nrow=reps,ncol=3)
# FECprobit<-matrix(data=NA,nrow=reps,ncol=3)
# REBs<-matrix(data=NA,nrow=reps,ncol=3)
# 
# for(i in 1:reps){
#   
#   alpha <- rnorm(N)
#   alphas <- rep(alpha,T)
#   X <- rnorm(NT) # Uncorrelated X
#   XCorr <- 0.5*alphas + 0.5*rnorm(NT) # X, alpha correlated
#   D <- rbinom(NT,1,0.5) # binary predictor
#   Ystar <- 0 + 1*X + 1*D + alphas # latent Y, Cov(X,alpha)=0
#   YCstar <- 0 + 1*XCorr + 1*D + alphas # latent Y, Cov(X,alpha)>0
#   
#   Y <- rbinom(NT,1,plogis(Ystar))
#   YC <-  rbinom(NT,1,plogis(YCstar))
#   YPC <-  rbinom(NT,1,pnorm(YCstar))
#   
#   fool<-glm(Y~X+D,family="binomial")
#   foolFE<-glm(Y~X+D+as.factor(alphas),family="binomial")
#   foolC<-glm(YC~XCorr+D,family="binomial")
#   foolCFE<-glm(YC~XCorr+D+as.factor(alphas),family="binomial")
#   probitFEC<-glm(YPC~XCorr+D+as.factor(alphas),
#                  family="binomial"(link="probit"))
#   RE<-glmmML(YC~XCorr+D, family="binomial",
#              cluster=as.factor(alphas))
#   
#   MSlogit[i,]<-fool$coefficients[1:3]
#   FElogit[i,]<-foolFE$coefficients[1:3]
#   MSClogit[i,]<-foolC$coefficients[1:3]  
#   FEClogit[i,]<-foolCFE$coefficients[1:3]  
#   FECprobit[i,]<-probitFEC$coefficients[1:3]
#   REBs[i,]<-RE$coefficients[1:3]
#   
# }
# 
# # Plot:
# 
# pdf("BinaryPanelSimsCorrProbitRET5.pdf",6,5)
# par(mar=c(4,4,2,2))
# plot(density(REBs[,2]),xlim=c(0,2.5),ylim=c(0,2.5),
#      lwd=2,lty=3,col="red",main="",
#      xlab="Estimated Betas (True value = 1.0)")
# lines(density(FEClogit[,2]),lwd=2)
# abline(v=1,lty=2,lwd=2)
# legend("topright",lwd=2,col=c("black","red"),lty=c(1,3),
#        legend=c("Fixed Effects Logit","Random Effects Logit"),
#        bty="n")
# dev.off()
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Back to the WDI + other data...                ####
#
# Prep things:

Data$PopMillions<-Data$Population/1000000
Data$CivilWar<-Data$ucdpongoing
Data$OnsetCount<-Data$sumonset1
Data$POLITY<-(Data$polity2+10)/2
Data$POLITYSquared<-Data$POLITY^2
Data$PostColdWar<-1-Data$ColdWar

# Summary statistics on the variables we're using...

vars<-c("ISO3","Year","country","CivilWar","OnsetCount",
        "LandArea","PopMillions","UrbanPopulation",
        "GDPPerCapita","GDPPerCapGrowth","PostColdWar",
        "POLITY","POLITYSquared")

DF<-Data[,vars]

describe(DF,skew=FALSE)

# Make panel data:

DF<-pdata.frame(DF,index=c("ISO3","Year"))

# Variation in civil wars:

summary(DF$CivilWar)
# summary(Between(CW,effect="individual",na.rm=TRUE))
# summary(Within(CW,effect="individual",na.rm=TRUE))

# Pooled logit model of civil war:

Logit<-glm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+log(GDPPerCapita)+
             GDPPerCapGrowth+PostColdWar+POLITY+POLITYSquared,data=DF,family="binomial")
summary(Logit)

# Fixed Effects logit model:

FELogit<-bife(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+log(GDPPerCapita)+
                GDPPerCapGrowth+PostColdWar+POLITY+POLITYSquared|ISO3,data=DF,model="logit")

summary(FELogit)

# Random Effects:

RELogit<-pglm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+log(GDPPerCapita)+
                GDPPerCapGrowth+PostColdWar+POLITY+POLITYSquared|ISO3,data=DF,family=binomial,
              effect="individual",model="random")

summary(RELogit)

# A table:

texreg(list(Logit,FELogit,RELogit),
       custom.model.names=c("Logit","FE Logit","RE Logit"),
       custom.coef.names=c("Intercept","ln(Land Area)","ln(Population)",
                           "Urban Population","ln(GDP Per Capita)","GDP Growth",
                           "Post-Cold War","POLITY","POLITY Squared",
                           "Estimated Sigma"), 
       stars=0.05)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Panel Data & Event counts...                       ####
# 
# See below (line #742)...
#
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Generalized Estimating Equations (GEEs)            ####
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#
# Back to the binary Civil War variable...
#
# Zap all the missingness, to make our life
# a little easier:

DF<-DF[complete.cases(DF),]

# Also, for comparability across different types of GEE
# models, we'll only look at data from 2013-2017, and 
# remove the "Post-Cold War" variable. (Fitting GEE models
# with unstructured correlation structures is a *bad* 
# idea with a big T...).

DF$flag<-ifelse(as.numeric(as.character(DF$Year))<2018 & 
                  as.numeric(as.character(DF$Year))>2012,1,0)
DF5<-DF[DF$flag==1,]

# GEE Models:
#
# Independence:

GEE.ind<-geeglm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+
                  log(GDPPerCapita)+GDPPerCapGrowth+POLITY+POLITYSquared,
                  data=DF5,id=ISO3,family="binomial",corstr="independence")
summary(GEE.ind)

# Exchangeable correlation:

GEE.exc<-geeglm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+
                    log(GDPPerCapita)+GDPPerCapGrowth+POLITY+POLITYSquared,
                  data=DF5,id=ISO3,family="binomial",corstr="exchangeable")
summary(GEE.exc)


# AR(1) correlation:

GEE.ar1<-geeglm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+
                  log(GDPPerCapita)+GDPPerCapGrowth+POLITY+POLITYSquared,
                  data=DF5,id=ISO3,family="binomial",corstr="ar1")
summary(GEE.ar1)

# Unstructured correlation...

GEE.unstr<-geeglm(CivilWar~log(LandArea)+log(PopMillions)+UrbanPopulation+
                    log(GDPPerCapita)+GDPPerCapGrowth+POLITY+POLITYSquared,
                    data=DF5,id=ISO3,family="binomial",corstr="unstructured")
summary(GEE.unstr)

# Plot the betas / SEs:

betas<-cbind(GEE.ind$coefficients,GEE.exc$coefficients,
             GEE.ar1$coefficients,
             GEE.unstr$coefficients)
betas<-betas[-1,]

pdf("GEE-NewBetasR-24.pdf",7,7)
par(mar=c(4,4,2,2))
scatterplotMatrix(betas[-1,],smooth=FALSE,
                  var.labels=c("Indep.","Exch.","AR1","Unstr."),
                  pch=19,diagonal=FALSE)
dev.off()

# SEs:

ses<-cbind(sqrt(diag(GEE.ind$geese$vbeta)),
           sqrt(diag(GEE.exc$geese$vbeta)),
           sqrt(diag(GEE.ar1$geese$vbeta)),
           sqrt(diag(GEE.unstr$geese$vbeta)))
ses<-ses[-1,]

pdf("GEE-NewSEsR-24.pdf",7,7)
par(mar=c(4,4,2,2))
scatterplotMatrix(ses[-1,],smooth=FALSE,
                  var.labels=c("Ind","Exch","AR1","Unstr"),
                  pch=19,diagonal=FALSE,xlim=c())
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Causal Inference Redux...                           ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# World Development Indicators data (yet again)...

# Pull the "alternative" WDI data...

WDI<-read_csv("https://github.com/PrisonRodeo/PLSC504-2024-git/raw/main/Data/WDI2-24.csv")

# log GDP Per Capita:

WDI$lnGDPPerCap<-log(WDI$GDPPerCapita)

# ... and log Net Aid Received:

WDI$lnNetAidReceived<-log(WDI$NetAidReceived)

# Keep a numeric year variable (for -panelAR-):

WDI$YearNumeric<-WDI$Year

# Add a "Post-Cold War" variable:

WDI$PostColdWar <- with(WDI,ifelse(YearNumeric<1990,0,1))

# summary(WDI)
#
# Make the data a panel dataframe:

WDI<-pdata.frame(WDI,index=c("ISO3","Year"))

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Descriptive statistics on the WDI data      ####

describe(WDI,fast=TRUE,ranges=FALSE,check=TRUE)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Some Regressions...                  ###########
#
# First, a simple preliminary investigation...
#
# Create logged Child Mortality variable:

WDI$lnCM <- log(WDI$ChildMortality)

# T-test (not reproduced):

t.test(lnCM~PaidParentalLeave,data=WDI)

# Bivariate regression:

BIV<-lm(lnCM~PaidParentalLeave,data=WDI)

# OLS:

OLS<-lm(lnCM~PaidParentalLeave+lnGDPPerCap+lnNetAidReceived+GovtExpenditures,data=WDI)

# Fixed Effects... One-way:

FE.1way<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+lnNetAidReceived+GovtExpenditures,
             data=WDI,effect="individual",model="within")

# Two-way:

FE.2way<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+lnNetAidReceived+GovtExpenditures,
             data=WDI,effect="twoway",model="within")


FE.LDV<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+lnNetAidReceived+GovtExpenditures+
              lag(ChildMortality),data=WDI,effect="individual",model="within")

# A nice table:

MortTable1 <- stargazer(BIV,OLS,FE.1way,FE.2way,FE.LDV,
                        title="Models of log(Child Mortality)",
                        column.separate=c(1,1),align=TRUE,
                        dep.var.labels.include=FALSE,p=c(0.05),
                        dep.var.caption="",omit.stat=c("f","ser"),
                        covariate.labels=c("Paid Parental Leave","ln(GDP Per Capita)",
                                           "ln(Net Aid Received)",
                                           "Government Expenditures",
                                           "Lagged Child Mortality"),
                        column.labels=c("Bivariate OLS","OLS","One-Way FE",
                                        "Two-Way FE","FE w.Lagged Y"),
                        header=FALSE,model.names=FALSE,
                        model.numbers=FALSE,multicolumn=FALSE,
                        object.names=TRUE,notes.label="",
                        out="ChildMortTable1-24.tex")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# INSTRUMENTAL VARIABLES!                           ####
#
# We'll instrument Paid Parental Leave with the
# Women in Legislature variable:

with(WDI,t.test(WomenInLegislature~PaidParentalLeave))

pdf("IVBoxplot-24.pdf",7,5)
par(mar=c(4,4,2,2))
boxplot(WomenInLegislature~PaidParentalLeave,data=WDI,
        xlab="Paid Parental Leave",
        ylab="Percent of Legislative Seats Held By Women")
dev.off()

# IV regressions (one-way FE and RE):

FE.IV<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+
             lnNetAidReceived+GovtExpenditures |
             .-PaidParentalLeave+WomenInLegislature,
           data=WDI,effect="individual",model="within")


RE.IV<-plm(lnCM~PaidParentalLeave+lnGDPPerCap+
             lnNetAidReceived+GovtExpenditures |
             .-PaidParentalLeave+WomenInLegislature,
           data=WDI,effect="individual",model="random")

# A nice table:

IVTable<-stargazer(OLS,FE.1way,FE.IV,RE.IV,
                   title="IV Models of log(Child Mortality)",
                   column.separate=c(1,1),align=TRUE,
                   dep.var.labels.include=FALSE,p=c(0.05),
                   dep.var.caption="",omit.stat=c("f","ser"),
                   covariate.labels=c("Paid Parental Leave","ln(GDP Per Capita)",
                                      "ln(Net Aid Received)",
                                      "Government Expenditures"),
                   column.labels=c("OLS","One-Way FE","FE w/IV","RE w/IV"),
                   header=FALSE,model.names=FALSE,
                   model.numbers=FALSE,multicolumn=FALSE,
                   object.names=TRUE,notes.label="",
                   out="ChildMortTable2-24.tex")


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Simple RDD...                                     ####
#
# Pull out *only* those countries that, at some
# point during the observed periods, instituted
# a paid parental leave policy:

PPLs<-WDI
PPLs<-PPLs %>% group_by(ISO3) %>%
  filter(any(PaidParentalLeave==1))
PPLs$YearNumeric<-as.numeric(as.character(PPLs$YearNumeric)) # fix

# Plot the trends by value of PaidParentalLeave: 

pdf("PPL-Plot-24.pdf",7,5)
par(mar=c(4,4,2,2))
with(PPLs[PPLs$PaidParentalLeave==0,],
     plot(YearNumeric,log(ChildMortality),
          pch=20,col="darkorange",xlab="Year",
          ylim=c(0.7,5.3)))
with(PPLs[PPLs$PaidParentalLeave==1,],
     points(YearNumeric,log(ChildMortality),
            pch=17,col="forestgreen"))
legend("topright",bty="n",pch=c(20,17),
       col=c("darkorange","forestgreen"),
       legend=c("No Paid Parental Leave",
                "Paid Parental Leave"))
dev.off()

# Create a better trend variable:

PPLs$Time<-PPLs$YearNumeric-1950

# Create interaction term:

PPLs$PPLxTime<-PPLs$PaidParentalLeave * PPLs$Time

# REGRESSION TIME 

RDD.OLS1<-lm(lnCM~PaidParentalLeave+Time+PPLxTime,data=PPLs)

RDD.OLS2<-lm(lnCM~PaidParentalLeave+Time+PPLxTime+lnGDPPerCap+
               lnNetAidReceived+GovtExpenditures,data=PPLs)

# FE models...
#
# PPLs<-pdata.frame(PPLs,index=c("ISO3","Year")) # make panel data

RDD.1way.1<-plm(lnCM~PaidParentalLeave+Time+PPLxTime,data=PPLs,
                effect="individual",model="within")

RDD.1way.2<-plm(lnCM~PaidParentalLeave+Time+PPLxTime+lnGDPPerCap+
                  lnNetAidReceived+GovtExpenditures,
                data=PPLs,effect="individual",model="within")

RDD.2way.1<-plm(ChildMortality~PaidParentalLeave+Time+PPLxTime,data=PPLs,
                effect="twoway",model="within")

RDD.2way.2<-plm(ChildMortality~PaidParentalLeave+Time+PPLxTime+lnGDPPerCap+
                  lnNetAidReceived+GovtExpenditures,
                data=PPLs,effect="twoway",model="within")

# TABLE TIME...
#
# -stargazer- doesn't seem to be working for me right now
# (October 2024), but I'll put the code here anyway:
#
# RDDMortTable<-stargazer(RDD.OLS1,RDD.OLS2,RDD.1way.1,RDD.1way.2,
#                         RDD.2way.1,RDD.2way.2,
# title="RDD Models of log(Child Mortality)",
# column.separate=c(1,1,1),align=TRUE,
# dep.var.labels.include=FALSE,
# dep.var.caption="",
# covariate.labels=c("Paid Parental Leave","Time (1950=0)",
#                    "Paid Parental Leave x Time",
#                    "ln(GDP Per Capita)",
#                    "ln(Net Aid Received)",
#                    "Government Expenditures"),
# header=FALSE,model.names=FALSE,
# model.numbers=FALSE,multicolumn=FALSE,
# object.names=TRUE,notes.label="",
# column.sep.width="-15pt",order=c(1,2,6,3,4,5),
# omit.stat=c("f","ser"),out="RDDMortRegs-24.tex")

# Alternatives:

RDDs<-list("OLS #1"=RDD.OLS1,"OLS #2"=RDD.OLS2,
           "One-Way FE #1"=RDD.1way.1,"One-Way FE #2"=RDD.1way.2,
           "Two-Way FE #1"=RDD.2way.1)

# Note that RDD.2way.2 is singular....)

modelsummary(RDDs,output="RDDMortRegs-24.tex",stars=TRUE,
             title="RDD Models of log(Child Mortality)",
             fmt=4,gof_map=c("nobs","r.squared","adj.r.squared"),
             coef_rename=c("(Intercept)","Paid Parental Leave",
                           "Time (1950=0)","Paid Parental Leave x Time",
                           "ln(GDP Per Capita)","ln(Net Aid Received)",
                           "Government Expenditures"))

# Note that you could also use -texreg-:
# 
# texreg(list(RDD.OLS1,RDD.OLS2,RDD.1way.1,RDD.1way.2,
#             RDD.2way.1,RDD.2way.2),file="RDDMortRegs-24A.tex",
#             caption="",label="")


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# DiD                                                 ####
#
# Finally, a differences-in-differences (DiD) example. This
# uses the -did- package, which deals with DiD models where
# there are more than two "periods" and/or staggered
# "treatment" timings.
#
# Ensure YEARNUMERIC is in fact numeric, and sort:

WDI$YearNumeric<-as.numeric(as.character(WDI$YearNumeric))
WDI<-WDI[order(WDI$ISO3,WDI$YearNumeric),] # sort the data

# Create a numeric unit ID variable:

WDI$ID<-as.numeric(WDI$ISO3)

# Now define a variable that equals the first year
# in which that country had paid parental leave:

WDI <- WDI %>%
  group_by(ID) %>%
  mutate(foo = ifelse(PaidParentalLeave==1,YearNumeric,NA)) %>%
  mutate(YearPPL = min(foo,na.rm=TRUE)) %>%
  ungroup()

WDI$foo<-NULL # clean up

# Recode those "INFs" to 2023:

WDI$YearPPL<-ifelse(WDI$YearPPL==Inf,2023,WDI$YearPPL)

# Now fit the simple / bivariate DiD model:

DiD.fit1<-att_gt(yname = "lnCM",gname = "YearPPL",idname = "ID",
                 tname = "YearNumeric",allow_unbalanced_panel = TRUE,
                 xformla = ~1,data = WDI,est_method = "reg")

# Event study object:

DiD.ev1 <- aggte(DiD.fit1,type="dynamic",na.rm=TRUE)

# Make an event study plot:

pdf("DiDEventPlot1-24.pdf",7,4)
par(mar=c(4,4,2,2))
ggdid(DiD.ev1,xgap=10)
dev.off()

# Total group effects:

DiD.grp1<-aggte(DiD.fit1,type="group",na.rm=TRUE)
summary(DiD.grp1)

# Plotted:

pdf("DiDGroupEffects1-24.pdf",5,6)
ggdid(DiD.grp1)
dev.off()

# Finally, add the three control variables:

DiD.fit2<-att_gt(yname = "lnCM",gname = "YearPPL",idname = "ID",
                 tname = "YearNumeric",allow_unbalanced_panel = TRUE,
                 xformla = ~lnGDPPerCap+lnNetAidReceived+GovtExpenditures,
                 data = WDI, est_method = "reg")

# Event study object:

DiD.ev2 <- aggte(DiD.fit2,type="dynamic",na.rm=TRUE)

# Make an event study plot:

pdf("DiDEventPlot2-24.pdf",7,4)
par(mar=c(4,4,2,2))
ggdid(DiD.ev1,xgap=10)
dev.off()

# Total group effects:

DiD.grp2<-aggte(DiD.fit2,type="group",na.rm=TRUE)
summary(DiD.grp2)

# Plotted:

pdf("DiDGroupEffects2-24.pdf",5,6)
ggdid(DiD.grp2)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Appendix (various)                            ####
#
# Event Counts                                  ####

xtabs(~DF$OnsetCount)

# Basic Poisson (no panel effects...)

Poisson<-glm(OnsetCount~log(LandArea)+log(PopMillions)+
               UrbanPopulation+log(GDPPerCapita)+
               GDPPerCapGrowth+PostColdWar+POLITY+
               POLITYSquared,data=DF,family="poisson")
summary(Poisson)


# Fixed effects Poisson:

FEPoisson<-pglm(OnsetCount~log(LandArea)+log(PopMillions)+
                  UrbanPopulation+log(GDPPerCapita)+
                  GDPPerCapGrowth+PostColdWar+POLITY+
                  POLITYSquared,data=DF,family="poisson",
                effect="individual",model="within")
summary(FEPoisson)

# alternative, using -fixest-:

FEPoisson2<-feglm(OnsetCount~log(LandArea)+log(PopMillions)+
                    UrbanPopulation+log(GDPPerCapita)+
                    GDPPerCapGrowth+PostColdWar+POLITY+
                    POLITYSquared|ISO3,data=DF,family="poisson")
summary(FEPoisson2,cluster="ISO3")

# Random effects Poisson

REPoisson<-glmer(OnsetCount~log(LandArea)+log(PopMillions)+
                   UrbanPopulation+log(GDPPerCapita)+
                   GDPPerCapGrowth+PostColdWar+POLITY+
                   POLITYSquared+(1|ISO3),data=DF,family="poisson")
summary(REPoisson)

# Another RE Poisson, using pglm (slightly different from the
# previous two):

REPoisson2<-pglm(OnsetCount~log(LandArea)+log(PopMillions)+
                   UrbanPopulation+log(GDPPerCapita)+
                   GDPPerCapGrowth+PostColdWar+POLITY+
                   POLITYSquared,data=DF,family="poisson",
                 effect="individual",model="random")
summary(REPoisson2)

# Can also do RE Poisson using glmmML...
#
# Basic / pooled negative binomial:

NegBin<-pglm(OnsetCount~log(LandArea)+log(PopMillions)+
               UrbanPopulation+log(GDPPerCapita)+
               GDPPerCapGrowth+PostColdWar+POLITY+
               POLITYSquared,data=DF,
             family="negbin",model="pooling")
summary(NegBin)

# Negative binomial with fixed effects:

FENegBin<-pglm(OnsetCount~log(LandArea)+log(PopMillions)+
                 UrbanPopulation+log(GDPPerCapita)+
                 GDPPerCapGrowth+PostColdWar+POLITY+
                 POLITYSquared,data=DF,family="negbin",
               effect="individual",model="within")
summary(FENegBin)

# (Can also use -fenegbin-...)
#
# Same, with random effects:

RENegBin<-glmer.nb(OnsetCount~log(LandArea)+log(PopMillions)+
                     UrbanPopulation+log(GDPPerCapita)+
                     GDPPerCapGrowth+PostColdWar+POLITY+
                     POLITYSquared+(1|ISO3),data=DF,
                   verbose=TRUE)
summary(RENegBin)

# Table?

texreg(list(Poisson,FEPoisson,REPoisson,NegBin,FENegBin,RENegBin),
       custom.model.names=c("Poisson","FE Poisson","RE Poisson",
                            "Neg. Bin.","FE N.B.","RE N.B."),
       custom.coef.names=c("Intercept","ln(Land Area)","ln(Population)",
                           "Urban Population","ln(GDP Per Capita)","GDP Growth",
                           "Post-Cold War","POLITY","POLITY Squared",
                           "Estimated Sigma"), 
       stars=0.05)

# \fin