#######################################################
##                                                   ##
##  R code to reproduce                              ##
##    The gender gap functional clustering analysis  ##
##        around the globe                           ##
##                                                   ##
##             Authors: Andrea Nigri                 ##
##           andrea.nigri88@gmail.com                ##
#######################################################



library(devtools)
library(MortalityLaws)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(fda)
library(demography)
library(HMDHFDplus)
library(scales)
library(keras)
library(tensorflow)
library(forecast)
library(reticulate)
library(urca)
library(vars)
library(tseries)
library(egcm)

# data and function loading
source("varie.R")
load("HMD_LT_F.RData")
load("HMD_LT_M.RData")

# gender gap
HMD_LT_F$data$ex <- HMD_LT_F$data$ex-HMD_LT_M$data$ex


paesi = unique(HMD_LT_F$input$countries)

# Check periods - max and min
A <- matrix(NA,length(paesi),3)
for (i in 1:length(paesi)){
  p <- HMD_LT_F$data %>% filter(country==paesi[i])
  
  A[i,1] <-unique(p[,1]) 
  A[i,2] <-max(p[,2]) 
  A[i,3] <-min(p[,2]) 
}

A[which.min(A[,3]),]
A[which.max(A[,3]),]
A[which.min(A[,2]),]
A[which.max(A[,2]),]


# let's exclude some countries
paesi = unique(HMD_LT_F$input$countries)
paesi=paesi[-c(7,8,11,18,20,22,25,28,30,34,35,36,41,43,46,47,48,49)]


# Second Check periods - max and min
A <- matrix(NA,length(paesi),3)
for (i in 1:length(paesi)){
  p <- HMD_LT_F$data %>% filter(country==paesi[i])
  
  A[i,1] <-unique(p[,1]) 
  A[i,2] <-max(p[,2]) 
  A[i,3] <-min(p[,2]) 
}

A[which.min(A[,3]),]
A[which.max(A[,3]),]
A[which.min(A[,2]),]
A[which.max(A[,2]),]


###################
#                 #
# CLUSTERING      #
#                 #
###################

# Data arranged into matrix form

anno_iniziale=1970
anno_finale=2013

ex.AUS.f = fun.ex(HMD_LT_F$data[which(HMD_LT_F$data$country==paesi[1]),],anno_iniziale,anno_finale)
ex.countries.f = t(ex.AUS.f)

for (i in 2:length(paesi)){
  ex.appo.f = fun.ex(HMD_LT_F$data[which(HMD_LT_F$data$country==paesi[i]),],anno_iniziale,anno_finale)
  ex.countries.f=cbind(ex.countries.f,t(ex.appo.f))
  }


# smoothing spline 3°

years=anno_iniziale:anno_finale
knots = seq(from = anno_iniziale, to = anno_finale, by =2)
length(knots)
norder = 5
nbasis = length(knots) + norder - 2 
basis = create.bspline.basis(range(years),nbasis,norder)
ex.countries.f.fd = smooth.basis(years,ex.countries.f, basis)$fd


# CLUSTER k-mean on Basis spline coeff.

coef.countries.f = t(ex.countries.f.fd$coefs)

# Optimal k "elbow"

elbow.f=matrix(0,nrow=7,ncol=2)

for(k in 1:7){
  
  appo = matrix(0,2000,2)
  
  for (i in 4555:6554){
    set.seed(i)
    mod.f = kmeans(coef.countries.f,k)
    appo[i-4554,1] = i
    appo[i-4554,2] = mod.f$tot.withinss
  }
  seed_minimo= appo[which(appo[,2]==min(appo[,2]))[1],1]
  elbow.f[k,]=c(seed_minimo,min(appo[,2]))
}

plot(x=1:7,elbow.f[,2], type = "l", main='Elbow Method',
     ylab="Total within-cluster sum of squares",
     xlab="Number of Clusters")
abline(v=4, col='red')

K=4

set.seed(elbow.f[K,1])
mod.f=kmeans(coef.countries.f,K)
appo = matrix(0,2000,4)

classi.f = mod.f$cluster
F.mb = matrix(0,length(paesi),anno_finale-anno_iniziale+1)
for(i in 1:32){ 
  F.mb[i,] = classi.f[i] 
}

rownames(F.mb) = paesi
colnames(F.mb) = years

#################
#               #
# Plot          #
#               #
#################

F.mb

# Let's create objects for each cluster

select1.f = fd(ex.countries.f.fd$coefs[,which(classi.f==1)],ex.countries.f.fd$basis)
select1.f$fdnames$reps=colnames(select1.f$coefs)

select2.f = fd(ex.countries.f.fd$coefs[,which(classi.f==2)],ex.countries.f.fd$basis)
select2.f$fdnames$reps=colnames(select2.f$coefs)

select3.f = fd(ex.countries.f.fd$coefs[,which(classi.f==3)],ex.countries.f.fd$basis)
select3.f$fdnames$reps=colnames(select3.f$coefs)

select4.f = fd(ex.countries.f.fd$coefs[,which(classi.f==4)],ex.countries.f.fd$basis)
select4.f$fdnames$reps=colnames(select4.f$coefs)

# Mean
par(mfrow=c(1,1))
plot(mean.fd(select1.f),col=1, lty=1, lwd=2, ylim = c(3,15))
lines(mean.fd(select2.f),col=2, lty=1,lwd=2)
lines(mean.fd(select3.f),col=3, lty=1,lwd=2)
lines(mean.fd(select4.f),col=4, lty=1,lwd=2)
title(main = "Gender Gap 1970-2013")
legend(anno_iniziale,max(select3.f$coefs)+5,
       legend=c("Low-decreasing Cluster","High-levels Cluster", 
                "Stagnating Cluster", "Increasing Cluster"), fill = 1:K)

# Clusters plots

png(file="Low_decreasing_p1.png",
    width=750, height=500)
plot(select1.f,
     ylab = "Life expectancy Gender Gap",xlab="Year")
title(main = "Low-decreasing Gap")
legend(anno_iniziale,max(select1.f$coefs)-1.8, legend=select1.f$fdnames$reps, fill = 1:length(select1.f$fdnames$reps))
dev.off()

png(file="High_p1.png",
    width=750, height=500)
plot(select2.f,
     ylab = "Life expectancy Gender Gap",xlab="Year")
title(main = "High-levels Gap")
legend(anno_iniziale,max(select2.f$coefs)-1.2, legend=select2.f$fdnames$reps, fill = 1:length(select2.f$fdnames$reps))
dev.off()


png(file="Stagnating_p1.png",
    width=750, height=500)
plot(select3.f,
     ylab = "Life expectancy Gender Gap",xlab="Year")
title(main = "Stagnating Gap")
legend(anno_iniziale,max(select3.f$coefs)-0.5, legend=select3.f$fdnames$reps, fill = 1:length(select3.f$fdnames$reps))
dev.off()

png(file="Increasing_p1.png",
    width=750, height=500)
plot(select4.f,
     ylab = "Life expectancy Gender Gap",xlab="Year")
title(main = "Increasing Gap")
legend(anno_iniziale,max(select4.f$coefs)-0.9, legend=select4.f$fdnames$reps, fill = 1:length(select4.f$fdnames$reps))
dev.off()


