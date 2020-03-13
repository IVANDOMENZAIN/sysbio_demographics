#!/usr/bin/env Rscript
# Ivan Domenzain. 2020-03-12
library(ggplot2)
library(dplyr)
library(ggrepel)
library(viridis)
library(tidyr)
#Set directory, change this line according to your local path for this repo
setwd('/Users/ivand/Documents/GitHub/sysbio_demographics/code')
source('plotResults.R')
#Load Dataset
dataset <- read.csv(file = '../data/sysbio_data.txt', sep = '\t', header = TRUE, stringsAsFactors = FALSE)
#Modify some values
dataset[is.na(dataset[,3]),3] <- 'NA'
dataset[is.na(dataset[,4]),4] <- 'NA'
dataset[which(dataset[,3]=='wet_dry',3),3] <- 'mixed'
setwd('/Users/ivand/Documents/GitHub/sysbio_demographics/results')
#Analyse group composition per position categories
column <- which(colnames(dataset)=='Category')
plotTitle <- 'employee_category.png'
png(plotTitle,width = 600, height = 600)
p <- getPieChart(dataset,column,10,'',0.8)
dev.off()
#Analyse group composition per gender categories
column <- which(colnames(dataset)=='gender')
plotTitle <- 'employee_gender.png'
png(plotTitle,width = 600, height = 600)
p <- getPieChart(dataset,column,10,'',0.8)
dev.off()
#Get gender composition for each staff category
categories <- unique(dataset$Category)
genders    <- unique(dataset$gender)
V1     <- c()
V2     <- c()
counts <- c()
for (cat in categories){
  #Count gender by employee category
  for (gen in genders){
    V1 <- c(V1,cat)
    V2 <- c(V2,gen)
    counts <- c(counts,sum(1*(dataset$Category==cat & dataset$gender==gen)))
  }
}
#Get stacked bar plots for gender composition across employee's categories
newDF <- data.frame(V1,V2,counts)
DF    <- newDF[which(newDF$V1!='Visiting researcher' & newDF$V1!='Guests' & newDF$V1!='Master student'),]
DF$V1 <- factor(DF$V1, levels = unique(DF$V1))
for (mode in c('fill','stack')){
  plotTitle <- paste('employees_gender_',mode,'.png',sep='')
  png(plotTitle,width = 900, height = 600)
  if (mode=='stack'){yLabel <- 'Number of employees'}
  else{yLabel <- 'Percentage of employees'}
  barPlot_2vars(DF,'',yLabel,'Gender',9,mode)
  dev.off()
}
#Employees nationalities
column    <- which(colnames(dataset)=='nationality')
plotTitle <- 'employee_nationality.png'
png(plotTitle,width = 700, height = 700)
p <- getPieChart(dataset,column,9,'',0.8)
dev.off()
#Employees geo-cultural regions
column <- which(colnames(dataset)=='region')
plotTitle <- 'employee_region.png'
png(plotTitle,width = 600, height = 600)
p <- getPieChart(dataset,column,12,'',0.8)
dev.off()
#Divide by Wet and Dry lab
newDF <- dataset[which(dataset$wet_dry!='NA'),]
#Get overall labor division of the group
column <- which(colnames(dataset)=='wet_dry')
plotTitle <- 'WetDry.png'
png(plotTitle,width = 600, height = 600)
p <- getPieChart(newDF,column,10,'',0.8)
dev.off()
#Get labor division by gender
V1     <- c()
V2     <- c()
counts <- c()
for (gen in c('F','M')){
  DF <- newDF[which(newDF$gender==gen),]
  plotTitle <- paste('WetDry_',gen,'.png',sep='')
  png(plotTitle,width = 600, height = 600)
  p <- getPieChart(DF,column,10,'',0.8)
  dev.off()
  for (lab in unique(DF$wet_dry)){
    V1 <- c(V1,lab)
    V2 <- c(V2,gen)
    counts <- c(counts,sum(1*(DF$wet_dry==lab & DF$gender==gen)))
  }
}
#Get stack bar plots for gender composition in each lab category
newDF <- data.frame(V1,V2,counts)
DF    <- newDF
DF$V1 <- factor(DF$V1, levels = unique(DF$V1))
for (mode in c('fill','stack')){
  plotTitle <- paste('WetDry_gender_',mode,'.png',sep='')
  png(plotTitle,width = 900, height = 600)
  if (mode=='stack'){yLabel <- 'Number of employees'}
  else{yLabel <- 'Percentage of employees'}
  barPlot_2vars(DF,'',yLabel,'Gender',9,mode)
  dev.off()
}
#Get geographical diversity per lab category
newDF <- dataset[which(dataset$wet_dry!='NA'),]
for (lab in unique(newDF$wet_dry)){
  DF <- newDF[which(newDF$wet_dry==lab),]
  column    <- which(colnames(DF)=='region')
  plotTitle <- paste(lab,'Lab_region.png',sep='')
  png(plotTitle,width = 600, height = 600)
  p <- getPieChart(DF,column,12,'',0.8)
  dev.off()
}





