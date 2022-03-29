########This script contains the code for running a conjoint experiment that will be 
#discussed in the Hertie School Data Science Workshop on experimental methods, presented by 
#Libby Jenke. 
#The code was adapted from that used to analyze the data in Jenke et al. (2020) and is available at https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/TISLDL. That portion of the code was written by Kirk Bansak.


rm(list = ls())
#install.packages("ggplot2")
#install.packages("ggthemes")
#install.packages("dplyr")
#install.packages("binom")
#install.packages("sandwich")
#install.packages("lmtest")
#install.packages("rddtools")
#install.packages("stringi")
#install.packages("effects")

library(ggplot2)
library(ggthemes)
library(dplyr)
library(binom)
library(sandwich)
library(lmtest)
library(rddtools)
library(stringi)
library(effects)

#Set working directory -- be sure that the effect plotter script is in that directory
#work_dir <-""
#setwd(work_dir)
source("./effect_plotter.R")

# Read data ---------------------------------------------------------------
cdat <- read.csv(paste("./finalData.csv"))

#List the attribute names (in order of numeric designation)
att.names <- c("age","party","race","occupation","military",
               "gender","same-sex marriage","taxation","gun control",
               "political experience","religion")
att.var.names <- c("age","party","race","prof","military",
                   "gender","ssmar","taxes","guns",
                   "polex","relig")
att.var.names.alt <- c("age","partyown","race","prof","military",
                       "gender","ssmarown","taxesown","gunsown",
                       "polex","relig")
subjs <- unique(cdat$subjid)
the.blocks <- c("5atts2cands","5atts3cands",
                "8atts2cands","8atts3cands",
                "11atts2cands","11atts3cands")

# Effect plots pooled -----------------------------------------------------
the.coefs <- data.frame()
for (k in 1:length(att.var.names)){
  #subset data by trials with attribute
  tdatt <- subset(cdat, !is.na(cdat[,att.var.names[k]]))
  #Read regression as formula, not string, and regress pref on single attribute at a time
  tmod <- lm(as.formula(paste("pref ~ as.factor(",att.var.names[k],")")), tdatt)
  #get estimate, std.error, tvalue, and p-value
  tout <- coeftest(tmod, vcov = vcovCluster(tmod, as.factor(tdatt$subjid)))
  #drop intercept
  if (k > 1){
    tout <- tout[-1,,drop = FALSE]
  }
  #Put in data frame
  toutt <- data.frame(tout[,1],
                      tout[,2],
                      tout[,3],
                      tout[,4])
  #set column and row names
  names(toutt) <- colnames(tout)
  rownames(toutt) <- rownames(tout)
  #put together
  the.coefs <- rbind(the.coefs,toutt)
  rm(tdatt,tmod,tout,toutt)
  
}

names.variables <- c("Age","Party","Race","Previous Profession","Military Service",
                     "Gender","Position on Same-Sex Marriage","Position on Tax Raise for Wealthy",
                     "Position on Gun Control","Prior Political Experience","Religion")
names.levels <- list(c("36","45","53","61","77"),
                     c("Republican","Democrat","Independent"),
                     c("White","Hispanic/Latino","Black","Asian American","Native American"),
                     c("Business executive","College professor","Lawyer","Doctor","Activist"),
                     c("Did not serve","Served in the Army","Served in the Navy","Served in the Marine Corps","Served in the Air Force"),
                     c("Male","Female"),
                     c("Strongly support","Weakly support","Weakly oppose","Strongly oppose"),
                     c("Strongly support","Weakly support","Weakly oppose","Strongly oppose"),
                     c("Strongly support","Weakly support","Weakly oppose","Strongly oppose"),
                     c("None","Mayor","Governor","U.S. Senator","U.S. Representative"),
                     c("Catholic","Evangelical Protestant","Mainline Protestant","Mormon","Jewish")
)

#The conjoint results will be in your working directory 
pdf("./conj_effects_pooled.pdf", width = 6, height = 7)
effect_plotter(model.output = the.coefs, 
                names.variables = names.variables, names.levels = names.levels,
                effect.label = "Change in Pr(Selected)")
dev.off()


