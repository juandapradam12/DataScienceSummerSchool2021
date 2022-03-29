################################################################################
################################################################################
#
#       Illustration MrP, MrsP, and autoMrP
#
#       Lucas Leemann, leemann@ipz.uzh.ch
#
################################################################################
################################################################################



library(foreign)
library(lme4)
library(extrafont)
library(swissMrP) # you do not need it
library(arm)
library(autoMrP)
library(haven)
library(lme4)

setwd("/home/juan-prada/Documentos/DataScienceSummerSchool2021/ModernSurveyMethods/Lab")

data1 <- read_dta("Minaret_B.dta")


head(data1)

table(data1$Minaret)

# Exclude observations where respondents have NA on vote question
data2 <- data1[-which(is.na(data1$Minaret)),]
summary(data2)

# Let's look at the normal national estimate for the YES vote share
table(data2$Minaret)/sum(table(data2$Minaret))




#############################################################################
# MrP -- Response model with education, age, and gender


model1 <- glmer(Minaret ~ 1 + (1|female) + (1|agegroup) + (1|educ) + (1|canton), data= data2, family=binomial("probit"))
summary(model1)

# Note: When variances of RE are very small one sometimes gets an estimate of 0.
# i.e. the realization of the male RE is 0 as well as for the female RE realization.

# Read out the RE realizations
re.female <- ranef(model1)$female[[1]]
re.agegroup <- ranef(model1)$agegroup[[1]]
re.educ <- ranef(model1)$educ[[1]]
re.canton <- ranef(model1)$canton[[1]]



#############################################################################
# Ideal Types


# We have  2 * 4 * 6 (=48) ideal types in a given canton and overall 26 * 48 (=1248) ideal types
# Let's take a look at the census data to see how we should 'order' them:
load("Census.Rda")
Censusobject
??swissMrP::census


# Let's recreate this sequence
female.re <- rep(re.female,24)
age.re <- rep(kronecker(re.agegroup,c(1,1)), 6)
educ.re <- kronecker(re.educ,rep(1, 8))

# The three objects above hold the contribution of the respective variable (educ, age, gender)
# for each of the 48 ideal types. Now we add them together and also add teh constant
ind.re <- rowSums(cbind(female.re, age.re, educ.re))
ind.re <- ind.re + fixef(model1)


# In a next step, we create the 'latent score' for each ideal type in each canton
y.lat1 <- rep(NA,1248)
for (i in 1:26){
  a <- ((i-1)*48)+1
  b <- a + 47
  y.lat1[a:b] <- ind.re + re.canton[i]
}

# To generate predicted probabilities of voting YES, we need to translate the 'latent score' to
# predicted probabilities and we do so by using the cumulative normal distribution (response model
# is probit)
p1 <- pnorm(y.lat1)


#############################################################################
# Post-Stratification Step -- weigh each ideal type by its frequency in the target population
dim(Censusobject)
a <- c()
for (i in 1:26){
  a <- c(a,Censusobject[,i])
}

# Estimate
sum(p1*a)/sum(a)
# Estimate is 58% -- actual result was 57.5% 



#############################################################################
#############################################################################
# Adding context-level variables to the response-model


# Let's read in data on cantonal outcomes as well as on a potential context-level variable

MINARET <- c(51.80,60.80,61.20,63.80,66.30,62.40,62.80,68.80,56.70,55.90,64.00,48.40,59.90,63.50,63.70,71.40,65.90,
             58.60,64.00,67.70,68.30,46.90,58.00,49.30,40.30,51.20)/100

rightP <- c(39.30,36.70,44.30,46.50, 59.90,47.10,49.10,48.90,44.30,27.00,41.40,28.50,35.20,42.80,42.60,48.30,48.30,
            34.90,46.80, 48.90,42.20,19.00,25.00,18.00,17.90,19.80)/100
# source: https://www.bk.admin.ch/ch/d/pore/va/20080601/can532.html

# Let's merge this new data with the survey data
cantonid <- c(1:26)
data3 <- cbind(rightP,cantonid)
data4 <- merge(data2,data3,by="cantonid")

#############################################################################
# New Response Model - adding right strength in past referendum to the model
model2 <- glmer(Minaret ~ rightP + (1|female) + (1|agegroup) + (1|educ) + 
                  (1|cantonid), data= data4, family=binomial("probit"))
summary(model2)


#############################################################################
# Ideal types
re.female <- ranef(model2)$female[[1]]
re.agegroup <- ranef(model2)$agegroup[[1]]
re.educ <- ranef(model2)$educ[[1]]
re.canton <- ranef(model2)$canton[[1]]

female.re <- rep(re.female,24)
age.re <- rep(kronecker(re.agegroup,c(1,1)), 6)
educ.re <- kronecker(re.educ,rep(1, 8))
ind.re <- rowSums(cbind(female.re, age.re, educ.re))
ind.re <- ind.re + fixef(model2)[1]
beta1 <- fixef(model2)[2]


#############################################################################
# Predictions for each ideal type
y.lat2 <- rep(NA,1248)
for (i in 1:26){
  a <- ((i-1)*48)+1
  b <- a + 47
  y.lat2[a:b] <- ind.re + beta1 * rightP[i] + re.canton[i]
}

p2 <- pnorm(y.lat2)

#############################################################################
# Post-Stratification Step -- weigh each ideal type by its frequency in the target population
dim(Censusobject)
a <- c()
for (i in 1:26){
  a <- c(a,Censusobject[,i])
}


# National estimate (is not improved)
sum(p2*a)/sum(a)

#############################################################################
# Let's turn to cantonal estimates
mrp.minaret2 <- rep(NA,26)
for (i in 1:26){
  a1 <- ((i-1)*48)+1
  a2 <- a1 + 47
  p2 <- pnorm(y.lat2[a1:a2])
  a <- Censusobject[,i]
  mrp.minaret2[i] <-  sum(p2*a)/sum(a)
}

# Cantonal estimates via disaggregation
TAB <- table(attributes(model2)$frame$cantonid,attributes(model2)$frame$Minaret)
dis.minaret <- TAB[,2]/rowSums(TAB)

# Cantonal estimates w/o L2 variables
mrp.minaret1 <- rep(NA,26)
for (i in 1:26){
  a1 <- ((i-1)*48)+1
  a2 <- a1 + 47
  p1 <- pnorm(y.lat1[a1:a2])
  a <- Censusobject[,i]
  mrp.minaret1[i] <-  sum(p1*a)/sum(a)
}

# Plot
plot(mrp.minaret2, MINARET, pch=20, cex=3, col=rgb(0,0,255,150,maxColorValue=255),
     bty="n", ylab="Official Ballot Result", xlab="Estimated Cantonal Support", ylim=c(.4,.72), xlim=c(.4,.72))
points(dis.minaret,MINARET, pch=18, col=rgb(0,0,255,60,maxColorValue = 255), cex=1.5)
points(mrp.minaret1,MINARET, col="blue", pch=21, cex=1.5,bg=rgb(255,255,255,100,maxColorValue = 255))
abline(c(0,1), lty=2, lwd=.5)
legend(.6,.45,legend = c("Disaggregation","MrP w/o L2", "MrP w/ L2"), pch=c(18,21,20), 
       col=c(rgb(0,0,255,60,maxColorValue = 255),"blue",
             rgb(0,0,255,200,maxColorValue = 255)), bty="n", pt.cex=c(1.5,1.5,3))
text(.68,.7,"45 Degree Line", cex=0.8)




########################################################################################
########################################################################################
#### MrP with Uncertainty

# Pseudo Bayesian Approach (see Gelman and Hill, 2007, chapter 7)
BLOCK <- sim(model2, n=1000)

re.female <- attributes(BLOCK)$ranef$female
re.agegroup <- attributes(BLOCK)$ranef$agegroup
re.educ <- attributes(BLOCK)$ranef$educ
re.canton <- attributes(BLOCK)$ranef$canton

# Generating ideal types, but 1,000 simulation for each ideal type, hence matrix 48*1000
# 2 * 4 * 6 (* 26)

female.re <- matrix(NA, 48,1000)
  for (i in 1:1000){
    female.re[,i] <- rep(re.female[i,,1],24)
  }
  
age.re <- matrix(NA, 48,1000)
  for (i in 1:1000){
    age.re[,i] <- rep(kronecker(re.agegroup[i,,1],c(1,1)), 6)
  }
  
educ.re <- matrix(NA, 48,1000)
for (i in 1:1000){
  educ.re[,i] <-   kronecker(re.educ[i,,1],rep(1, 8))
}

# generating sum of individual RE realizations
ind.re <- female.re + age.re + educ.re



y.lat2 <- matrix(NA,1248,1000)
for (i in 1:26){
  a <- ((i-1)*48)+1
  b <- a + 47
  level2 <- attributes(BLOCK)$fixef %*% matrix(c(1,rightP[i]),2,1) + re.canton[i]
#  level2 <- attributes(BLOCK)$fixef[1] +  attributes(BLOCK)$fixef[1] *rightP[i] + re.canton[i]
  level2.48 <- matrix(rep(level2,48),48,1000, byrow = TRUE)
  y.lat2[a:b,] <- ind.re + level2.48
}


# Cantonal estimates
mrp.minaret2.unc <- matrix(NA,26,1000)
for (i in 1:26){
  a1 <- ((i-1)*48)+1
  a2 <- a1 + 47
  p2 <- pnorm(y.lat2[a1:a2,])
  a <- Censusobject[,i]
  mrp.minaret2.unc[i,] <-  t(p2)%*%a/sum(a)
}

orderL <- order(MINARET)

plot(seq(.31,.8,length.out=26),1:26, bty="n", pch=26, yaxt="n",
     xlab="Support for Initiative", ylab="")

for (i in 1:26){
  points(mrp.minaret2[orderL][i],i, col="blue", pch=19, cex=2)
  if(i!=6&i!=24) points(MINARET[orderL][i],i, col="blue", pch=4, cex=2)
  if(i==6|i==24)  points(MINARET[orderL][i],i, col="red", pch=4, cex=2)
}

for (i in 1:26){
  draws <- mrp.minaret2.unc[orderL[i],]
  CI <- quantile(draws,c(0.025,0.975))
  segments(CI[1],i,CI[2],i, col="blue", lwd=0.5)
}

legend(.32,26,c("MrP Estimate", "Official Outcome (within CI)","Official Outcome (outside CI)"), 
       pch=c(19,4,4), col=c("blue","blue","red"), bty="n")


################################################################################################
################################################################################################
################################################################################################
#     MrsP - MrP's better half
#     Ban on export of armaments

## Subset to observations that are non-missing on vote outcome
data5 <- data1[-which(is.na(data1$KriegsmatVerbot)),]


# Sample mean estimate
table(data5$KriegsmatVerbot)/sum(table(data5$KriegsmatVerbot)) 
# true outcome: 31.8% Yes


################################################################################################
#### MRP
model1 <- glmer(KriegsmatVerbot ~ 1 + (1|female) + (1|agegroup) + (1|educ) + 
                  (1|canton), data= data5, family=binomial("probit"))
summary(model1)

re.female <- ranef(model1)$female[[1]]
re.agegroup <- ranef(model1)$agegroup[[1]]
re.educ <- ranef(model1)$educ[[1]]
re.canton <- ranef(model1)$canton[[1]]

female.re <- rep(re.female,24)
age.re <- rep(kronecker(re.agegroup,c(1,1)), 6)
educ.re <- kronecker(re.educ,rep(1, 8))
ind.re <- rowSums(cbind(female.re, age.re, educ.re))
ind.re <- ind.re + fixef(model1)


#############################################################################
# Predictions for each ideal type
y.lat1 <- rep(NA,1248)
for (i in 1:26){
  a <- ((i-1)*48)+1
  b <- a + 47
  y.lat1[a:b] <- ind.re + re.canton[i]
}

# predicted probabilities!
p1 <- pnorm(y.lat1)



#############################################################################
# Post-Stratification Step -- weigh each ideal type by its frequency in the target population
dim(Censusobject)
a <- c()
for (i in 1:26){
  a <- c(a,Censusobject[,i])
}

# Estimate
sum(p1*a)/sum(a)
# True Value 31.8% -- not bad!


#### Can we do better of we know whether someone was left or not?
# SP (3) and GPS (5)
data5$left <- 0
data5$left[data5$party==3|data5$party==5] <- 1


#### MRP
model2 <- glmer(KriegsmatVerbot ~ 1 + (1|female) + (1|agegroup) + (1|educ) + 
                  (1|left) + (1|canton), data= data5, family=binomial("probit"))
summary(model2)

re.female <- ranef(model2)$female[[1]]
re.agegroup <- ranef(model2)$agegroup[[1]]
re.educ <- ranef(model2)$educ[[1]]
re.canton <- ranef(model2)$canton[[1]]
re.left <- ranef(model2)$left[[1]]

# 2 * 4 * 6  * 2 * 26 
# adding left at the end of individual elements
female.re <- rep(rep(re.female,24),2)
age.re <- rep(rep(kronecker(re.agegroup,c(1,1)), 6),2)
educ.re <- rep(kronecker(re.educ,rep(1, 8)),2)
left.re <- c(rep(re.left[1],48),rep(re.left[2],48))
ind.re <- rowSums(cbind(female.re, age.re, educ.re, left.re))
ind.re <- ind.re + fixef(model2)



#############################################################################
# Predictions for each ideal type
y.lat2 <- rep(NA,2496) # 2*1248
for (i in 1:26){
  a <- ((i-1)*96)+1
  b <- a + 95
  y.lat2[a:b] <- ind.re + re.canton[i]
}

# predicted probabilities!
p2 <- pnorm(y.lat2)

#############################################################################
# Post-Stratification Step -- weigh each ideal type by its frequency in the target population
# -> how many left people per canton?
L2.left <- unique(cbind(data5$NR_left_2011,data5$cantonid))
L2.left
L2.left[6,1] <- 15
L2.left[15,1] <- 20

dim(Censusobject)
a <- c()
for (i in 1:26){
  slice <- Censusobject[,i]
  new.joint <- c(slice*(1-L2.left[i,1]/100), slice*(L2.left[i,1]/100))
  a <- c(a,new.joint)
}


###########################################################################
#Cantonal results?

# Cantonal estimates w/o MrsP
mrp.krieg1 <- rep(NA,26)
for (i in 1:26){
  a1 <- ((i-1)*48)+1
  a2 <- a1 + 47
  p1 <- pnorm(y.lat1[a1:a2])
  a <- Censusobject[,i]
  mrp.krieg1[i] <-  sum(p1*a)/sum(a)
}


# Cantonal estimates w/ MrsP
mrp.krieg2 <- rep(NA,26)
for (i in 1:26){
  a1 <- ((i-1)*96)+1
  a2 <- a1 + 95
  p2 <- pnorm(y.lat2[a1:a2])
  slice <- Censusobject[,i]
  new.joint <- c(slice*(1-L2.left[i,1]/100), slice*(L2.left[i,1]/100))
  mrp.krieg2[i] <-  sum(p2*new.joint)/sum(new.joint)
}

KRIEG <- c(33.90,
           29.70,
           25.60,
           15.60,
           20.20,
           19.10,
           12.00,
           21.90,
           25.40,
           29.00,
           26.20,
           46.90,
           34.20,
           30.00,
           28.40,
           23.10,
           26.50,
           32.10,
           26.50,
           21.60,
           37.60,
           40.50,
           29.20,
           37.20,
           48.20,
           38.90)/100


mse1 <- mean((KRIEG-mrp.krieg1)^2)
mse2 <- mean((KRIEG-mrp.krieg2)^2)


## Is MrsP MrP's better half?
mse1
mse2



#############################################################################
#############################################################################
# AutoMrP

#############################################################################
# create census file for auto_MrP function (see ??auto_mrp)
census_auto_mrp <- matrix(NA,1248,9)
female.re <- rep(c(0,1),24)
age.re <- rep(kronecker(c(1:4),c(1,1)), 6)
educ.re <- kronecker(c(1:6),rep(1, 8))

L2.block <- unique(cbind(data5$cantonnr,data5$urbanization,
             data5$bildungsausgabenproeinwohner,
             data5$NR_left_2011))


for (i in 1:26){
  a1 <- ((i-1)*48)+1
  a2 <- a1 + 47
  census_auto_mrp[a1:a2,1] <- i
  census_auto_mrp[a1:a2,2:4] <- cbind(female.re,age.re,educ.re)
  census_auto_mrp[a1:a2,6] <- Censusobject[,i]
  census_auto_mrp[a1:a2,5] <- census_auto_mrp[a1:a2,6]/sum(census_auto_mrp[a1:a2,6])
  census_auto_mrp[a1:a2,7:9] <- matrix(L2.block[i,-1],48,3, byrow = TRUE)
}
colnames(census_auto_mrp) <- c("cantonid", "female","agegroup","educ","proportion","freq",
                               "urbanization", "bildungsausgabenproeinwohner","NR_left_2011")

census_auto_mrp <- as.data.frame(census_auto_mrp)




model.automrp0 <- auto_MrP(y = "KriegsmatVerbot",
         L1.x = c("female","agegroup","educ"),
         L2.x = c("urbanization", "bildungsausgabenproeinwohner","NR_left_2011"),
         L2.unit = "cantonid",
         bin.proportion = "proportion",
         survey = data5,
         census = census_auto_mrp,
         best.subset = FALSE,
         lasso = FALSE,
         pca = FALSE,
         gb = FALSE,
         svm = FALSE,
         mrp = TRUE)

summary(model.automrp0)


model.automrp <- auto_MrP(y = "KriegsmatVerbot",
                           L1.x = c("female","agegroup","educ"),
                           L2.x = c("urbanization", "bildungsausgabenproeinwohner","NR_left_2011"),
                           L2.unit = "cantonid",
                           bin.proportion = "proportion",
                           survey = data5,
                           census = census_auto_mrp)


mse3 <- mean((KRIEG - c(model.automrp$ebma[,2])$ebma)^2)


####################################################################################
# Comparison of Family P models

# MrP
mse1
# MrsP
mse2
# autoMrP
mse3

