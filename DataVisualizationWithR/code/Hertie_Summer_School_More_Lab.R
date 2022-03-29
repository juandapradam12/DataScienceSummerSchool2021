#######################################################################
# Building a Few-Style Bullet Graph

# Start with on bar
measure <- 7
standard <- 6.8

par(mar=c(3,8,2,2), oma=c(10, 1, 5, 10), yaxs="i")
plot(0, 0, xlim=c(0, 10), ylim=c(.45, .55), pch="",  ylab="", xlab="", axes=F)
axis(1)
rect(0, .45, 10, .55, col="#fbb4ae", border="#fbb4ae")
rect(6, .45, 10, .55, col="#fff2ae", border="#fff2ae")
rect(8, .45, 10, .55, col="#ccebc5", border="#ccebc5")
segments(0, .5, measure, .5, lwd=10, lend=1)
points(standard, .5, pch="|", col="red", cex=2.5, lwd=10)
axis(2, at=.5, label="Some Variable", col="white")


# Multiple bars
dat <- c(27, 34, 22,  7)

par(mar=c(3,8,2,3), oma=c(4, 1, 4, 1), yaxs="i")
p <- barplot(dat, horiz=T, space=4, border=F, col="black", ylim=c(0, 22), xlim=c(0, 40), axes=F)

for(i in 1:4){
  rect(0, p[i]-1, 35, p[i]+1, col="#ccebc5", border="#ccebc5")
  rect(0, p[i]-1, 30, p[i]+1, col="#fff2ae", border="#fff2ae")
  rect(0, p[i]-1, 25, p[i]+1, col="#fbb4ae", border="#fbb4ae")
}

barplot(dat, horiz=T, space=4, border="black", col="black", ylim=c(0, 22),  axes=F, add=T, names.arg=c("A", "B", "C", "D"))
points(c(10, 13, 8, 30), p, pch="|", cex=2, lwd=8, col="red")
axis(1, at=seq(0, 35, 5), label=seq(0, 35, 5))


#############################################################################
# Table Plots
# Tennekes, M., Jonge, E. de, Daas, P.J.H. (2013) Visualizing and Inspecting Large Datasets with Tableplots, Journal of Data Science 11 (1), 43-58.

# library(devtools)
# install_github("mtennekes/tabplot")

library(tabplot)
data <- read.csv("swiss_census.csv", sep=",")

tableplot(data)

tableplot(data, sortCol=AGE_HARM) # sort by age
tableplot(data, sortCol=RELIGIOUSCOMMAGGII_HARM, from=0, to=100, nBins=100) # sort by religion
tableplot(data, sortCol=RELIGIOUSCOMMAGGII_HARM, from=0, to=100, nBins=10) # smooth
tableplot(data, sortCol=RELIGIOUSCOMMAGGII_HARM, from=0, to=100, nBins=300) # more detail
tableplot(data, sortCol=RELIGIOUSCOMMAGGII_HARM, from=54, to=62, nBins=300) # zoom in
tableplot(data, sortCol=CURRACTIVITYSTATUSIII_HARM, subset=RELIGIOUSCOMMAGGII_HARM=="Jewish", nBins=300) # filter

#############################################################################
# Joy Plot Map

setwd("~/Downloads/gpw-v4-admin-unit-center-points-population-estimates-rev11_deu_csv")
dat <- read.csv("gpw_v4_admin_unit_center_points_population_estimates_rev11_deu.csv")

# Start with map
library(maps)
map()

map(xlim=c(5.924, 14.988), ylim=c(47.37 , 55.02), col="lightgrey")

axis(1)
axis(2)

# Add lines
for(i in seq(47, 55, by=.1)){
  
  ord <- order(dat$INSIDE_X[dat$INSIDE_Y>i & dat$INSIDE_Y<=i+.1])
  lines(dat$INSIDE_X[dat$INSIDE_Y>i & dat$INSIDE_Y<=i+.1][ord], dat$UN_2020_E[dat$INSIDE_Y>i & dat$INSIDE_Y<=i+.1][ord]/3000000+i+.05)
  
}

# Start with empty plot
plot(0,0, pch="", xlim=c(5.924, 14.988), ylim=c(47.37 , 60), axes=F, ann=F)
for(i in seq(47, 55, by=.1)){
  
  ord <- order(dat$INSIDE_X[dat$INSIDE_Y>i & dat$INSIDE_Y<=i+.1])
  lines(dat$INSIDE_X[dat$INSIDE_Y>i & dat$INSIDE_Y<=i+.1][ord], dat$UN_2020_E[dat$INSIDE_Y>i & dat$INSIDE_Y<=i+.1][ord]/500000+i+.05, col=rgb(0,0,0,.5))
  
}

# Fix design
par(bg="black")
plot(0,0, pch="", xlim=c(5.924, 14.988), ylim=c(47.37 , 60), axes=F, ann=F)
for(i in seq(47, 55, by=.1)){
  
  ord <- order(dat$INSIDE_X[dat$INSIDE_Y>i & dat$INSIDE_Y<=i+.1])
  lines(dat$INSIDE_X[dat$INSIDE_Y>i & dat$INSIDE_Y<=i+.1][ord], dat$UN_2020_E[dat$INSIDE_Y>i & dat$INSIDE_Y<=i+.1][ord]/500000+i+.05, col=rgb(1,1,1,.5))
  polygon(dat$INSIDE_X[dat$INSIDE_Y>i & dat$INSIDE_Y<=i+.1][ord], dat$UN_2020_E[dat$INSIDE_Y>i & dat$INSIDE_Y<=i+.1][ord]/500000+i+.05, col="red")
}

# Fix design 2
par(bg="black")
plot(0,0, pch="", xlim=c(5.924, 14.988), ylim=c(47.37 , 60), axes=F, ann=F)
for(i in rev(seq(47, 55, by=.1))){
  
  ord <- order(dat$INSIDE_X[dat$INSIDE_Y>i & dat$INSIDE_Y<=i+.1])
  polygon(dat$INSIDE_X[dat$INSIDE_Y>i & dat$INSIDE_Y<=i+.1][ord], dat$UN_2020_E[dat$INSIDE_Y>i & dat$INSIDE_Y<=i+.1][ord]/500000+i+.05, col="black", border="white")
  #lines(dat$INSIDE_X[dat$INSIDE_Y>i & dat$INSIDE_Y<=i+.1][ord], dat$UN_2020_E[dat$INSIDE_Y>i & dat$INSIDE_Y<=i+.1][ord]/500000+i+.05, col=rgb(1,1,1,.5))
  
}

#########################################################################
# Visualizing Statistical Models

library(arm)
library(effects)

par(family="Gill Sans", lwd=.5, bty="l", mgp=c(1.5, .5, 0), mar=c(3,3,2,1), las=1, tck=-.02, cex.axis=.8, cex.lab=.8)


data(Arrests) # Load Data
data <- Arrests 

# Recode variables
data$released <- ifelse(data$released=="Yes", 1, 0) # Recode outcome variable
data$black <- ifelse(data$colour=="Black", 1, 0) # 
data$age.10 <- data$age/10 


###################################################################################
# Logistic Regression Model 

m <- glm(released ~  checks, data=data, family=binomial(link = "logit"))
display(m, digits=3)


# Plot Effect
par(mfrow=c(1,1), xaxs="r", yaxs="r", tck=-.02)
plot(jitter(data$checks, .5), jitter(data$released, .5), ylab="Pr(Released)", xlab="Number of Previous Records", cex=.5, pch=19, ylim=c(0, 1.1), col=rgb(0, 0, 0, 50, max=255))
abline(h=c(0, 1), lty=2, lwd=.5)
curve(invlogit(cbind(1, x)%*%coef(m)), col="maroon3", lwd=2, add=T)

# Add more covariates
m.2 <- glm(released ~  checks + black + employed + age.10, data=data, family=binomial(link = "logit"), x=T)
display(m.2, digits=3)

# Coefficient Plot
par(mar=c(3,5,3,2))
coefplot(m.2, main="Logit Coefficients for Released", col.main="black", cex.main=.8, xlim=c(-1, 1.5))


# Coefficient Plot per hand
coef.vec <- coef(m.2)   # Extract coefficient estimates
names.vec <- names(coef(m.2)) # Extract variable names
se.vec <- se.coef(m.2) # Extract standard errors

plot(coef.vec, length(coef.vec):1, pch=17, cex=.8, axes=F, ylab="", xlab="Logit coefficients", xlim=c(-1, 2))
axis(1)
axis(2, at=length(coef.vec):1, label=names.vec, col="white")
abline(v=0, lty=2) # add zero reference line

segments(coef.vec - 1.96*se.vec,  length(coef.vec):1, coef.vec + 1.96*se.vec, length(coef.vec):1) # add confidence intervals


##################################################################################
# Predicted Probabilities
# Comparing Black vs. White (for employed=1 and average checks and average age)

pp.1 <- invlogit(cbind(1, mean(data$checks), 1, 1, mean(data$age.10)) %*% coef(m.2))
pp.2 <- invlogit(cbind(1, mean(data$checks), 0, 1, mean(data$age.)) %*% coef(m.2))
diff <- pp.1 - pp.2

# Plot
par(mfrow=c(1,2), mar=c(3, 3, 2, 0))
plot(c(1:2), c(pp.1, pp.2),  pch=c(19, 21), ylab="Predicted Probability", xlab="", xlim=c(.5, 2.5), ylim=c(0, 1), axes=F)
axis(1, at=c(1:2), label=c("Black", "White"), col="white")
axis(2)

par(mar=c(3, 0, 2, 12))
plot(1, diff,  pch=c(17), ann=F,  xlim=c(.9, 1.1), ylim=c(-.2, .2), axes=F, col="maroon3")
rect(-2, -2, 2, 2, border=F, col="grey92")
axis(1, at=1, label=c("Difference"), col.axis="Maroon3", col="white")
axis(4) 
abline(h=0, lty=2)
points(1, diff,  pch=c(17), col="Maroon3")

# Let's add inferential uncertainty!
s <- 1000 # number of simulations
s.m.2 <- sim(m.2, s) # simulate the model s times

# Again, caculate predicted probabilities
s.pp.1 <- invlogit(cbind(1, mean(data$checks), 1, 1, mean(data$age.10)) %*% t(coef(s.m.2)))
s.pp.2 <- invlogit(cbind(1, mean(data$checks), 0, 1, mean(data$age.10)) %*% t(coef(s.m.2)))
s.diff <- s.pp.1 - s.pp.2


# Let's first look at the uncertainties in terms of 95 % confidence intervals
ci.1 <- quantile(s.pp.1, c(.025, .975))
ci.2 <- quantile(s.pp.2, c(.025, .975))
ci.diff <- quantile(s.diff, c(.025, .975))

# now let's add them to the plot
par(mfrow=c(1,2), mar=c(3, 3, 2, 0))
plot(c(1:2), c(pp.1, pp.2),  pch=c(19, 21), ylab="Predicted Probability", xlab="", xlim=c(.5, 2.5), ylim=c(.7, 1), axes=F)
axis(1, at=c(1:2), label=c("Black", "White"), col="white")
axis(2)

segments(c(1:2), c(ci.1[1], ci.2[1]), c(1:2), c(ci.1[2], ci.2[2])) # Add 95% Confidence Intervals 

par(mar=c(3, 0, 2, 12))
plot(1, diff,  pch=c(17), ann=F,  xlim=c(.9, 1.1), ylim=c(-.1, .05), axes=F, col="maroon3")
rect(-2, -2, 2, 2, border=F, col="grey92")
axis(1, at=1, label=c("Difference"), col.axis="Maroon3", col="white")
axis(4) 
abline(h=0, lty=2)
points(1, diff,  pch=c(17), col="Maroon3")

segments(1, ci.diff[1], 1, ci.diff[2], col="Maroon3") # Add 95% Confidence Intervals


##################################################################################
# Interactions

m.3 <- glm(released ~  checks + black + employed + age.10 + black:age.10, data=data, family=binomial(link = "logit"), x=T)
display(m.3, digits=3)
coefplot(m.3)

# Plot 
par(mfrow=c(1,1), mar=c(3,3,2,3))
plot(data$age.10, data$released, pch="", ylab="Pr(Released)", xlab="Age/10", cex=.4,  ylim=c(.6, 1), xlim=c(0, 7))
curve(invlogit(cbind(1, mean(data$checks), 1, 1, x, 1*x) %*% coef(m.3)), col="maroon3", lwd=2, add=T)
curve(invlogit(cbind(1, mean(data$checks), 0, 1, x, 0*x) %*% coef(m.3)), col="darkolivegreen2", lwd=2, add=T)

text(0, .7, "Black", col="maroon3",  pos=4)
text(0, .95, "White", col="darkolivegreen2",  pos=4)


## Curves with Simulations for Inferential Uncertainty Added to the Plot
s <- 1000
s.m.3 <- sim(m.3, s)

for(i in 1:s){
  curve(invlogit(cbind(1, mean(data$checks), 1, 1, x, 1*x) %*% coef(s.m.3)[i,]), col=rgb(205, 41, 144, 30, max=255), add=T)
}

for(i in 1:s){
  curve(invlogit(cbind(1, mean(data$checks), 0, 1, x, 0*x) %*% coef(s.m.3)[i,]), col=rgb(188, 238, 104, 30, max=255), add=T)
}

rug(data$age.10, side=1, col="grey")
curve(invlogit(cbind(1, mean(data$checks), 1, 1, x, 1*x) %*% coef(m.3)), add=T, col="white", lwd=2)
curve(invlogit(cbind(1, mean(data$checks), 0, 1, x, 0*x) %*% coef(m.3)), add=T, col="white", lwd=2)



###################################################################################
# Alternative Visualization for Confidence Intervals

x <- seq(1, 7, by=.1) # Vary age/10 from 1 to 7 in steps of .1

mean.vec.1 <- matrix(NA, length(x), 1) # set up empty containers
CI.mat.1 <- matrix(NA, length(x), 2)

for(i in 1:length(x)){
  mean.vec.1[i,] <- mean(invlogit(c(1, mean(data$checks), 1, 1, x[i], 1*x[i]) %*% t(coef(s.m.3))))
  CI.mat.1[i,] <- quantile(invlogit(c(1, mean(data$checks), 1, 1, x[i], 1*x[i]) %*% t(coef(s.m.3))), c(.025, .975))
}

mean.vec.2 <- matrix(NA, length(x), 1)
CI.mat.2 <- matrix(NA, length(x), 2)

for(i in 1:length(x)){
  mean.vec.2[i,] <- mean(invlogit(c(1, mean(data$checks), 0, 1, x[i], 0*x[i]) %*% t(coef(s.m.3))))
  CI.mat.2[i,] <- quantile(invlogit(c(1, mean(data$checks), 0, 1, x[i], 0*x[i]) %*% t(coef(s.m.3))), c(.05, .95))
}

plot(c(1:length(x)), CI.mat.1[,1], ylim=c(.7, 1), type="l", col="maroon3", xlab="Age", ylab="Pr(Released)", axes=F)
axis(1, at=c(1, 11, 21, 31, 41, 51, 61), label=c(10, 20, 30, 40, 50, 60, 70))
axis(2, col="white")   
lines(c(1:length(x)), CI.mat.1[,2], col="maroon3")
polygon(c(1:length(x), length(x):1), c(CI.mat.1[,2], rev(CI.mat.1[,1])), col=rgb(205, 41, 144, 50, max=255), border=F) 
lines(c(1:length(x)), mean.vec.1, col="maroon3")


lines(c(1:length(x)), CI.mat.2[,1], col="darkolivegreen2")
lines(c(1:length(x)), CI.mat.2[,2], col="darkolivegreen2")
polygon(c(1:length(x), length(x):1), c(CI.mat.2[,2], rev(CI.mat.2[,1])), col=rgb(188, 238, 104, 100, max=255), border=F)
lines(c(1:length(x)), mean.vec.2, col="darkolivegreen2")


axis(4, at=mean.vec.1[61], label="Black", col.axis="maroon3", col.tick="white", cex.axis=.8)
axis(4, at=mean.vec.2[61], label="White", col.axis="darkolivegreen2", col.tick="white", cex.axis=.8)

