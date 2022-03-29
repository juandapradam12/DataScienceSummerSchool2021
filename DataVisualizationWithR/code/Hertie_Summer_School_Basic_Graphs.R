##############################################################################################
# Basic Graphs
###############################################################################################

getwd()
# Set Working Directory 
#setwd("~/home/juan-prada/Documentos/DataScienceSummerSchool2021/DataVisualizationWithR/code/data")

# Getting Data into R
library(foreign)
data <- read.csv("./data/gapminder.csv", sep=",")

# Look at Data Set
head(data)

# Look at Single Variables
class(data$lifeExp)
class(data$continent)

summary(data$lifeExp)
summary(data$continent)

table(data$continent)
prop.table(table(data$continent))

#########################################################################
# High Level Functions

# The Plot Function
afg <- data[data$country=="Afghanistan",]
plot(afg$lifeExp)

plot(afg$lifeExp, type="p") 
plot(afg$lifeExp, type="l")
plot(afg$lifeExp, type="b")
plot(afg$lifeExp, type="o")
plot(afg$lifeExp, type="h")
plot(afg$lifeExp, type="s")
plot(afg$lifeExp, type="n")


plot(data$continent)


# Other High Level Functions
my.tab <- prop.table(table(data$continent))
barplot(my.tab)

my.tab <- sort(my.tab, decreasing=T)
barplot(my.tab)

pie(my.tab)

dotchart(my.tab)

hist(data$lifeExp)
hist(data$lifeExp, breaks=100)
hist(data$lifeExp, breaks=50)
hist(data$lifeExp, breaks=5)

boxplot(data$lifeExp)


# Using 2 Variables

boxplot(data$lifeExp ~ data$continent)

data$gdpPercap <- data$gdpPercap/10000
plot(data$gdpPercap, data$lifeExp)

plot(data$continent, data$lifeExp)
plot(data$lifeExp, data$continent)

data$rich <- ifelse(data$gdpPerc > median(data$gdpPercap), "rich", "poor")
data$rich <- as.factor(data$rich)
my.tab <- prop.table(table(data$rich, data$continent), 2)
barplot(my.tab)

ord <- order(my.tab[2,])
my.tab <- my.tab[, ord]
barplot(my.tab)


##############################################################################################
# Some Arguments

# Some arguments are specific to certain high level functions
barplot(my.tab, horiz=T)
barplot(my.tab, beside=T)
barplot(my.tab, beside=T, horiz=T)
barplot(my.tab, beside=T, legend=T)
barplot(my.tab, beside=T, legend=T, border=F)

# Use Help to find more arguments
?barplot()


# Standard arguments work for most high level functions
# Graphical parameters
plot(data$gdpPercap, data$lifeExp, col="red") 
plot(data$gdpPercap, data$lifeExp,  pch=17)
plot(data$gdpPercap, data$lifeExp, pch=17, col="green")
plot(data$gdpPercap, data$lifeExp, pch=17, col="green", cex=3)


plot(data$gdpPercap, data$lifeExp, pch=17, col=rgb(39, 100, 25, max=2), cex=3)


plot(afg$year, afg$lifeExp, type="l", lty="dashed")
plot(afg$year, afg$lifeExp, type="l", lwd=3)
plot(afg$year, afg$lifeExp, type="l", lwd=3, lty="dashed")
plot(afg$year, afg$lifeExp, type="l", lwd=3, lty="dashed", col="purple")



# Text, Labels, and Axes
plot(afg$year, afg$lifeExp, type="l", lwd=2, col="red", main="Life Expectancy in Afghanistan is Increasing")
plot(afg$year, afg$lifeExp, type="l", lwd=2, col="red", main="Life Expectancy in Afghanistan is Increasing", xlab="Year", ylab="Life Expectancy at Birth (in Years)")
plot(afg$year, afg$lifeExp, type="l", lwd=2, col="red", main="Life Expectancy in Afghanistan is Increasing", xlab="Year", ylab="Life Expectancy at Birth (in Years)", ylim=c(25, 50), xlim=c(1950, 2010))

plot(afg$year, afg$lifeExp, type="l", lwd=2, col="red", 
     main="Life Expectancy in Afghanistan is Increasing", 
     xlab="Year", ylab="Life Expectancy at Birth (in Years)", 
     ylim=c(25, 50), xlim=c(1950, 2010),
     family="Helvetica Light")

plot(afg$year, afg$lifeExp, type="l", lwd=2, col="red", 
     main="Life Expectancy in Afghanistan is Increasing", 
     xlab="Year", ylab="Life Expectancy at Birth (in Years)", 
     ylim=c(25, 50), xlim=c(1950, 2010),
     family="Helvetica Light", font.main=1, font.lab=3)

plot(afg$year, afg$lifeExp, type="l", lwd=2, col="red", 
     main="Life Expectancy in Afghanistan is Increasing", 
     xlab="Year", ylab="Life Expectancy at Birth (in Years)", 
     ylim=c(25, 50), xlim=c(1950, 2010),
     family="Helvetica Light", font.main=1, font.lab=3, 
     cex.main=1.5, cex.lab=1, cex.axis=.8)


# Some more argumens with the par() function
par(mgp=c(2, .5, 0))
plot(afg$year, afg$lifeExp, type="l", lwd=2, col="red", 
     main="Life Expectancy in Afghanistan is Increasing", 
     xlab="Year", ylab="Life Expectancy at Birth (in Years)", 
     ylim=c(25, 50), xlim=c(1950, 2010),
     family="Helvetica Light", font.main=1, font.lab=3, 
     cex.main=1.5, cex.lab=1, cex.axis=.8)

par(mgp=c(2, .5, 0), las=1)
plot(afg$year, afg$lifeExp, type="l", lwd=2, col="red", 
     main="Life Expectancy in Afghanistan is Increasing", 
     xlab="Year", ylab="Life Expectancy at Birth (in Years)", 
     ylim=c(25, 50), xlim=c(1950, 2010),
     family="Helvetica Light", font.main=1, font.lab=3, 
     cex.main=1.5, cex.lab=1, cex.axis=.8)

par(mgp=c(2, .5, 0), las=1, tck=-0.005)
plot(afg$year, afg$lifeExp, type="l", lwd=2, col="red", 
     main="Life Expectancy in Afghanistan is Increasing", 
     xlab="Year", ylab="Life Expectancy at Birth (in Years)", 
     ylim=c(25, 50), xlim=c(1950, 2010),
     family="Helvetica Light", font.main=1, font.lab=3, 
     cex.main=1.5, cex.lab=1, cex.axis=.8)

par(mgp=c(2, .5, 0), las=1, tck=-0.005, bg="honeydew")
plot(afg$year, afg$lifeExp, type="l", lwd=2, col="red", 
     main="Life Expectancy in Afghanistan is Increasing", 
     xlab="Year", ylab="Life Expectancy at Birth (in Years)", 
     ylim=c(25, 50), xlim=c(1950, 2010),
     family="Helvetica Light", font.main=1, font.lab=3, 
     cex.main=1.5, cex.lab=1, cex.axis=.8)

par(mgp=c(2, .5, 0), las=1, tck=-0.005, fg="grey")
plot(afg$year, afg$lifeExp, type="l", lwd=2, col="red", 
     main="Life Expectancy in Afghanistan is Increasing", 
     xlab="Year", ylab="Life Expectancy at Birth (in Years)", 
     ylim=c(25, 50), xlim=c(1950, 2010),
     family="Helvetica Light", font.main=1, font.lab=3, 
     cex.main=1.5, cex.lab=1, cex.axis=.8)


##############################################################################################
# Some Low Level Functions

plot(afg$year, afg$lifeExp, type="l", lwd=2, col="red", 
     main="Life Expectancy in Afghanistan is Increasing", 
     xlab="Year", ylab="Life Expectancy at Birth (in Years)", 
     ylim=c(20, 80), xlim=c(1950, 2010),
     family="Helvetica Light", font.main=1, font.lab=3, 
     cex.main=1.5, cex.lab=1, cex.axis=.8)

grid(lty=1, lwd=.5)

normal.life <- quantile(data$lifeExp, c(.05, .95))
rect(1940, normal.life[1], 2020, normal.life[2], col="grey92", border=F)
text(1955, 82.5, "95% Quantile", col="grey92", pos=1, cex=.6)
text(1955, 40, "5% Quantile", col="grey92", pos=1, cex=.6)

lines(afg$year, afg$lifeExp, lwd=2, col="red")

average.life <- mean(data$lifeExp)
abline(h=average.life, lty=2, col="black")
text(1955, 60, "Global Average", col="black", pos=1, cex=.6)


iraq <- data[data$country=="Iraq",]
lines(iraq$year, iraq$lifeExp, lwd=2, col="orange")
text(iraq$year[12], iraq$lifeExp[12], "Iraq", pos=1, col="orange", cex=.6)
text(afg$year[12], afg$lifeExp[12], "Afghanistan", pos=1, col="red", cex=.6)


points(iraq$year[8], iraq$lifeExp[8], pch=21, cex=2, col="black")
text(iraq$year[8], iraq$lifeExp[8], "What happened here?", col="black", pos=4, cex=.6)


##############################################################################################
# Arranging Multiple Plots

par(mfrow=c(2, 3))

country.samp <- sample(unique(data$country), 6)

for(i in 1:6){
  
  plot(data$year[data$country==country.samp[i]], data$lifeExp[data$country==country.samp[i]], type="l", lwd=2, col="red", 
       main=country.samp[i], 
       xlab="Year", ylab="Life Expectancy", 
       ylim=c(20, 80), xlim=c(1950, 2010),
       family="Helvetica Light", font.main=1, font.lab=3, 
       cex.main=1.5, cex.lab=1, cex.axis=.8)
  
  grid(lty=1, lwd=.5)
  abline(h=average.life, lty=2, col="black")
  
}

means <- rep(NA, 6)
for(i in 1:6){
  means[i] <- mean(data$lifeExp[data$country==country.samp[i]])
}

ord <- order(means, decreasing=T)

for(i in ord){
  plot(data$year[data$country==country.samp[i]], data$lifeExp[data$country==country.samp[i]], type="l", lwd=2, col="red", 
       main=country.samp[i], 
       xlab="Year", ylab="Life Expectancy", 
       ylim=c(20, 80), xlim=c(1950, 2010),
       family="Helvetica Light", font.main=1, font.lab=3, 
       cex.main=1.5, cex.lab=1, cex.axis=.8)
  
  grid(lty=1, lwd=.5)
  abline(h=average.life, lty=2, col="black")
  
}


#######################################################################

