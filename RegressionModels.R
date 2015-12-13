setwd("~/Dropbox/Datascience/Universite_Johns-Hopkins/Modèles_de_régression/RegressionModels")

#-------------------------------------------------------------------------------
# Week 1
#-------------------------------------------------------------------------------

library(UsingR) # contient datasets...type galton
data(galton) 

## Analyse exploratoire de donn??es
summary(galton)
boxplot(galton)

par(mfrow=c(1,2)) 
hist(galton$child,col="blue",breaks=100) 
hist(galton$parent,col="blue",breaks=100)



par(mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1)) #  initialiser
with(galton, plot(parent,child))
with(galton, plot(jitter(parent, factor=3),jitter(child, factor =3)))


library(reshape)
long <- melt(galton)
str(long)
## transforme un tableau de plusieurs colonnes
## en un dataframe avec variable variable de type factor correspondant aux colunnes initiales 
## et value valeur... 

boxplot(value ~ variable, data = long , col = "red") # variable: categorical

g <- ggplot(long, aes(x=value, fill = variable))
g <- g + geom_histogram(colour = "black", binwidth=1)
g <- g+ facet_grid(. ~ variable)
g


library(manipulate) 
myHist<-function(mu){
        hist(galton$child,col="blue") 
        lines(c(mu,mu),c(0,150),col="red",lwd=5) 
        mse<-mean((galton$child-mu)^2) 
        text(63,150,paste("mu=",mu)) 
        text(63,140,paste("MSE=",round(mse,2)))
}
manipulate(myHist(mu),mu=slider(62,74,step=0.5))

# Scatter plot en utilisant ggplot 
ggplot(galton, aes(x=parent, y=child)) + geom_point()

# Size of point represents number of points at that (X, Y) combination 
# (See the Rmd file for the code).
library(dplyr)
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
g


y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
myPlot <- function(beta){
        g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
        g <- g  + scale_size(range = c(2, 20), guide = "none" )
        g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
        g <- g + geom_point(aes(colour=freq, size = freq))
        g <- g + scale_colour_gradient(low = "lightblue", high="white")                     
        g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
        mse <- mean( (y - beta * x) ^2 )
        g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
        g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))

# 1 pour dire origin, ...
lm(I(child-mean(child))~I(parent-mean(parent))-1,data=galton)


y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) *  sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
rbind(c(beta0, beta1), coef(lm(y ~ x)))
