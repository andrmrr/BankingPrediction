# author: Nayara Costa
 

path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
df <- read.table("bank_after_cleaning.csv", header = T, sep = ",", stringsAsFactors = TRUE)


#Dimensions 

View(df) ### Veure la base de dades
class(df) ### Classe
dim(df) ### Dimensió
nrow(df) ### Nombre de files
ncol(df) ### Nombre de columnes
colnames(df) ### Nom de les columnes
rownames(df) ### Nom de les files
str(df) ### Conèixer l’estructura de la base de dades
summary(df)

#libraries
attach(df)
library(fBasics)
library(dplyr)
library(car)
library(ggplot2)
library(lessR)

#NUMERICAL columns
numeric_df <- df[sapply(df, is.numeric)]
numeric_df
basicStats(data.frame(age,balance,day,duration,campaign,pdays,previous))
par(mfrow=c(1,1))
hist(df$duration, main="Histogram of Duration", xlab="Duration (in seconds)", ylab="Frequency")
hist(df$campaign,  main="Histogram of Campaign", xlab="Campaign (number of contacts)", ylab="Frequency")
hist(df$age, main="Histogram of Age", xlab="Age", ylab="Frequency")
hist(df$previous, main="Histogram of previous", xlab="previous (Previously contacted)", ylab="Frequency")
hist(df$pdays, main="Histogram of pdays", xlab="pdays (Number of days after the client was contacted)", ylab="Frequency")
hist( df$balance, 10, main="Histogram Balance", col=rainbow(10), xlab='Balance' )

#VBS PLOT, identifying outliers (dots in read)
Boxplot(df$age)
Plot(age,data=df, xlab='Age')
Plot(balance, data=df, xlab='Balance')
Plot(balance, data=df, by1=education)
Plot(pdays, data=df)

scatter <- ggplot(df,aes(age,balance)) +
  geom_point() +
  ggtitle('Balance by age') +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_smooth(method='lm', formula = y ~ x, se=FALSE, col='red')
scatter

scatter <- ggplot(df,aes(duration, balance)) +
  geom_point() +
  ggtitle('Balance by duration') +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_smooth(method='lm', formula = y ~ x, se=FALSE, col='red')
scatter


#CORRELATION PLOTS
pairs(df[c('age','balance','duration')],pch=19,col='blue')
heatmap(cor(numeric_df), main="Correlation Heatmap", col=heat.colors(12))


#QUANTILES 
quantile( df$balance)
quantile( df$balance, prob= seq( 0, 1, by=0.1))

#DESCRIBING CATEGORICAL VARIABLES
count(job)
count(education)
count(contact)
count(marital)

#tendency for y
BarChart(y,data=df, by1=job, main='Tendency by job')
BarChart(y,data=df, by1=education, main='Tendency by education')
BarChart(y, data=df, by1=contact, main='Tendency by contact')
BarChart(y,data=df,by1=marital, main='Tendency by marial status')




#DESCRIPTIVA


descriptiva<-function(X, nom){
  if (!(is.numeric(X) || class(X)=="Date")){ 
    frecs<-table(as.factor(X), useNA="ifany")
    proportions<-frecs/n
    #ojo, decidir si calcular porcentages con o sin missing values
    pie(frecs, cex=0.6, main=paste("Pie of", nom))
    barplot(frecs, las=3, cex.names=0.7, main=paste("Barplot of", nom), col=listOfColors)
    print(paste("Number of modalities: ", length(frecs)))
    print("Frequency table")
    print(frecs)
    print("Relative frequency table (proportions)")
    print(proportions)
    print("Frequency table sorted")
    print(sort(frecs, decreasing=TRUE))
    print("Relative frequency table (proportions) sorted")
    print(sort(proportions, decreasing=TRUE))
  }else{
    if(class(X)=="Date"){
      print(summary(X))
      print(sd(X))
      #decide breaks: weeks, months, quarters...
      hist(X,breaks="weeks")
    }else{
      hist(X, main=paste("Histogram of", nom))
      boxplot(X, horizontal=TRUE, main=paste("Boxplot of",nom))
      print("Extended Summary Statistics")
      print(summary(X))
      print(paste("sd: ", sd(X, na.rm=TRUE)))
      print(paste("vc: ", sd(X, na.rm=TRUE)/mean(X, na.rm=TRUE)))
    }
  }
}

descriptiva(df$balance,'Balance')

