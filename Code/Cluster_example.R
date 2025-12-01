## Set working path to this file's path (RStudio necessary)
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

## Load Bank information and display basic information
dd <- read.csv("bank_after_cleaning.csv", header = T, row.names = 1, stringsAsFactors = TRUE)
dd$month <- factor(dd$month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

## Variable divisions
column.classes = lapply(dd, class)

# Division by class
categorical = column.classes == "factor"
categorical.names = names(dd)[categorical]

numerical = !categorical
numerical.names = names(dd)[numerical]

# Division by purpose
response.name = "y"
response = names(dd) == response.name

explanatory = names(dd) != response.name
explanatory.name = names(dd)[explanatory]

names(dd)
dim(dd)
summary(dd)

attach(dd)

#set a list of numerical variables
names(dd)

dcon <- dd[numerical.names]
dim(dcon)

#
# CLUSTERING
#



# KMEANS RUN, BUT HOW MANY CLASSES?

k1 <- kmeans(dcon,5)
names(dcon)
print(k1)

attributes(k1)

k1$size

k1$withinss

k1$centers

# LETS COMPUTE THE DECOMPOSITION OF INERTIA

Bss <- sum(rowSums(k1$centers^2)*k1$size)
Bss
Wss <- sum(k1$withinss)
Wss
Tss <- k1$totss
Tss

Bss+Wss

Ib1 <- 100*Bss/(Bss+Wss)
Ib1

# LETS REPEAT THE KMEANS RUN WITH K=5

k2 <- kmeans(dcon,5)
k2$size

Bss <- sum(rowSums(k2$centers^2)*k2$size)
Bss
Wss <- sum(k2$withinss)
Wss

Ib2 <- 100*Bss/(Bss+Wss)
Ib2
Ib1

k2$centers
k1$centers

plot(k1$centers[,3],k1$centers[,2])

table(k1$cluster, k2$cluster)

# WHY WE HAVE OBTAINED DIFFERENT RESULTS?, AND WHICH RUN IS BETTER?

# NOW TRY K=8

k3 <- kmeans(dcon,8)
k3$size

Bss <- sum(rowSums(k3$centers^2)*k3$size)
Wss <- sum(k3$withinss)

Ib3 <- 100*Bss/(Bss+Wss)
Ib3


# HIERARCHICAL CLUSTERING

d  <- dist(dcon[1:50,])
h1 <- hclust(d,method="ward.D")  # NOTICE THE COST
plot(h1)

d  <- dist(dcon[sample(nrow(dcon), 50),])
h1 <- hclust(d,method="ward.D")  # NOTICE THE COST
plot(h1)

d  <- dist(dcon)
h1 <- hclust(d,method="ward.D")  # NOTICE THE COST
plot(h1)

# BUT WE ONLY NEED WHERE THERE ARE THE LEAPS OF THE HEIGHT

# WHERE ARE THER THE LEAPS? WHERE WILL YOU CUT THE DENDREOGRAM?, HOW MANY CLASSES WILL YOU OBTAIN?

nc = 3

c1 <- cutree(h1,nc)

c1[1:20]

nc = 5

c5 <- cutree(h1,nc)

c5[1:20]


table(c1)
table(c5)
table(c1,c5)


cdg <- aggregate(as.data.frame(dcon),list(c1),mean)
cdg

plot(cdg[,1], cdg[,7])

# LETS SEE THE PARTITION VISUALLY


plot(balance,duration,col=c1,pch=c1+2, lwd=2,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3))

plot(age,previous,col=c1,pch=c1+2, lwd=2,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3), cex=0.6)

plot(campaign,day,col=c1,pch=c1+2, lwd=2,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3), cex=0.6)

pairs(dcon[,], col=c1)

#plot(FI[,1],FI[,2],col=c1,main="Clustering of credit data in 3 classes")
#legend("topleft",c("c1","c2","c3"),pch=1,col=c(1:3))

# LETS SEE THE QUALITY OF THE HIERARCHICAL PARTITION



Bss <- sum(rowSums(cdg^2)*as.numeric(table(c1)))

Ib4 <- 100*Bss/Tss
Ib4


#move to Gower mixed distance to deal 
#simoultaneously with numerical and qualitative data

library(cluster)

#dissimilarity matrix

actives<-c(which(categorical & explanatory))
dissimMatrix <- daisy(dd[,actives], metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2

h1 <- hclust(distMatrix,method="ward.D")  # NOTICE THE COST
#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot

plot(h1)

c2 <- cutree(h1,5)

#class sizes 
table(c2)

#comparing with other partitions
table(c1,c2)


names(dd)
#balance
boxplot(dd$balance~c2, horizontal=TRUE)

#duration
boxplot(dd$duration~c2, horizontal=TRUE)

#age
boxplot(dd$age~c2, horizontal=TRUE)

pairs(dcon[,], col=c2)

plot(balance,age,col=c2,main="Clustering of credit data in 5 classes")
legend("topright",paste("class", sort(unique(c2))),pch=1,col=c(1:5), cex=0.6)

cdg <- aggregate(as.data.frame(dcon),list(c2),mean)
cdg

plot(age, balance, col= c2)
legend("topright",paste("class", sort(unique(c2))),pch=1,col=c(1:length(unique(c2))), cex=0.6)
points(cdg[,2],cdg[,3],pch=16,col="orange")
text(cdg[,2],cdg[,3], labels=cdg[,1], pos=2, font=2, cex=0.7, col="orange")

potencials<-c(3,4,6,7,10,11)
pairs(dcon[,potencials],col=c2)

#Profiling plots
