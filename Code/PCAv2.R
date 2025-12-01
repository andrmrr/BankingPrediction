## Set working path to this file's path (RStudio necessary)
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

## Load Bank information and display basic information
df <- read.csv("bank_after_cleaning.csv", header = T, row.names = 1, stringsAsFactors = TRUE)
df$month <- factor(df$month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))
head(df, 10)
summary(df)

## Variable divisions
column.classes = lapply(df, class)

# Division by class
categorical = column.classes == "factor"
categorical.names = names(df)[categorical]
print(categorical.names)

numerical = !categorical
numericals = sum(numerical)
numerical.names = names(df)[numerical]
print(numerical.names)
summary(df[numerical])

# Division by purpose
response.name = "y"
response = names(df) == response.name

explanatory = names(df) != response.name
explanatory.name = names(df)[explanatory]

## Show data without missing values
summary(df)

# PERFORM PCA ON NUMERICAL DATA

pca <- prcomp(df[numerical], scale = TRUE)

print(pca)

# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?
layout(matrix(1:2, nrow = 1))

inertia <- pca$sdev^2 
total.inertia <- sum(inertia)
inertia.perc <- 100 * inertia / total.inertia
barplot(inertia.perc, names.arg = paste('Dim', seq(numericals)), main = "Individual Data Percentages")

# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)

barplot(cumsum(inertia.perc), names.arg = paste(seq(numericals), 'dim(s)'), ylim=c(0, 100), main = "Cummulated Data Percentages")
lines(c(0, numericals + 2), rep(80, 2), col = 'red', lty = 2)

layout(1)

ndims = 5
Psi <- pca$x[, 1:ndims]

# PCA SCATTER PLOT SLICES

pca.scatter <- function(X, axis1, axis2, labels=NULL) {
  if(is.null(labels)) {
    plot(X[,axis1], X[,axis2], pch = 20,
         xlab = paste("dim", axis1), ylab = paste("dim", axis2))
  } else {
    plot(X[,axis1], X[,axis2], type = "n",
         xlab = paste("dim", axis1), ylab = paste("dim", axis2))
    text(X[,axis1], X[,axis2], labels = labels, cex=0.5)
  }
  axis(side=1, pos= 0, labels = F, col="cyan")
  axis(side=3, pos= 0, labels = F, col="cyan")
  axis(side=2, pos= 0, labels = F, col="cyan")
  axis(side=4, pos= 0, labels = F, col="cyan")
}

pca.scatter(Psi, 1, 2)
pca.scatter(Psi, 1, 3)
pca.scatter(Psi, 1, 4)
pca.scatter(Psi, 2, 3)
pca.scatter(Psi, 4, 5)

# PCA PROJECTIONS OF ORIGINAL VARIABLES

Phi = cor(df[numerical], Psi)

pca.dims <- function(X, axis1, axis2, labels, border = 1.1) {
  v.X <- X[,axis1]
  v.Y <- X[,axis2]
  plot(0, 0, type = "n",
       xlim = c(min(v.X, 0), max(v.X, 0)) * border,
       ylim = c(min(v.Y, 0), max(v.Y, 0)) * border,
       xlab = paste("dim", axis1), ylab = paste("dim", axis2))
  axis(side=1, pos= 0, labels = F, col="cyan")
  axis(side=3, pos= 0, labels = F, col="cyan")
  axis(side=2, pos= 0, labels = F, col="cyan")
  axis(side=4, pos= 0, labels = F, col="cyan")
  arrows(v.X * 0, v.Y * 0, v.X, v.Y, length = 0.07,col="blue")
  text(v.X * sqrt(border), v.Y * sqrt(border), labels = labels, col = "darkblue", cex = 0.7)
}

pca.dims(Phi, 1, 2, numerical.names)
pca.dims(Phi, 1, 3, numerical.names)
pca.dims(Phi, 4, 5, numerical.names)

 # #Plot two categories#

pca.scatter.category <- function(X, axis1, axis2, category) {
  plot(X[,axis1], X[,axis2], pch = 20,
       col = rainbow(length(levels(category)))[as.numeric(category)], type = "n",
       xlab = paste("dim", axis1), ylab = paste("dim", axis2))
  axis(side=1, pos= 0, labels = F, col="cyan")
  axis(side=3, pos= 0, labels = F, col="cyan")
  axis(side=2, pos= 0, labels = F, col="cyan")
  axis(side=4, pos= 0, labels = F, col="cyan")
}

pca.scatter.category(Psi, 1, 2, df$poutcome)

pca.centroid.categories <- function(X, axis1, axis2, categories, border = 1.1, scale = 1, quantitative = list()) {
  centroids = list()
  total <- length(categories[, 1])
  n <-  length(categories)
  i <- 1
  v.X <- c()
  v.Y <- c()
  for (category in names(categories)) {
    info <- list()
    info$name <- category
    info$text <- levels(categories[, category])
    info$n <- length(info$text)
    info$X <- tapply(X[, axis1], categories[, category], mean)
    info$Y <- tapply(X[, axis2], categories[, category], mean)
    info$S <- tapply(X[, axis2], categories[, category], length) * (scale * info$n / total)
    info$col <- rainbow(n)[i]
    centroids[[category]] <- info
    i <- i + 1
    v.X <- c(v.X, info$X)
    v.Y <- c(v.Y, info$Y)
  }
  plot(X[,axis1], X[,axis2], type = "n",
       col = rainbow(length(levels(category)))[as.numeric(category)],
       xlab = paste("dim", axis1), ylab = paste("dim", axis2),
       xlim = c(min(v.X, 0), max(v.X, 0)) * border,
       ylim = c(min(v.Y, 0), max(v.Y, 0)) * border)
  axis(side=1, pos= 0, labels = F, col="cyan")
  axis(side=3, pos= 0, labels = F, col="cyan")
  axis(side=2, pos= 0, labels = F, col="cyan")
  axis(side=4, pos= 0, labels = F, col="cyan")
  for (info in centroids) {
    points(info$X, info$Y, cex = info$S, col = info$col)
    text(info$X, info$Y, labels = info$text, cex = 0.7)
    if (!is.null(quantitative[[info$name]])) {
      print(quantitative[info$name])
      print(info$X)
      lines(info$X[quantitative[[info$name]]], info$Y[quantitative[[info$name]]], col = info$col)
    }
  }
  legend("bottomleft", pch = 1, legend = names(categories), col = rainbow(n))
}


pca.centroid.categories(Psi, 1, 2, df[categorical.names[1:5]], scale = 2)

quantitative = list()
quantitative$month <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec", "jan")

pca.centroid.categories(Psi, 1, 2, df[c("month", "y")], scale = 3, quantitative = quantitative)

pca.dims.centroids <- function(X, D, axis1, axis2, labels, categories, border = 1.1, scale = 1, quantitative = list()) {
  centroids = list()
  total <- length(categories[, 1])
  n <-  length(categories)
  i <- 1
  d.X <- D[,axis1]
  d.Y <- D[,axis2]
  v.X <- d.X
  v.Y <- d.Y
  for (category in names(categories)) {
    info <- list()
    info$name <- category
    info$text <- levels(categories[, category])
    info$n <- length(info$text)
    info$X <- tapply(X[, axis1], categories[, category], mean)
    info$Y <- tapply(X[, axis2], categories[, category], mean)
    info$S <- tapply(X[, axis2], categories[, category], length) * (scale * info$n / total)
    info$col <- rainbow(n)[i]
    centroids[[category]] <- info
    i <- i + 1
    v.X <- c(v.X, info$X)
    v.Y <- c(v.Y, info$Y)
  }
  plot(X[,axis1], X[,axis2], pch = 20, cex = 0.5, col = 5,
       xlim = c(min(v.X, 0), max(v.X, 0)) * border,
       ylim = c(min(v.Y, 0), max(v.Y, 0)) * border,
       xlab = paste("dim", axis1), ylab = paste("dim", axis2))
  axis(side=1, pos= 0, labels = F, col="cyan")
  axis(side=3, pos= 0, labels = F, col="cyan")
  axis(side=2, pos= 0, labels = F, col="cyan")
  axis(side=4, pos= 0, labels = F, col="cyan")
  arrows(d.X * 0, d.Y * 0, d.X, d.Y, length = 0.07,col="blue")
  text(d.X * sqrt(border), d.Y * sqrt(border), labels = labels, col = "darkblue", cex = 0.7)
  for (info in centroids) {
    points(info$X, info$Y, cex = info$S, col = info$col)
    text(info$X, info$Y, labels = info$text, cex = 0.7)
    if (!is.null(quantitative[[info$name]])) {
      print(quantitative[info$name])
      print(info$X)
      lines(info$X[quantitative[[info$name]]], info$Y[quantitative[[info$name]]], col = info$col)
    }
  }
  legend("bottomleft", pch = 1, legend = names(categories), col = rainbow(n))
}

pca.dims.centroids(Psi, Phi, 1, 2, numerical.names, df[categorical.names[9:10]], scale = 3)
pca.dims.centroids(Psi, Phi, 4, 5, numerical.names, df[categorical.names[1]], scale = 3)
print(categorical.names)

pca.complete.quantile <- function(X, D, axis1, axis2, labels, categories, quant = 0.01, scale = 1, quantitative = list()) {
  centroids = list()
  total <- length(categories[, 1])
  n <-  length(categories)
  i <- 1
  d.X <- D[,axis1]
  d.Y <- D[,axis2]
  v.X <- d.X
  v.Y <- d.Y
  for (category in names(categories)) {
    info <- list()
    info$name <- category
    info$text <- levels(categories[, category])
    info$n <- length(info$text)
    info$X <- tapply(X[, axis1], categories[, category], mean)
    info$Y <- tapply(X[, axis2], categories[, category], mean)
    info$S <- tapply(X[, axis2], categories[, category], length) * (scale * info$n / total)
    info$col <- rainbow(n)[i]
    centroids[[category]] <- info
    i <- i + 1
    v.X <- c(v.X, info$X)
    v.Y <- c(v.Y, info$Y)
  }
  print(quantile(X[,axis1], c(quant, 1 - quant)))
  print(quantile(X[,axis2], c(quant, 1 - quant)))
  plot(X[,axis1], X[,axis2], pch = 20, cex = 0.5, col = 8,
       xlim = quantile(X[,axis1], c(quant, 1 - quant)),
       ylim = quantile(X[,axis2], c(quant, 1 - quant)),
       xlab = paste("Dim", axis1), ylab = paste("Dim", axis2))
  axis(side=1, pos= 0, labels = F, col="cyan")
  axis(side=3, pos= 0, labels = F, col="cyan")
  axis(side=2, pos= 0, labels = F, col="cyan")
  axis(side=4, pos= 0, labels = F, col="cyan")
  arrows(d.X * 0, d.Y * 0, d.X, d.Y, length = 0.07,col="blue", width = 0.1)
  text(d.X * 1.1, d.Y * 1.1, labels = labels, col = "darkblue", cex = 0.8, font = 2)
  for (info in centroids) {
    points(info$X, info$Y, cex = info$S, col = info$col, lwd = 2)
    text(info$X, info$Y, labels = info$text, cex = 0.8, font = 2)
    if (!is.null(quantitative[[info$name]])) {
      print(quantitative[info$name])
      print(info$X)
      lines(info$X[quantitative[[info$name]]], info$Y[quantitative[[info$name]]], col = info$col)
    }
  }
  legend("bottomleft", pch = 1, legend = names(categories), col = rainbow(n))
}

category_indexes <- c(3, 10)
pca.complete.quantile(Psi, Phi, 4, 5, numerical.names, df[categorical.names[category_indexes]], scale = 3)
pca.complete.quantile(Psi, Phi, 1, 3, numerical.names, df[categorical.names[category_indexes]], scale = 3)
print(categorical.names)
