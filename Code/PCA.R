## Set working path to this file's path (RStudio necessary)
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

## Load Bank information and display basic information
df <- read.csv("bank_after_cleaning.csv", header = T, row.names = 1, stringsAsFactors = TRUE)
head(df, 10)
summary(df)

## Variable divisions
column.classes = lapply(df, class)

# Division by class
categorical = column.classes == "factor"
categorical.names = names(df)[categorical]

numerical = !categorical
numericals = sum(numerical)
numerical.names = names(df)[numerical]

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

inertia <- pca$sdev^2 
total.inertia <- sum(inertia)
inertia.perc <- 100 * inertia / total.inertia
barplot(inertia.perc, names.arg = paste('dim', seq(numericals)))

# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)

barplot(cumsum(inertia.perc), names.arg = paste(seq(numericals), 'dim(s)'), ylim=c(0, 100))
lines(c(0, numericals + 2), rep(80, 2), col = 'red', lty = 2)

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

par(mfrow=c(2, 2))
pca.scatter(Psi, 1, 2)
pca.scatter(Psi, 1, 3)
pca.scatter(Psi, 2, 4)
pca.scatter(Psi, 2, 5)
par(mfrow=c(1, 1))

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


par(mfrow=c(2, 2))
pca.dims(Phi, 1, 2, numerical.names)
pca.dims(Phi, 1, 3, numerical.names)
pca.dims(Phi, 2, 4,numerical.names)
pca.dims(Phi, 2, 5, numerical.names)
par(mfrow=c(1, 1))

# PCA CATEGORIES DISTRIBUTION

pca.dims.centroids <- function(X, D, axis1, axis2, labels, categories, border = 1.1, scale = c(1, 0.3), quantitative = list()) {
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
    info$S <- tapply(X[, axis2], categories[, category], length) * (scale[1] * info$n / total) + scale[2]
    info$col <- paste(rainbow(n), "7F", sep = '')[i]
    centroids[[category]] <- info
    i <- i + 1
    v.X <- c(v.X, info$X)
    v.Y <- c(v.Y, info$Y)
  }
  plot(X[,axis1], X[,axis2], type = "n",
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
    points(info$X, info$Y, cex = info$S, col = info$col, pch = 16)
    if (!is.null(quantitative[[info$name]])) {
      print(quantitative[info$name])
      print(info$X)
      lines(info$X[quantitative[[info$name]]], info$Y[quantitative[[info$name]]], col = info$col)
    }
  }
  legend("bottomleft", pch = 16, legend = names(categories), col = rainbow(n))
}

pca.dims.centroids(Psi, Phi, 1, 2, numerical.names, df[categorical.names])
pca.dims.centroids(Psi, Phi, 3, 4, numerical.names, df[categorical.names])

# CATEGORY SCATTER PLOT

pca.scatter.category <- function(X, D, axis1, axis2, labels, category, quant = 0.01, scale = c(10, 1)) {
  d.X <- D[,axis1]
  d.Y <- D[,axis2]
  cat.names <- levels(category)
  cat.n <- length(cat.names)
  plot(X[,axis1], X[,axis2], pch = 20,
       col = paste(rainbow(cat.n)[as.numeric(category)],
                   "7F", sep = ''),
       xlim = quantile(X[,axis1], c(quant, 1 - quant)),
       ylim = quantile(X[,axis2], c(quant, 1 - quant)),
       xlab = paste("dim", axis1), ylab = paste("dim", axis2))
  axis(side=1, pos= 0, labels = F, col="cyan")
  axis(side=3, pos= 0, labels = F, col="cyan")
  axis(side=2, pos= 0, labels = F, col="cyan")
  axis(side=4, pos= 0, labels = F, col="cyan")
  arrows(d.X * 0, d.Y * 0, d.X, d.Y, length = 0.07,col="blue")
  text(d.X * 1.1, d.Y * 1.1, labels = labels, col = "darkblue", cex = 0.7)
  loc.X <- tapply(X[, axis1], category, mean)
  loc.Y <- tapply(X[, axis2], category, mean)
  loc.S <- tapply(X[, axis2], category, length) *
    (scale[1] * cat.n / length(X)) + scale[2]
  points(loc.X, loc.Y, cex = loc.S,
         col = paste(rainbow(cat.n), "7F", sep = ""), pch = 16)
  text(loc.X, loc.Y, labels = cat.names, cex = 0.7)
  legend("bottomleft", pch = 16, legend = cat.names, col = rainbow(cat.n))
}

pca.scatter.category(Psi, Phi, 2, 3, numerical.names, df$education, quant = 0.05)
pca.scatter.category(Psi, Phi, 2, 3, numerical.names, df$job, quant = 0.05)
pca.scatter.category(Psi, Phi, 1, 4, numerical.names, df$month)
pca.scatter.category(Psi, Phi, 2, 3, numerical.names, df$marital)
pca.scatter.category(Psi, Phi, 1, 4, numerical.names, df$poutcome)
pca.scatter.category(Psi, Phi, 1, 2, numerical.names, df$y)
pca.scatter.category(Psi, Phi, 3, 4, numerical.names, df$y)
