## Set working path to this file's path (RStudio necessary)
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

# Load libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(FactoMineR)
library(factoextra)
library(ggpubr)

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

# PLOTS

pca <- prcomp(df[numerical], scale = TRUE)
ndims = 5
num.samples <- pca$x[, 1:ndims]
num.coords <- data.frame(num.samples)
num.axis <- cor(df[numerical], num.samples)
num.pca <- data.frame(num.axis)
num.pca$name <- "PCA axis"
num.levels <- dimnames(num.axis)[[1]]
num.pca$label <- num.levels
num.pca$levels <- length(num.levels)
num.pca$level <- seq(length(num.levels))
num.pca$pscale <- 1

cat.centroids.list = list()
for (category in categorical.names) {
  centroids <- sapply(num.coords, . %>% tapply(., df[[category]], mean))
  total <- sapply(num.coords, . %>% tapply(., df[[category]], mean))
  centroids.df <- data.frame(centroids)
  centroids.df$name <- category
  cat.levels <- dimnames(centroids)[[1]]
  centroids.df$label <- cat.levels
  centroids.df$levels <- length(cat.levels)
  centroids.df$level <- seq(length(cat.levels))
  centroids.df$pscale <- table(df[[category]]) * length(cat.levels) / nrow(df)
  cat.centroids.list[[category]] <- centroids.df
}
cat.centroids <- do.call("rbind", cat.centroids.list)

stats.df <- rbind(num.pca, cat.centroids)

cat.values <-c("education", "job", "marital")
p <- ggplot(stats.df, aes(x = PC2, y = PC3)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  geom_segment(aes(xend = 0, yend = 0),
               arrow = arrow(length = unit(0.2, "cm"),
                             ends="first", type = "closed"),
               color = "darkblue", size = .5,
               data = ~filter(.x, name %in% c("PCA axis"))) +
  geom_point(aes(colour = name), size = 2,
             data = ~filter(.x, name %in% cat.values)) +
  geom_text_repel(aes(label = label, colour = name), show.legend = FALSE,
                  data = ~filter(.x, name %in% c("PCA axis", cat.values))) +
  scale_color_manual(values=c(rainbow(length(cat.values)), "black")) +
  scale_size(guide = 'none') +
  theme(legend.position = c(0.1, 0.2),
        legend.background = element_rect(
          fill="lightblue", linewidth=0.5,
          linetype="solid", colour ="darkblue"
        )); plot(p)

cat.values <-c("month")
p <- ggplot(stats.df, aes(x = PC1, y = PC4)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  geom_segment(aes(xend = 0, yend = 0),
               arrow = arrow(length = unit(0.2, "cm"),
                             ends="first", type = "closed"),
               color = "darkblue", size = .5,
               data = ~filter(.x, name %in% c("PCA axis"))) +
  geom_point(aes(colour = name, size = pscale),
             data = ~filter(.x, name %in% cat.values)) +
  geom_text_repel(aes(label = label, colour = name), show.legend = FALSE,
                  data = ~filter(.x, name %in% c("PCA axis", cat.values))) +
  scale_color_manual(values=c(rainbow(length(cat.values)), "black")) +
  scale_size(guide = 'none') +
  theme(legend.position = c(0.35, 0.2),
        legend.background = element_rect(
          fill="lightblue", linewidth=0.5,
          linetype="solid", colour ="darkblue"
        )); plot(p)


mca <- MCA(df, quanti.sup = which(numerical),
               quali.sup = which(response),
               method = "Indicator", graph = FALSE)

plot(mca, invisible = c("ind"), cex = 0.9,
     title = "Indicator method",
     col.var = c(
       rep("green", length(levels(df$job))),
       rep("turquoise", length(levels(df$marital))),
       rep("blue", length(levels(df$education))),
       rep("purple", length(levels(df$default))),
       rep("pink", length(levels(df$housing))),
       rep("red", length(levels(df$loan))),
       rep("orange", length(levels(df$contact))),
       rep("yellow3", length(levels(df$month))),
       rep("brown", length(levels(df$poutcome))),
       rep("black", length(levels(df$y)))
     )) + theme(aspect.ratio=.5)
