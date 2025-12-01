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

numerical = !categorical
numerical.names = names(df)[numerical]

# Division by purpose
response.name = "y"
response = names(df) == response.name

explanatory = names(df) != response.name
explanatory.name = names(df)[explanatory]

# Load libraries
library(FactoMineR)
library(factoextra)
library(ggpubr)

## INDICATOR MCA

mca.ind <- MCA(df, quanti.sup = which(numerical),
           quali.sup = which(response),
           method = "Indicator", graph = FALSE)
plot(mca.ind, invisible = c("ind", "quanti.sup"), cex = 0.8)

## BURT TABLE MCA
mca.burt <- MCA(df, quanti.sup = which(numerical),
                quali.sup = which(response),
                method = "Burt", graph = TRUE)

# COMPARISON INDICATOR VS BURT TABLE
p1 <- plot(mca.ind, invisible = c("ind"), cex = 0.8,
           title = "Indicator method",
           col.var = c(
             rep("green", length(levels(df$job))),
             rep("turquoise", length(levels(df$marital))),
             rep("blue", length(levels(df$education))),
             rep("purple", length(levels(df$default))),
             rep("pink", length(levels(df$housing))),
             rep("red", length(levels(df$loan))),
             rep("orange", length(levels(df$contact))),
             rep("yellow", length(levels(df$month))),
             rep("brown", length(levels(df$poutcome))),
             rep("black", length(levels(df$y)))
           ))
p2 <- plot(mca.burt, invisible = c("ind"), cex = 0.8,
           title = "Burt method",
           col.var = c(
             rep("green", length(levels(df$job))),
             rep("turquoise", length(levels(df$marital))),
             rep("blue", length(levels(df$education))),
             rep("purple", length(levels(df$default))),
             rep("pink", length(levels(df$housing))),
             rep("red", length(levels(df$loan))),
             rep("orange", length(levels(df$contact))),
             rep("yellow", length(levels(df$month))),
             rep("brown", length(levels(df$poutcome))),
             rep("black", length(levels(df$y)))
           ))
ggpubr::ggarrange(p1, p2)

# DIMENSION SIGNIFICANCE
print(mca.burt$eig)
barplot(mca.burt$eig[,1], main = "Eigenvalues",
        names.arg = names(mca.burt$eig), las = 2)
barplot(mca.burt$eig[,3], main = "Eigenvalues",
        names.arg = names(mca.burt$eig), las = 2)
lines(c(0, 100), rep(80, 2), col = 'red', lty = 2)

# RELEVANCE OF VARIABLES IN INITIAL DIMENSIONS
fviz_mca_var(mca.burt, choice = "mca.cor", 
             axes = c(1, 2), # Dimensions of MCA
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(mca.burt,
             axes = c(1, 2), # Dimensions of MCA
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
fviz_mca_var(mca.burt,
             axes = c(3, 4), # Dimensions of MCA
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

# CONTRIBUTORS ON THE MAIN DIMENSIONS
print(mca.burt$var$contrib)
fviz_contrib(mca.burt, choice = "var", axes = 1:5, top = 15)
p1 <- fviz_contrib(mca.burt, choice = "var", axes = 1, top = 10)
p2 <- fviz_contrib(mca.burt, choice = "var", axes = 2, top = 10)
p3 <- fviz_contrib(mca.burt, choice = "var", axes = 3, top = 10)
p4 <- fviz_contrib(mca.burt, choice = "var", axes = 4, top = 10)
ggpubr::ggarrange(p1, p2, p3, p4)

fviz_mca_var(mca.burt, col.var = "contrib",
             axes=c(1, 2),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal())
fviz_mca_var(mca.burt, col.var = "contrib",
             axes=c(3, 4),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal())

# DISTRIBUTION OF FACTORS WITH MAIN CONTRIBUTORS MODALITIES
fviz_mca_ind(mca.burt, 
             label = "none", # hide individual labels
             habillage = "education", # color by groups 
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "#000000"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())

fviz_mca_ind(mca.burt, 
             label = "none", # hide individual labels
             habillage = "job", # color by groups 
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())

fviz_mca_ind(mca.burt, 
             axes = c(3, 4), # Dimensions of MCA
             label = "none", # hide individual labels
             habillage = "poutcome", # color by groups 
             palette = c("#FC4E07", "#000000", "#E7B800", "#00AFBB"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())

fviz_mca_ind(mca.burt, 
             label = "none", # hide individual labels
             habillage = "contact", # color by groups 
             palette = c("#FC4E07", "#00AFBB"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())

fviz_mca_ind(mca.burt, 
             label = "none", # hide individual labels
             habillage = "housing", # color by groups 
             palette = c("#FC4E07", "#00AFBB"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())

fviz_mca_ind(mca.burt, 
             label = "none", # hide individual labels
             habillage = "marital", # color by groups 
             palette = c("#FC4E07", "#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())

fviz_mca_ind(mca.burt, 
             label = "none", # hide individual labels
             habillage = "month", # color by groups
             palette = rainbow(12),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())

# DISTRIBUTION FOR ALL FACTORS
plotellipses(mca.burt, keepvar=c("quali"), cex=0.4)

# RELATION TO SUPLEMENTARI VARIABLES
dimdesc(mca.burt)

plot(mca.burt, invisible = c("ind", "quali.sup", "var"), cex = 0.8)
mca.dims <- function(X, axis1, axis2, labels, border = 1.1) {
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

par(mfrow=c(1, 2))
mca.dims(mca.burt$quanti.sup$coord, 1, 2, numerical.names)
mca.dims(mca.burt$quanti.sup$coord, 3, 4, numerical.names)
par(mfrow=c(1, 1))

p1 <- fviz_mca_ind(mca.burt, 
                   axes = c(1, 2), # Dimensions of MCA
                   label = "none", # hide individual labels
                   habillage = df$y, # color by groups
                   palette = c("#FC4E07", "#00AFBB"),
                   addEllipses = TRUE, ellipse.type = "confidence",
                   ggtheme = theme_minimal())
p2 <- fviz_mca_ind(mca.burt, 
                   axes = c(3, 4), # Dimensions of MCA
                   label = "none", # hide individual labels
                   habillage = df$y, # color by groups
                   palette = c("#FC4E07", "#00AFBB"),
                   addEllipses = TRUE, ellipse.type = "confidence",
                   ggtheme = theme_minimal())
ggpubr::ggarrange(p1, p2)

## MOST SIGNIFICANT MCA (To plot a less cluttered with similar relevance)
main.vars <- c("y", "education", "job", "housing", "poutcome", "marital")

mca.red <- MCA(df[main.vars], quali.sup = 1, method = "Indicator", graph = FALSE)
cats = apply(df[main.vars[2:6]], 2, function(x) nlevels(as.factor(x)))
sups = apply(df[main.vars[1]], 2, function(x) nlevels(as.factor(x)))

mca1_vars_df = data.frame(mca.red$var$coord, Variable = rep(names(cats), cats))
mca1_obs_df = data.frame(mca.red$ind$coord)

mca2_vars_df = data.frame(mca.red$quali.sup$coord, Variable = rep(names(sups), sups))
mca3_vars_df = rbind(mca1_vars_df, mca2_vars_df)

library(ggplot2)
ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca3_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca3_vars_df), colour = Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR") +
  scale_colour_discrete(name = "Variable")
