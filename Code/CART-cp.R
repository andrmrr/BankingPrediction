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
library(rpart)
library(rpart.plot)
library(ROSE)
library(randomForest)

# Seed
set.seed(1234)
# Train test split
n <- nrow(df)
ind <- sample(2, nrow(df), replace = T, prob = c(0.7, 0.3))
ind.train <- ind == 1
df.train <- df[ind.train,]
y.train <- df.train$y
ind.test <- ind == 2
df.test <- df[ind.test,]
y.true <- df.test$y

# ROSE package ovun.sample()
df.ROSE <- ovun.sample(y~., data = df.train, N = nrow(df.train), p = 0.5)$data

# CP exploration
tree.ROSE <- rpart(y ~ ., method = "class", data = df.ROSE, control = rpart.control(cp = 0))
plotcp(tree.ROSE)
lowest <- which.min(tree.ROSE$cptable[,"xerror"])
cp.cut <- tree.ROSE$cptable[lowest, "CP"]
threshold <- tree.ROSE$cptable[lowest, "xerror"] + 1.96 * tree.ROSE$cptable[lowest, "xstd"]
error.lower <- which(tree.ROSE$cptable[, "xerror"] < threshold)[1] - 1
cp.error.cut <- tree.ROSE$cptable[error.lower, "CP"]
tree.ROSE <- prune(tree.ROSE, cp.cut)
tree.error.ROSE <- prune(tree.ROSE, cp.error.cut)

y.pred.prob <- predict(tree.ROSE, df.train)
y.train.ROSE <- levels(y.true)[max.col(y.pred.prob)]
y.pred.prob <- predict(tree.ROSE, df.test)
y.pred.ROSE <- levels(y.true)[max.col(y.pred.prob)]

y.pred.prob <- predict(tree.error.ROSE, df.train)
y.train.error.ROSE <- levels(y.true)[max.col(y.pred.prob)]
y.pred.prob <- predict(tree.error.ROSE, df.test)
y.pred.error.ROSE <- levels(y.true)[max.col(y.pred.prob)]

table(y.true, y.pred.ROSE)
plotcp(tree.ROSE)
prp(tree.ROSE, type=0, extra=0)
prp(tree.error.ROSE, type=0, extra=0)

# Sample weights 
y.class <- table(df.train$y)
y.weights <- length(df.train$y) / y.class[df.train$y] / length(y.class)

# CP exploration
tree.w <- rpart(y ~ ., method = "class", data = df.train, weights = y.weights, control = rpart.control(cp = 0))
plotcp(tree.w)
cp.w.cut <- tree.w$cptable[which.min(tree.w$cptable[,"xerror"]), "CP"]
tree.w <- prune(tree.w, cp.w.cut)

y.pred.prob <- predict(tree.w, df.train)
y.train.weights <- levels(y.true)[max.col(y.pred.prob)]
y.pred.prob <- predict(tree.w, df.test)
y.pred.weights <- levels(y.true)[max.col(y.pred.prob)]

table(y.true, y.pred.weights)
plotcp(tree.w)
prp(tree.w, type=1, extra=4)

##==== Random Forest ====##

forest.all <- randomForest(y ~ ., data = df.ROSE)

y.train.forest <- predict(forest.all, df.train)
y.pred.forest <- predict(forest.all, df.test)

table(y.true, y.pred.forest)

##==== other params ====##

params = rpart.control(maxdepth = 5, minsplit = 3, minbucket = 3)

tree.params <- rpart(y ~ ., method = "class", data = df.ROSE, control = params)

y.pred.prob <- predict(tree.params, df.train)
y.train.params <- levels(y.true)[max.col(y.pred.prob)]
y.pred.prob <- predict(tree.params, df.test)
y.pred.params <- levels(y.true)[max.col(y.pred.prob)]

prp(tree.params, type=1, extra=3)

# Comparison

values <- function(gt, pred) {
  tab <- table(gt, pred)
  acc <- 100 * mean(gt == pred)
  sensitivity <- 100 * prop.table(tab, margin = 1)
  predictivity <- 100 * prop.table(tab, margin = 2)
  print(sprintf("Accuracy %4.2f%%", acc))
  print(sprintf("Sensitivity %4.2f%%", sensitivity['yes', 'yes']))
  print(sprintf("Specificity %4.2f%%", sensitivity['no', 'no']))
  print(sprintf("Pos Pred Value %4.2f%%", predictivity['yes', 'yes']))
  print(sprintf("Neg Pred Value %4.2f%%", predictivity['no', 'no']))
}

print("ROSE")
values(y.train, y.train.ROSE)
values(y.true, y.pred.ROSE)
print("ROSE (plus error)")
values(y.train, y.train.error.ROSE)
values(y.true, y.pred.error.ROSE)
print("Params")
values(y.train, y.train.params)
values(y.true, y.pred.params)
print("Forest")
values(y.train, y.train.forest)
values(y.true, y.pred.forest)

importance.matrix <- matrix(c(
  forest.all$importance[,1],
  tree.ROSE$variable.importance[explanatory.name],
  tree.error.ROSE$variable.importance[explanatory.name],
  tree.params$variable.importance[explanatory.name]),
  nrow = 4, byrow = TRUE, dimnames = list(c(
    "Random Forest", "Decision Tree (optimal cp)", "Decision Tree (cp with deviation)", "Decision Tree (apriori)"
  ), explanatory.name))
barplot(importance.matrix, beside = TRUE, las = 2,
        col = c("red", "green", "yellow", "blue"), main = "Feature Importance",
        legend.text = TRUE, args.legend = list(x = "topleft"))

