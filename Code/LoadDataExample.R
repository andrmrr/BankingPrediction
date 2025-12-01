# author: David Candela
# date: 05/10/2023 18:32

## Set working path to this file's path (RStudio necessary)
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

## Load Bank information and display basic information
df <- read.table("bank.csv", header = T, sep = ";", stringsAsFactors = TRUE)
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

## Set missing values to NA
df[categorical][df[categorical] == "unkown"] = NA
df$pdays[df$pdays == -1] = NA

## Show data without missing values
summary(df)
