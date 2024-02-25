# setting up packages
library(arules)
library(arulesViz)
library(tidyverse)
library(dplyr)
library(plyr)
library(knitr)
library(lubridate)
library(RColorBrewer)
library(plotly)
library(igraph)

# importing bakery data set and cleaning
bakery <- read.csv('Bakery.csv')
bakery <- bakery[complete.cases(bakery), ]

bakery$Items <- as.factor(bakery$Items)
bakery$Daypart <- as.factor(bakery$Daypart)
bakery$DayType <- as.factor(bakery$DayType)

# group-by and creating transaction data
bakery <- bakery %>% distinct(.)
bakery_tr <- ddply(bakery, c("TransactionNo"), function(df1)paste(df1$Items, collapse = ","))
bakery_tr$TransactionNo <- NULL
colnames(bakery_tr) <- c('items')

write.csv(bakery_tr, '/Users/gladys/Documents/projects/data analysis/R/R/Bakery/Bakery_Transaction.csv', quote = FALSE, row.names = FALSE)


## EDA




## MARKET BASKET

# reading data set as transaction
b_tr <- read.transactions('/Users/gladys/Documents/projects/data analysis/R/R/Bakery/Bakery_Transaction.csv', format = 'basket', sep = ',')
summary(b_tr)

# frequency plot
itemFrequencyPlot(b_tr, topN = 10, type = 'absolute', col = "#EBD9B4", main = "Item Frequency Plot")

# generating rules
association_rules <- apriori(b_tr, parameter = list(supp=0.0005, conf=0.8, minlen=2, maxlen=10))
inspect(association_rules)

# based on highest frequencies
# coffee
cof_arules <- apriori(b_tr, parameter = list(supp=0.0001, conf=0.1, minlen=2),appearance = list(lhs="Coffee",default="rhs"))
inspect(head(cof_arules))

# Bread
bread_arules <- apriori(b_tr, parameter = list(supp=0.001, conf=0.08, minlen=2),appearance = list(lhs="Bread",default="rhs"))
inspect(bread_arules)

# Tea
tea_arules <- apriori(b_tr, parameter = list(supp=0.0001, conf=0.1, minlen=2),appearance = list(lhs="Tea",default="rhs"))
inspect(tea_arules)

# Cake
cake_arules <- apriori(b_tr, parameter = list(supp=0.0001, conf=0.1, minlen=2),appearance = list(lhs="Cake",default="rhs"))
inspect(cake_arules)

# Pastry
pastry_arules <- apriori(b_tr, parameter = list(supp=0.0001, conf=0.1, minlen=2),appearance = list(lhs="Pastry",default="rhs"))
inspect(pastry_arules)

# Sandwich
sw_arules <- apriori(b_tr, parameter = list(supp=0.0001, conf=0.1),appearance = list(lhs="Sandwich",default="rhs"))
inspect(sw_arules)

# scatter plot
plot(association_rules, engine="plotly")
bakery_aPlot <- plot(association_rules, engine="htmlwidget")
plot(association_rules, engine="base")


#graph
top10 <- head(association_rules, n = 10, by = "confidence")
bakery_aGraph <- plot(top10, method = "graph",  engine = "htmlwidget")
plot(top10, method = "graph",  engine = "igraph")


htmlwidgets::saveWidget(
  widget = bakery_aGraph, #the plotly object
  file = "bakery_aGraph.html", #the path & file name
  selfcontained = TRUE #creates a single html file
)

htmlwidgets::saveWidget(
  widget = bakery_aPlot, #the plotly object
  file = "bakery_aPlot.html", #the path & file name
  selfcontained = TRUE #creates a single html file
)
