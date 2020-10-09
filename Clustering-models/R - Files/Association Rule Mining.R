#ASSOCIATION RULES

#Adapted from https://towardsdatascience.com/association-rule-mining-in-rddf2d044ae50

install.packages("arules")
install.packages("arulesViz")
library(arules)

#Read in Groceries data
data('Groceries')

#inspect first 3 transactions
inspect(head(Groceries, 3))

#Apriori Algorithm - generte rules
grocery_rules <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.5))

#inspect first 3 sorted by confidence
inspect(head(sort(grocery_rules, by = 'confidence'), 3))