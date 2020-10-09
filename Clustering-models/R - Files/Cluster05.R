mydata = read.csv("/Users/mps/AI/Lab2/spaeth_02.csv" , sep=",")
plot(mydata)
mydata = na.omit(mydata) # Detele missing data
mydata = scale(mydata) # Standarize variables
d <- dist(mydata, method = "euclidean") #Distance matrix ( middle diaginal is "0")

#Average link clustering - look at all the distances of all pairs and their average. 
fit <- hclust(d, method = "average")

#Distance betweent the two members that are farthest apart 
fit <- hclust(d, method = "complete")

#Distance between the closest memebers of two cluster
fit <- hclust(d, method = "single") 

plot(fit)

#Create the clusters by cutting this dendrogram
Hgroups <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")

#Different cluster number
Hgroups <- cutree(fit, k=4) 
Hgroups <- cutree(fit, k=5)
Hgroups <- cutree(fit, k=6)
Hgroups <- cutree(fit, k=7) 

plot(mydata, col=Hgroups)