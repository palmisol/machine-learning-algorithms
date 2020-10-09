mydata1 = read.csv("/Users/mps/AI/Lab2/spaeth_01.csv" , sep=",")

plot(mydata1)

mydata1 = na.omit(mydata1) # Detele missing data
mydata1 = scale(mydata1) # Standarize variables

d <- dist(mydata1, method = "euclidean") #Distance matrix ( middle diaginal is "0")

#Average link clustering - look at all the distances of all pairs and their average. 
fit <- hclust(d, method = "average")
plot(fit)

#Create the clusters by cutting this dendrogram
Hgroups <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")
plot(mydata1, col=Hgroups)

#Different cluster number
HC <- cutree(fit, k=4) 
HC <- cutree(fit, k=5)
HC <- cutree(fit, k=6)
HC <- cutree(fit, k=7) 
plot(mydata1, col=HC)

#Distance betweent the two members that are farthest apart 
fit2 <- hclust(d, method = "complete")
#Distance between the closest memebers of two cluster
fit2 <- hclust(d, method = "single") 

plot(fit2)

