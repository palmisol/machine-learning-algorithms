mydata1 = read.csv("/Users/mps/AI/Lab2/spaeth_01.csv" , sep=",")
mydata2 = read.csv("/Users/mps/AI/Lab2/spaeth_02.csv" , sep=",")
mydata3 = read.csv("/Users/mps/AI/Lab2/spaeth_03.csv" , sep=",")
mydata4 = read.csv("/Users/mps/AI/Lab2/spaeth_04.csv" , sep=",")
mydata5 = read.csv("/Users/mps/AI/Lab2/spaeth_05.csv" , sep=",")

plot(mydata1)
plot(mydata2)
plot(mydata3)
plot(mydata4)
plot(mydata5)

mydata1 = na.omit(mydata1) # Detele missing data
mydata1 = scale(mydata1) # Standarize variables

mydata2 = na.omit(mydata2) # Detele missing data
mydata2 = scale(mydata2) # Standarize variables

mydata3 = na.omit(mydata3) # Detele missing data
mydata3 = scale(mydata3) # Standarize variables

mydata4 = na.omit(mydata4) # Detele missing data
mydata4 = scale(mydata4) # Standarize variables

mydata5 = na.omit(mydata5) # Detele missing data
mydata5 = scale(mydata5) # Standarize variables

d <- dist(mydata, method = "euclidean") #Distance matrix ( middle diaginal is "0")

#Average link clustering - look at all the distances of all pairs and their average. 
fit <- hclust(d, method = "average")
plot(fit)

#Create the clusters by cutting this dendrogram
Hgroups <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")
plot(mydata, col=Hgroups)

#Different cluster number
HC <- cutree(fit, k=4) 
HC <- cutree(fit, k=5)
HC <- cutree(fit, k=6)
HC <- cutree(fit, k=7) 
plot(mydata, col=HC)

#Distance betweent the two members that are farthest apart 
fit2 <- hclust(d, method = "complete")
#Distance between the closest memebers of two cluster
fit2 <- hclust(d, method = "single") 

plot(fit2)




