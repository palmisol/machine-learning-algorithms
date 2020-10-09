mydata = read.csv("/Users/mps/AI/Lab2/spaeth_02.csv" , sep=",")

plot(mydata)

mydata = na.omit(mydata) # Detele missing data
mydata = scale(mydata) # Standarize variables

d <- dist(mydata, method = "euclidean") #Distance matrix ( middle diaginal is "0")

fit <- kmeans(mydata, 5) # 5 cluster solution

aggregate(mydata,by=list(fit$cluster),FUN=mean)

Hgroups = fit$cluster

plot(mydata, col=Hgroups)