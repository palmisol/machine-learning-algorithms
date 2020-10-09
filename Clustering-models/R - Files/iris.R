irisData = read.csv("/Users/mps/AI/Lab2/iris.csv" , sep=",")

irisData  = na.omit(irisData) # Detele missing data
irisData = scale(irisData) # Standarize variables

plot(main="Iris", irisData)

irisFit <- kmeans(irisData, 8) #cluster solution

aggregate(irisData,by=list(irisFit$cluster),FUN=mean)

irisGroup = irisFit$cluster

plot(irisData, col=irisGroup)



