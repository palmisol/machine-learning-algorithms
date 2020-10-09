L = 15
x <- vector(mode = "double", length = L)
x1 <- vector(mode = "double", length = L)
y <- vector(mode = "double", length = L)

for (i in 2 : L) {
  # Kgroups <- kMeansClustering(irisData, i)
  # Hgroups <- Hclustering(irisData, i, "average")
  # wk = WK_R(Kgroups, Hgroups)
  wk = WK_R(irisRealData$X1, kMeansClustering(irisData, K))
  y[i] = wk
  x[i] = i
  message("K = ", i," Weighted Kappa = ",  wk)
}
par(mfrow=c(1,1))
plot(x, y, xlab = "K", ylab = "Weighted Kappa")

#plot(1, type="n", xlab="K means", ylab="Weighted Kappa", xlim=c(0, 15), ylim=c(-1, 1))

#### Data FRames ####

df = data.frame(matrix(vector(), 0, 4,
                       dimnames=list(c(), c("lowerK", "upperK", "WK", "AgreementStrength"))))

df <- data.frame(lowerK=integer(),
                 upperK=integer(),
                 WK=double(),
                 AgreementStrength = character(),
                 stringsAsFactors=FALSE)



emptyFrame = data.frame(upperK = 0, lowerK = 0, WKk = 0.0, 
                        AgreementStrength = NA, stringsAsFactors= FALSE)

df = rbind(df , c(3,3,3,"test"))

#### Compare K means ####

lowBand = 7
highBand = 15

#Declare vectors
K1vector <- vector(mode = "integer", length = L)
K2vector <- vector(mode = "integer", length = L)
WKvector <- vector(mode = "integer", length = L)
AgreementVector <- vector(mode = "character", length = L)

for ( i in 1 : L) {
  
  # Generate two random numbers 
  K <- runif(1, min = 2, max = lowBand)
  K1 = as.integer(K)
  
  K <- runif(1, min = 8, max = highBand)
  K2 = as.integer(K)
  
  #calculate Weighted Kappa 
  wk = WK_R(kMeansClustering(irisData, K1), kMeansClustering(irisData, K2))
  AS = getAgreementStrengt(wk)
  
  #Store randome numbers in a vector
  K1vector[i] = K1
  K2vector[i] = K2
  
  #Store Weighted Kappa
  WKvector[i] = wk
  AgreementVector[i] = AS
  
  message("k1 = ", K1, " and ", "K2 = ", K2, " WK = ", AS)
  
  outputData <- data.frame(K1vector, K2vector, WKvector, AgreementVector)
  
  return(outputData)
  
}

cl <- colors()
groupColours = c("red", "blue", "green", "orange", "blue", "yellow", )

plot(1, type = "n" ,xlab="K means", ylab="Weighted Kappa", xlim=c(0, 15), ylim=c(-1, 1))

plotWK <- function(df){
  
  
  
  ggplot() + geom_linerange(data=df, 
                            mapping=aes(x=WKvector, ymin=upperKvector, ymax = lowerKvector), 
                            size=0.5)
}