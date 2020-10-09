x = 1
isError <- function(expr){
  tryCatch(expr,
           error = function(err){
             message("An error occurred:\n", err)
             return(NULL)
           },
           warning = function(warn){
             message("A warning occured:\n", warn)
             return(NULL)
           })
}

#result <- isError(kmeans(irisRealData, x))

while (is.null(result <- isError(kmeans(irisRealData, x))) == FALSE) {
  x = x + 1 
  print(x)
  result <- isError(kmeans(irisRealData, x))
}


