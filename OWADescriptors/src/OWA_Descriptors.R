orness <- function(inputs)
{
  #put the inputs in local variables to work better
  weights = inputs$weights
  n<-length(weights)
  
  #return variable
  result <-0
  
  #Orness calculation
  for (i in 1:length(weights)){
    actual <- weights[i]
    actual <- actual*((n-i)/(n-1))
    result<-result + actual
    
  }
  
  #put the name for each alternative
  names(result) <- "Orness"
  return(list(lambda = result))
}

balance <- function(inputs)
{
  #put the inputs in local variables to work better
  weights = inputs$weights
  n<-length(weights)
  
  #return variable
  result <-0
  
  #Balance calculation
  for (i in 1:length(weights)){
    actual <- weights[i]
    actual <- actual*((n+1-(2*i))/(n-1))
    result<-result + actual
    
  }
  
  #put the name for each alternative
  names(result) <- "Balance"
  
  return(list(lambda = result))
}

entropy <- function(inputs)
{
  #put the inputs in local variables to work better
  weights = inputs$weights
  n<-length(weights)
  
  #return variable
  result <-0
  
  #Entropy calculation
  for (i in 1:length(weights)){
    actual <- weights[i]
    if (actual > 0)
      actual <- actual*(log(actual))
    result<-result + actual
    
  }
  result <- -result
  
  #put the name for each alternative
  names(result) <- "Entropy"
  
  return(list(lambda = result))
}

divergence <- function(inputs, orness)
{
  #put the inputs in local variables to work better
  weights = inputs$weights
  n<-length(weights)
  ornessValue = orness
  if (is.null(ornessValue))
    stop("Orness value in Divergence calculation is null ")
  
  #return variable
  result <-0
  
  #Divergence calculation
  for (i in 1:length(weights)){
    actual <- weights[i]
    actual <- actual*((((n-i)/(n-1))-orness)^2)
    result<-result + actual
    
  }
  
  #put the name for each alternative
  names(result) <- "Divergence"
  
  return(list(lambda = result))
}

owaDescriptorMethod <- function(inputs)
{
  #execute the operations owa_descriptors
  result <- orness(inputs)$lambda
  result <- c(result, balance(inputs)$lambda)
  result <- c(result, entropy(inputs)$lambda)
  result <- c(result, divergence(inputs, result[1])$lambda)
  
  
  #if there aren't an error, return the values
  for (i in 1:length(result)){
    #if error, stop the execution
    if(is.null(result[i])){
      stop("Error in owa_descriptors operations ")
    }
  }
  return(list(orness=round(result[1],3), balance=round(result[2],3), entropy=round(result[3],3), divergence=round(result[4],3)))
}
