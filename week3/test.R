

library(glmnet)
getPowerOne <- function(n, d, M=100, alpha = .05){
  
  isClose <- rep(0, M)
  
  
  for(i in 1:M){
    #print(i)
    
    df <- data.frame(blank = rep(0, n))
    df$errors <- rnorm(n)
    df$treated  = (rep(0:1, each = n/2))
    #generate data with real effect
    df$y = d * df$treated+ df$errors
    results  = glm(df$y ~ df$treated)
    print(summary(results)$coefficients["df$treated", 4])
    dHatValue = summary(results)$coefficients["df$treated", 4]
    if(dHatValue < alpha){
      isClose[i] = 1 #yes it is sinificant
    }
    else{
      isClose[i] = 0 
    }
    
  }
  return (sum(isClose)/M)
  #return proportion of loops where it was significant
  
  
}

run1 <- getPowerOne(1000, .5)
run2 <- getPowerOne(1000, .5, 100, .01)
run3 <- getPowerOne(4, .5)


#Build a higher-level function that takes d and power = 0.80 that outputs N required. This will call the base function.
getNRequired = function(d, power = .80){
  N=500 #MAX OUT AT 1000
  while(N<power){
    
  }
  for(i in seq(from=4, to=N, by=2)){
    mypower =  getPowerOne(i, d)
    print(mypower)
    if(mypower == power){
      return(i)
    }
  }
  
}

nRquired = getNRequired(.01)
#want lower d 
getDRequired = function(n, power = .80){
  incrementD = TRUE
  while(mypower != power){
    
  getPowerOne(n, d)
  
  }
  
}
