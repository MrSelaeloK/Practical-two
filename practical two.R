#Generating simulated data
set.seed(1)
X<-1:100
error_terms<-rnorm(100,0,0.2)
Y_matrix<-matrix(error_terms)

for(i in 1:length(X)){
  
  Y_matrix[i]<-sin(X[i]/10)+Y_matrix[i]
}
LowessFunction<-function(X,Y,f){
  
  # function for findingm the span 
  k<-ceiling(f*length(X))
  
  #matrices and vectors that will be used in the calculation of the lowess smoothing
  weights<-rep(0,
               length(X))                #weights vector
  
  distances_and_index<-matrix(NA,
                              length(X),
                              ncol=2)     #matrix of distances of all neighbours and thier                                                indexes 
  
  sortedDistances_and_index<-matrix(NA,
                                    length(X),
                                    ncol=2)    #matrix of distances and all neigbours sorted out                                              by distance to the closest neighbours
  
  lowess<-rep(NA,
              length(X))      # Lowess vector that will store the lowess values after they have                                been calulated
  
  # for loop for fining K closest relatives, finding their distances and also finding lowess values that will be updated to the lowess vector
  
  for(i in 1:length(X)){
    for(j in 1:length(X)){
      distances_and_index[j,1]<-abs(X[j]-X[i])
      distances_and_index[j,2]<-j
    }                                           #at this stage all the distances have been filled up                                                 for X[i]
    
    #Ordering/Sorting of the distances and index matrix
    sortedDistances_and_index<-distances_and_index[order(distances_and_index[,1]),]
    
    #Finding the closest neighbors index and adding to weight to weights
    maxdistance<-sortedDistances_and_index[k+1,1]         # k neighbours + 1 used becasue the use                                                           of distance matrices in findking k                                                             closest neighors results in the X[j]                                                           itself being considered relative to                                                            its neigbours
    
    #Calculation of the weights using the weights function
    for(l in 1:length(X)){
      if(sortedDistances_and_index[l,1]<=maxdistance){ 
        weights[sortedDistances_and_index[l,2]]<-
          (1-(abs(X[i]-X[sortedDistances_and_index[l,2]])/maxdistance)^3)^3
      }
    }
    #At this point, the for loop has calculated the weights of all k neighbors and has assigned 0 weights to all neighbors that are more than k closest neigbours away
    
    #calculating the beta_coefficients
    new_X=cbind(rep(1,
                    length(X)),
                X)             #adding the intercept of 1s to X for beta                                                       coefficient calculation
    
    Beta_coefficients<-solve(t(new_X)%*%diag(weights)%*%new_X)%*%(t(new_X)%*%diag(weights)%*%Y_matrix)
    
    
    lowess[i]<-Beta_coefficients[1]+Beta_coefficients[2]*X[i]   #assiging lowess values to the                                                                       lowess vector
  }
  
  #output refers to the output of the R. built in lowess function and is plotted in red
  #plot is a plot of the manually calculated lowess function
  output<-lowess(X,Y_matrix,f,iter=0)
  output
  plot(X, Y_matrix,main = "Lowess Smoothing",
       xlab = "X",
       ylab = "Y",
       pch = 16)
  
  lines(lowess,
        col ="red",
        lwd=2)
  
  lines(output,
        col="blue",
        lwd=2)
  
  legend("bottomright",                         
         legend = c("Lowess",
                    "Output"),      
         col = c("red",
                 "blue"),
         lwd = 2)
}
LowessFunction(X,Y,0.051)
