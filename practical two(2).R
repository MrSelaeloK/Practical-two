#Generating simulated data
set.seed(1)
X<-sample(1:100,100,FALSE)
error_terms<-rnorm(100,0,02^2)
Y_matrix<-matrix(error_terms)

for(i in 1:length(X)){
  
  Y_matrix[i]<-sin(X[i]/10)+Y_matrix[i]
}
# span 
k<-ceiling(0.021*length(X))#should be put inside function

#number of neighbuors determines how much sectioning off is done
#matrices and vectors
weights<-rep(NA,
                length(X))
distances_and_index<-matrix(NA,
                  length(X),ncol=2)
sortedDistances_and_index<-matrix(NA,
                                 length(X),ncol=2)
y_estimates<-matrix(NA,length(X))

lowess<-rep(NA,length(X))# check if reset exists before entering outer loop

#find K closest relatives
#take a smaller and kick out the largest
for(i in 1:length(X)){
  for(j in 1:length(X)){
    distances_and_index[j,1]<-abs(X[j]-X[i])
    distances_and_index[j,2]<-j
  } #at this stage all the distances have been filled up
  X
  distances_and_index
  sortedDistances_and_index<-distances_and_index[order(distances_and_index[,1]),]
  sortedDistances_and_index
  #Finding the closest neighbours index and adding to weight to weights
  maxdistance<-sortedDistances_and_index[k+1,1] #may have to fix max distance
  
  #please help here
  #max distance is 2
  for(l in 1:length(X)){
    if(sortedDistances_and_index[l,1]<=maxdistance){ #closest index found
      print(sortedDistances_and_index[l,2])
      weights[sortedDistances_and_index[l,2]]<-
       (1-(abs(X[i]-X[sortedDistances_and_index[l,2]])/maxdistance)^3)^3
    }else{
      weights[sortedDistances_and_index[l, 2]] <- 0
    }
  }
  
  #calculating the beta
  new_X<-cbind(rep(1,length(X)),X)
  new_X
  Beta_coefficients<-solve(t(new_X)%*%diag(weights)%*%new_X)%*%(t(new_X)%*%diag(weights)%*%Y_matrix)
  Beta_coefficients
  
  lowess[i]<-Beta_coefficients[1]+Beta_coefficients[2]*X[i]
  
  }
lowess
