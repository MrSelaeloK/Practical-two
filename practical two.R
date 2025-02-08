#Generating simulated data
set.seed(1)
length<-100
X<-matrix(1:length)
error_terms<-rnorm(100,0,02^2)
error_terms
Y_matrix<-matrix(error_terms)

for(i in 1:length){
  
  Y_matrix[i]<-sin(X[i]/10)+Y_matrix[i]
}
# span 
k<-ceiling(0.021*length)+1#should be put inside function
#-----
X[1]
distances<-matrix(NA,nrow=nrow(X),ncol=2) #col2 is index of distance

for (i in 1:nrow(X)){
  distances[i,1]<-abs(X[1]-X[i])
  distances[i,2]<-X[i]
}
sorted_distances<-distances[order(distances[,1]),]#sorted by distances
sorted_distances

max_distance <-max(sorted_distances[1:k,1])
max_distance
weights<-matrix(NA,nrow=nrow(X))
weights[1]
#determining the weights
for( i in 1:nrow(X)){
  if(sorted_distances[i,1]<=max_distance){
    weights[i]<-(1-(abs(X[1]-sorted_distances[i,2])/max_distance)^3)^3
  }else{
    weights[i]<-0
  }
}
intercept<-rep(1,nrow(X))
X<-as.matrix(cbind(intercept,X))

#calculating Beta coefficients
Y
weights_vector<-as.vector(weights)
Beta_coefficients<-solve(t(X)%*%diag(weights_vector)%*%X)%*%(t(X)%*%diag(weights_vector)%*%Y_matrix)
Beta_coefficients



