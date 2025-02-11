#Generating simulated data
set.seed(1)
X<-1:100
error_terms<-rnorm(100,0,0.2)
Y_matrix<-matrix(error_terms)

for(i in 1:length(X)){
  
  Y_matrix[i]<-sin(X[i]/10)+Y_matrix[i]
}
