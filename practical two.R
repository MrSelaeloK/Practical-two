#Generating simulated data
set.seed(1)
length<-100
X<-(1:length)
error_terms<-rnorm(100,0,02^2)
error_terms
Y_matrix<-matrix(error_terms)

for(i in 1:length){
  
  Y_matrix[i]<-sin(X[i]/10)+Y_matrix[i]
}
# span 
k<-ceiling(0.021*length)
k

