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
sorted_distances<-distances[order(distances[,1]),]
sorted_distances

max_distance <-k
