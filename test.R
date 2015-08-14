z<-matrix(rnorm(30),10,3,dimnames=list(c(),c("a","b","c")))
z[3,3]<-NA
z[10,3]<-NA
head(z)
vals<-matrix(c(1,2,3,4,5,6,7,8,9,10),10,1)
for(i in 1:nrow(z)) {
  print(i)
  if (is.na(z[i,3])) {z[i,3]<- vals[i]}
}