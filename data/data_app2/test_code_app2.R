data<-read.csv(file = "liste8_JMG.csv",sep = ";",header = TRUE,stringsAsFactors = FALSE)


precedant=data$X[1]

for(i in 2:dim(data)[1]){
  if(data$X[i]=="") data$X[i]=precedant else precedant=data$X[i]
  
}
