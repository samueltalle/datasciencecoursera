
complete<-function(directory,nbre){
  
  id<-NULL
  nob<-NULL
  
  list_files<-c(list.files(path=paste("./",directory,sep="")))
  
  for(i in nbre){
    mydata<-na.omit(read.csv(paste("./specdata/",list_files[i],sep=""), header = TRUE))
    id<-c(id,unique(mydata$ID))           
    nob<-c(nob,nrow(mydata))
    
  }
  result<-data.frame(id,nob)
  result
}
  
  
  
  