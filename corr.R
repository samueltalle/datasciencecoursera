corr<-function(directory,threshold=0){
  result<-NULL
  
  list_files<-c(list.files(path=paste("./",directory,sep="")))
  
  for(i in 1:NROW(list_files)){
    mydata<-na.omit(read.csv(paste("./specdata/",list_files[i],sep=""), header = TRUE))
    if(nrow(mydata)>threshold){
      result<-c(result,cor(mydata$sulfate,mydata$nitrate))
    }
    
    
  }
  
  result
  
}
  
  