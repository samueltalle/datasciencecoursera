
pollutantmean<-function(directory,pollutant,id){
  
  list_files<-c(list.files(path=paste("./",directory,sep="")))
  result<-NULL
  
  if(pollutant=="sulfate"){
  
    for(i in id){
      mydata<-na.omit(read.csv(paste("./specdata/",list_files[i],sep=""), header = TRUE))
      result<-c(result,mydata$sulfate)
      
    }
    mean(result)
  }
  else{
      if(pollutant=="nitrate"){
        var_sulfate<-NULL
        for(i in id){
          mydata<-na.omit(read.csv(paste("./specdata/",list_files[i],sep=""), header = TRUE))
          result<-c(result,mydata$nitrate)
        
        }
        mean(result)
      }
      else{
        print("Pollutant does not exist")
      }
  }
  #mean(result)
}
  

