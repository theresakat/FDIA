t<-rep(1:5,5)
dim(t)<-c(5,5);t
testdat<-data.frame(t)
rowID<-25:30
newID<-c(51,65,100,98,200, 158)
corrections<-data.frame(cbind(rowID,newID))
rm(t, rowID,newID)