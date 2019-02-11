cnk<-mydataNoComm[1:5, c(1:15,41)]
long<-cnk %>% gather(QuesID, Response, QuesID.13:QuesID.28, na.rm = FALSE)

 
# s<-long %>% separate(QuesID, c(NA, "QuesID"))
# 
# s2<-s[41:45,]

# Create the LUT from Scoring
lut<-scoring2[22:252, c("MaturityVar", "QuesID", "label","PointValue")] 
val<-as.integer(lut$PointValue)
lut<-cbind(lut,val)

# Merge the long data with the LUT
l2<-merge(s,lut, by.x = c("Response","QuesID"), by.y = c("label", "QuesID"), all.x = TRUE)
long2<-l2[,-c()]
