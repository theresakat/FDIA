# cnk<-mydataNoComm[1:5, c(1:15,41)]
# long<-cnk %>% gather(QuesID, Response, QuesID.13:QuesID.28, na.rm = FALSE)

cnk3<-mydataNoComm[1:20, c(1:15,41)]

MatQs <- paste0(rep("QuesID."), select_vect, collapse = ", " )

long <- mydataNoComm[,-42] %>% gather(QuesID, Response, MatQs, na.rm = FALSE)
 
s<-long %>% separate(QuesID, c(NA, "QuesID"))
# 
# s2<-s[41:45,]

# Create the LUT from Scoring
lut<-scoring[22:252, c("MaturityVar", "QuesID", "shortLabel",as.integer("PointValue"))] 
val<-as.integer(lut$PointValue)
lut<-cbind(lut,val)

# Merge the long data with the LUT
l2<-merge(s,lut, by.x = c("Response","QuesID"), by.y = c("Response", "QuesID"), all.x = TRUE)


# Subset and calculate the summaries by theme
longVals <- l2[l2$MaturityVar == 1,]
cdata<-ddply(longVals, c("V10", "Theme"), summarize,
             N = sum(!is.na(val)),
             mean = mean(!is.na(val)))
