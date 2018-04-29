# clean-part-01.R

# The purpose of this script is to merge response columns into 
# single columns.

library(tidyr)

# Step 1.

# a<-head(x[,22])
# a
# b<-as.character(head(x[,23]))
# df<-data.frame(a,b)
# df
# c<-unite(df,"united",1:2)
# c
# df<-cbind(df,c)
# df

# Question 13
q<-"QuesID.13"
a<-x[,22]; head(a)
b<-as.character(x[,23]); head(b)
df<-data.frame(a,b); head(df)
QuesID.13<-unite(df,"QuesID.13",1:2); head(QuesID.13)
rm(df)

# Question 14
q<-"QuesID.14"
a<-x[,24]; head(a)
b<-as.character(x[,25]); head(b)
df<-data.frame(a,b); head(df)
QuesID.14<-unite(df,"QuesID.14",1:2); head(QuesID.14)
rm(df)

# Question 15
q<-"QuesID.15"
a<-x[,27]; head(a)
b<-as.character(x[,28]); head(b)
df<-data.frame(a,b); head(df)
QuesID.15<-unite(df,"QuesID.15",1:2, sep=""); head(QuesID.15)
rm(df)
