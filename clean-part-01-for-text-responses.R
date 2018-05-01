# clean-part-01-for-text-responses.R

# The purpose of this script is to merge response columns into 
# single columns.

# Step 1. Source 1_load_FDIA.R
source("1_load_FDIA.R")

### Question 13 (QID 112) (data_text version) ###
q<-"QuesID.13"
a<-x[,22]; head(a)
b<-is.empty(as.character(x[,23])); str(b); head(b)
# bb<-recode(as.character(b),`FALSE` = "Yes", .default = "")
bb<-recode(as.character(b),`FALSE` = "Yes", .default = "")
df<-data.frame(a,bb); head(df)
r<-unite(df,"QuesID.13",1:2, sep="")
QuesID.13<-data.frame(factor(r[,1]))
names(QuesID.13)<-q
# levels(QuesID.13[,1])<-c("missing","No","Yes")
summary(QuesID.13[,1])
rm(df,bb,r,q,a,b)

### Question 14 (QID114) ###
q<-"QuesID.14"
a<-x[,24]; head(a)
b<-is.empty(as.character(x[,25])); head(b)
bb<-recode(as.character(b),`FALSE` = "Yes", .default = "")
df<-data.frame(a,bb); head(df)
r<-unite(df,"QuesID.14",1:2, sep="")
QuesID.14<-data.frame(factor(r[,1]))
names(QuesID.14)<-q
levels(QuesID.14[,1])<-c("missing","No","Yes")
summary(QuesID.14[,1])
rm(df,bb,r)

### Question 16 (QID006) Levels: Active, Static, Other ###
q<-"QuesID.16"
a<-x[,27]; head(a)
b<-x[,28]; head(b)
# create logical object where nonempty values return FALSE & signify open-ended text
c<-is.empty(as.character(x[,29])); str(c)
cc<-recode(as.character(c),`FALSE` = "Other", .default = "")
df<-data.frame(a,b,cc); df[1:10,]
r<-unite(df,"QuesID.16",1:3, sep="")
QuesID.16<-data.frame(factor(r[,1]))
names(QuesID.16)<-q
levels(QuesID.16[,1])<-c("missing", "Active", "Other","Static"); summary(QuesID.16[,1])
rm(df,c,cc,r)

### Question 17 (QID105) ###
q<-"QuesID.17"
a<-x[,30]; head(a)
b<-x[,31]; head(b)
df<-data.frame(a,b); head(df)
r<-unite(df,"QuesID.17",1:2, sep=""); 
QuesID.17<-data.frame(factor(r[,1]))
names(QuesID.17)<-q
levels(QuesID.17[,1])<-c("missing","No","Yes")
head(QuesID.17)
rm(df)

### Question 18 (QID106) ###
q<-"QuesID.18"
a<-x[,32]; head(a)
# create logical object where nonempty values return FALSE & signify response = "No"
b<-is.empty(as.character(x[,33])); head(b)
bb<-recode(as.character(b),`FALSE` = "No", .default = "")
df<-data.frame(a,bb); head(df)
r<-unite(df,"QuesID.18",1:2, sep="")
QuesID.18<-data.frame(factor(r[,1]))
names(QuesID.18)<-q; 
levels(QuesID.18[,1])<-c("missing","No","Yes")
head(QuesID.18)
rm(df,bb,r)

### Question 19 (QID107) ###
q<-"QuesID.19"
a<-x[,34]; head(a)
# create logical object where nonempty values return FALSE & signify response = "No"
b<-is.empty(as.character(x[,35])); head(b)
bb<-recode(as.character(b),`FALSE` = "No", .default = "")
df<-data.frame(a,bb); head(df)
r<-unite(df,"QuesID.19",1:2, sep="")
QuesID.19<-data.frame(factor(r[,1]))
names(QuesID.19)<-q; summary(QuesID.19[,1])
levels(QuesID.19[,1])<-c("missing","No","Yes"); summary(QuesID.19[,1])
head(QuesID.19)
rm(df,bb,r)

### Question 20 (QID108) ###
q<-"QuesID.20"
a<-x[,36]; head(a)
# create logical object where nonempty values return FALSE & signify response = "No"
b<-is.empty(as.character(x[,37])); head(b)
bb<-recode(as.character(b),`FALSE` = "No", .default = "")
df<-data.frame(a,bb); head(df)
r<-unite(df,"resp",1:2, sep="")
rdf<-data.frame(factor(r[,1]))
names(rdf)<-q; summary(rdf[,1])
levels(rdf[,1])<-c("missing","No","Yes"); summary(rdf[,1])
QuesID.20<-rdf
head(QuesID.20)
rm(df,bb,r,rdf)

unitedQs<-cbind(QuesID.13,QuesID.14, QuesID.16, QuesID.17,QuesID.18,QuesID.19,QuesID.20)
#rm(QuesID.13,QuesID.14,QuesID.15, QuesID.16, QuesID.17,QuesID.18,QuesID.19,QuesID.20)

### QuesID = 21 (QID109) ###
# this question has 5 possible responses + open-ended text = 6 responses in total
# Add the returned values to the data frame "unitedQs" each time.
q<-"QuesID.21"
cols<-c(38:43)
# create logical object where nonempty values return FALSE & signify response equals the 
# appropriate value OR figure out how to recode the responses at the end
a<-as.character(x[,cols[1]]); head(a)
a<-is.empty(a); head(a)

b<-as.character(x[,cols[2]]) 
b<-is.empty(b); head(b)

df<-data.frame(a,bb); head(df)
r<-unite(df,"resp",1:2, sep="")
rdf<-data.frame(factor(r[,1]))
names(rdf)<-q; summary(rdf[,1])
levels(rdf[,1])<-c("missing","No","Yes"); summary(rdf[,1])
QuesID.20<-rdf
head(QuesID.20)
rm(df,bb,r,rdf)