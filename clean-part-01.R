# clean-part-01.R

# The purpose of this script is to merge response columns into 
# single columns.

# Step 1. Source 1_load_FDIA.R
source("1_load_FDIA.R")

mydata<- data.frame(rowID=c(1:length(x[,1])))

### Question 13 (QID 112) (data_num version) levels = "No", "Yes" ###
q<-"QuesID.13"
i<-22
j<-23

a<-factor(x[,i], 
          levels = c(paste(scoring[scoring$VarID==i,"labelCode"])), 
          labels = c(paste(scoring[scoring$VarID==i,"label"]))); head(a); str(a)
alog<-is.empty(as.character(a))
aa<-recode(as.character(alog),'FALSE'="2", .default = "", .missing = NULL) #recode "No" from 1 to 2

b<-x[,j]
blog<-is.empty(as.character(b))
bb<-recode(as.character(blog),`FALSE` = "1", .default = "", .missing = NULL) #recode "Yes" from empty (FALSE) to 1

df<-data.frame(aa,bb); head(df)
r<-unite(df,paste(c(q)),1:2, sep="")

QuesID.13<-data.frame(factor(r[,1], labels=c("Yes","No"))) # Need to add in some code here to catch missing values.
names(QuesID.13)<-q
summary(QuesID.13[,1])
mydata<-cbind(mydata,QuesID.13)

rm(df,aa,bb,r,q,a,b, alog,blog)

### Question 14 (QID114) levels = "No", "Yes" ###
q<-"QuesID.14"
i<-24
j<-25
a<-x[,i]; head(a); str(a)
alog<-is.empty(as.character(a))
aa<-recode(as.character(alog),'FALSE'="2", .default = "", .missing = NULL) #recode "No" from 1 to 2
b<-x[,j]
blog<-is.empty(as.character(b))
bb<-recode(as.character(blog),'FALSE' = "1", .default = "", .missing = NULL) #recode "Yes" from empty (FALSE) to 1

df<-data.frame(aa,bb); head(df)
r<-unite(df,paste(c(q)),1:2, sep="")

QuesID.14<-data.frame(factor(r[,1]))
names(QuesID.14)<-q; str(QuesID.14)
summary(QuesID.14[,1])
mylevels<-c("missing",
            paste(scoring[scoring$VarID==j,"label"]),
            paste(scoring[scoring$VarID==i,"label"]))
levels(QuesID.14[,1])<-c(mylevels); summary(QuesID.14[,1])
mydata<-cbind(mydata,QuesID.14)

rm(df,aa,bb,r,q,a,b, alog,blog)

### Question 16 (QID006) Levels: Active, Static, Other  VarIDs: 27:29 ###
q<-"QuesID.16"
i<-27
j<-28
k<-29
a<-factor(x[,i], 
          levels = c(paste(scoring[scoring$VarID==i,"labelCode"])), 
          labels = c(paste(scoring[scoring$VarID==i,"label"]))); head(a); str(a)
# create logical object where nonempty values return FALSE & signify open-ended text
alog<-is.empty(as.character(a))
# recode logical using iterator rowID and corresponding labelCode (e.g., for VarID = 27: labelCode=1 and label="Active")
aa<-recode(as.character(alog), 'FALSE' = c(paste(scoring[scoring$VarID==i,"labelCode"])), .default = "", .missing = NULL) 

b<-factor(x[,j], 
          levels = c(paste(scoring[scoring$VarID==j,"labelCode"])), 
          labels = c(paste(scoring[scoring$VarID==j,"label"]))); head(b); str(b)
blog<-is.empty(as.character(b))
bb<-recode(as.character(blog), 'FALSE' = c(paste(scoring[scoring$VarID==j,"labelCode"])), .default = "", .missing = NULL)

c<-factor(x[,k], 
          levels = c(paste(scoring[scoring$VarID==k,"labelCode"])), 
          labels = c(paste(scoring[scoring$VarID==k,"label"]))); head(c); str(c)
clog<-is.empty(as.character(c))
cc<-recode(as.character(clog), 'FALSE' = c(paste(scoring[scoring$VarID==k,"labelCode"])), .default = "", .missing = NULL)

df<-data.frame(aa,bb,cc); df[1:10,]
r2<-unite(df,"QuesID.16",1:3, sep="")

QuesID.16<-data.frame(factor(r[,1]))
names(QuesID.16)<-q
# pull the levels from the Scoring table
mylevels<-c("missing",
            paste(scoring[scoring$VarID==i,"label"]),
            paste(scoring[scoring$VarID==j,"label"]),
            paste(scoring[scoring$VarID==k,"label"]))
mylevels
levels(QuesID.16[,1])<-c(mylevels); summary(QuesID.16[,1])
mydata<-cbind(mydata,QuesID.16)

rm(a,b,c,df,aa,bb,cc,r,alog,blog,clog,i,j,k,q)

### Question 17 (QID105) Levels: Yes, No (in this order)  VarIDs: 30-31 ###
q<-"QuesID.17"
i<-30
j<-31

a<-x[,i]; head(a)
# create logical object where nonempty values return FALSE & signify response = "No"
alog<-is.empty(as.character(a))
aa<-recode(as.character(alog),'FALSE'="1", .default = "", .missing = NULL) #"recode "Yes" responses
b<-x[,j]; head(b)
blog<-is.empty(as.character(b))
bb<-recode(as.character(blog),'FALSE' = "2", .default = "", .missing = NULL) #recode "No" responses

df<-data.frame(aa,bb); head(df)
r<-unite(df,paste(c(q)),1:2, sep=""); 

QuesID.17<-data.frame(factor(r[,1]))
names(QuesID.17)<-q
levels(QuesID.17[,1])<-c("missing","Yes","No")
head(QuesID.17)
mydata<-cbind(mydata,QuesID.17)

rm(a,b, df,aa,bb,r,alog,blog,i,j,q)

### Question 18 (QID106) Levels: Yes, No (in this order)  VarIDs: 32-33 ###
q<-"QuesID.18"
i<-32
j<-33

a<-x[,i]; head(a)
# create logical object where nonempty values return FALSE & signify response = "No"
alog<-is.empty(as.character(a))
aa<-recode(as.character(alog),'FALSE'="1", .default = "", .missing = NULL) #"recode "Yes" responses
b<-x[,j]; head(b)
blog<-is.empty(as.character(b))
bb<-recode(as.character(blog),'FALSE' = "2", .default = "", .missing = NULL) #recode "No" responses

df<-data.frame(aa,bb); head(df)
r<-unite(df,paste(c(q)),1:2, sep="")

QuesID.18<-data.frame(factor(r[,1]))
names(QuesID.18)<-q; 
levels(QuesID.18[,1])<-c("missing","Yes","No")
summary(QuesID.18)
mydata<-cbind(mydata,QuesID.18)

rm(a,b, df,aa,bb,r,alog,blog,i,j,q)

### Question 19 (QID107) ###
### XLS Cols: AH-AI  Levels: Yes, No (in this order)  VarIDs: 34-35
q<-"QuesID.19"
i<-34
j<-35

a<-x[,i]; head(a)
# create logical object where nonempty values return FALSE & signify response = "No"
alog<-is.empty(as.character(a))
aa<-recode(as.character(alog),'FALSE'="1", .default = "", .missing = NULL) #"recode "Yes" responses
b<-x[,j]; head(b)
blog<-is.empty(as.character(b))
bb<-recode(as.character(blog),'FALSE' = "2", .default = "", .missing = NULL) #recode "No" responses

df<-data.frame(aa,bb); head(df)
r<-unite(df,paste(c(q)),1:2, sep="")

QuesID.19<-data.frame(factor(r[,1]))
names(QuesID.19)<-q; 
levels(QuesID.19[,1])<-c("missing","Yes","No")
summary(QuesID.19)

mydata<-cbind(mydata,QuesID.19)

rm(a,b, df,aa,bb,r,alog,blog,i,j,q)

### Question 20 (QID108) ###
### XLS Cols: AJ-AK  Levels: Yes, No (in this order)  VarIDs: 36-37
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
