# clean-part-01.R

# The purpose of this script is to merge response columns into 
# single columns.

# Step 1. Source 1_load_FDIA.R
source("1_load_FDIA.R")
source("yesno.R")

x<-surveyData
mydata<- data.frame(rowID=c(1:length(x[,1])))
mydata<-cbind(mydata,x[,c("V10","V11")])
names(mydata)[2:3] <- c("DataElem", "ID")

# Add names to the outlines object as code is developed
outnames<-c("13_Keep","14_NameIssue","16_Active","17_StatExtent","18_BrdUsers",
            "19_FDE","20_2FDE","21_RtThm","21_RtThmComments")

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
### NOT RUNNING CORRECTLY
### 
q<-"QuesID.16"
i<-27  # Active
j<-28  # Static
k<-29  # Other

a<-factor(x[,i])
b<-factor(x[,j])
c<-as.character(x[,k])
clog<-is.empty(c)
cc<-recode(as.character(clog), 'FALSE' = c(paste(scoring[scoring$VarID==k,"label"])), .default = "", .missing = NULL)

df<-cbind(x[,i:j],cc)
head(df)

str(df)
r<-unite(df, "QuesID.16", 1:3, sep="")
QuesID.16<-factor(r[,1])
levels(QuesID.16)[1]<-"missing"
str(QuesID.16)
summary(QuesID.16)

mydata<-cbind(mydata,QuesID.16)

rm(a,b,c,df,aa,bb,cc,r,alog,blog,clog,i,j,k,q)

source("yesno.R") # this function imports and cleans Yes/No questions (but not No/Yes questions)

### Question 17 (QID105) Levels: Yes, No (in this order)  VarIDs: 30-31 ###
mydata<-yesno(x,"QuesID.17", 30, 31)

### Question 18 (QID106) Levels: Yes, No (in this order)  VarIDs: 32-33 ###
mydata<-yesno(x,"QuesID.18", 32, 33)

### Question 19 (QID107) ###
### XLS Cols: AH-AI  Levels: Yes, No (in this order)  VarIDs: 34-35
mydata<-yesno(x,"QuesID.19", 34,35)


### Question 20 (QID108) ###
### XLS Cols: AJ-AK  Levels: Yes, No (in this order)  VarIDs: 36-37
mydata<-yesno(x,"QuesID.20", 36,37)


### QuesID = 21 (QID109) ###
# this question has 5 possible responses + open-ended text = 6 responses in total
# Add the returned values to the data frame "mydata" each time.
q<-"QuesID.21"
cols<-c(38:43)
source("impFactor.R")

df<-mydata[,1]
  for (i in cols)
    df<-data.frame(df,impFactor(x, scoring,i))
names(df)<-c("rowID", "Disagree", "Move", "CC-move", "CC-stay", "Stay","BetterTheme")
r<-unite(df,"QuesID.21",2:6, sep="")

rdf<-data.frame(factor(r[,"QuesID.21"])) # trash
names(rdf)<-q; 
summary(rdf[,1])

mylevels<-c("missing")
  for (i in cols)
    mylevels<-c(mylevels,paste(scoring[scoring$VarID==i,"label"]))
levels(rdf[,1])<-c(mylevels[1:length(cols)]); summary(rdf[,1])
QuesID.21<-rdf

QuesID.21.Comments<-data.frame(x[,cols[6]], stringsAsFactors = FALSE)
names(QuesID.21.Comments)<-paste(q,"Comments", sep=".")
mydata<-cbind(mydata, rdf, QuesID.21.Comments)

rm(q,cols,df,r,rdf)


### QuesID = 22 (QID007) ###
### XLS Cols: AR-AX  Levels: "No process", "Inconsistent", "Planned", "Exists-inadequate", "Exists-adequate", "Recurring", "Comments"
### VarIDs: 44-50

# Notes: should be able to turn Question 21 into a function that can be used for a variety of multiple choice questions



### MERGE UNITED SURVEY DATA WITH THEMES & OTHER IDENTIFYING INFO ##

selcols<-c("V11", "V10") # ID and data element names
sel<-select(x, selcols)
# names(sel)[1:2]<-c("ID","DataElem")
tmydata<-cbind(mydata,sel)
thms<-c("V11","V10","Theme")
tmydata<-merge(tmydata,surveyDataThm[,thms], by=c("V11","V10"), all.x = TRUE)
names(mydata)[5:13]<-outnames

### Select some records from the merged survey data
as.matrix(tmydata[tmydata$QuesID.13=="No","V10"])
