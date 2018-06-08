# clean-02-UniteSurveyData.R

# PUrpose
# The purpose of this script is to merge response columns into 
# single columns.

# Prerequisites
# 1_load_FDIA.R     script that loads libraries, raw survey data, Framework MASTER table, etc.
# yesno.R           function developed to handle questions where 'Yes' responses occur in the first column
#                   and 'No' responses occur in the second column for a question
# surveyData        raw survey data loaded using `1_load_FDIA.R` Be aware that columns are ordered "V1", "V2",...
# surveyDataThm     corrected surveyData, developed using `cleanSurveyData.R`. Can be used as alternative to
#                   raw surveyData data frame. Be aware that columns are ordered "V11", "V1", "V2",... This re-
#                   ordering resulted from the merging of the raw data with Framework MASTER table            

# Futuer enhancements
#   1. update manipulation of factors for questions 13, 14, and 17-21 to be more efficient
#   
#   
#################################  

### Step 1. Source 1_load_FDIA.R
source("1_load_FDIA.R")
source("yesno.R")

x<-surveyData
mydata<- data.frame(rowID=c(1:length(x[,1])))
# mydata<-cbind(mydata,x[,c("V10","V11")])
# names(mydata)[2:3] <- c("DataElem", "ID")

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
### Note: this section was updated 6/4/2018. My understanding of factor 
### manipulation was better than when I started this script. IOW, the code
### for Question 16 is more efficient than for Ques. 13, 14, and 17-21.
q<-"QuesID.16"
i<-27  # Active
j<-28  # Static
k<-29  # Other (includes comments)

a<-factor(x[,i])
b<-factor(x[,j])
c<-as.character(x[,k])  #import as character to recode the comments contained in this field to the label/level "Other"
clog<-is.empty(c)
cc<-recode(as.character(clog), 'FALSE' = c(paste(scoring[scoring$VarID==k,"label"])), .default = "", .missing = NULL)

df<-cbind(x[,i:j],cc)
head(df)

str(df)
r<-unite(df, "QuesID.16", 1:3, sep="")
QuesID.16<-factor(r[,1])
levels(QuesID.16)[1]<-"missing" #this is the key to simplifying the recoding of factors.
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
source("impLongFactor.R")
mynames<-c()

myoutdata<-myfunc(mydata, x,"TestID.21", 38,43, mynames, scoring)
mydata<-cbind(mydata, myoutdata)

rm(myoutdata)

### QuesID = 22 (QID007) ###
### XLS Cols: AR-AX  Levels: "No process", "Inconsistent", "Planned", "Exists-inadequate", 
###                          "Exists-adequate", "Recurring", "Comments"
### VarIDs: 44-49; comments in VarID 50 (V50)

mynames<-c("rowID", "No process", "Inconsistent", "Planned", "Exists-inadequate", 
           "Exists-adequate", "Recurring")
myoutdata<-myfunc(mydata, x,"QuesID.22", 44,50, mynames, scoring)
mydata<-cbind(mydata, myoutdata)


### MERGE UNITED SURVEY DATA WITH THEMES & OTHER IDENTIFYING INFO ##

selcols<-c("V11", "V10") # ID and data element names
sel<-select(x, selcols)
# names(sel)[1:2]<-c("ID","DataElem")
mydata<-cbind(mydata,sel)
thms<-c("V11","V10","Theme", "")
mydata<-merge(mydata,surveyDataThm[,thms], by=c("V11","V10"), all.x = TRUE)
# names(mydata)[5:13]<-outnames

### Select some records from the merged survey data
as.matrix(tmydata[tmydata$QuesID.13=="No","V10"])
