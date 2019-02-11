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

# Future enhancements
#   1. update manipulation of factors for questions 13, 14, and 17-21 to be more efficient
#   
#   
#################################  

### Step 1. Source 1_load_FDIA.R and functions
source("1_load_FDIA.R")  # loads packages only
source("yesno.R")
source("FDIA-functions.R")

# Add names to the outlines object as code is developed
outnames<-c("13_Keep","14_NameIssue","16_Active","17_StatExtent","18_BrdUsers",
            "19_FDE","20_2FDE","21_RtThm","21_RtThmComments")

# Response sets (response names)
nonetofullimplNames<-c("rowID", "None", "Under development", "Initiated", "Progressing", 
                              "Well established", "Fully implemented")
fgdcOther<-c("rowID", "FGDC", "other")
resources<-c("rowID", "Desired", "Planned-no resrcs", "Planned-resrcs avail", "In progress-partial resrcs", 
              "In progress-full resrcs", "Fully implemented", "NA")
processScale<-c("rowID", "No process", "Ad-hoc", "Repeatable", "Defined", "Managed", "Optimized")


# Retrieve data
x<-surveyDataThm
mydata<- data.frame(rowID=c(1:length(x[,1])))
# mydata<-cbind(mydata,x[,c("V10","V11")])
# names(mydata)[2:3] <- c("DataElem", "ID")
# 
# 

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

rm(df,aa,bb,r,q,a,b,alog,blog)

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

rm(a,b,c,df,aa,bb,cc,r,clog,i,j,k,q)

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
# cols<-c(38:43) 
source("FDIA-functions.R")
mynames<-c("rowID", "Disagree", "Move", "CC-move","CC-stay","Stay")

myoutdata<-impLongFactWComm(mydata, x,"QuesID.21", 38,43, mynames, scoring)
mydata<-cbind(mydata, myoutdata)

rm(myoutdata)

### QuesID = 22 (QID007) ###
### XLS Cols: AR-AX  Levels: "No process", "Inconsistent", "Planned", "Exists-inadequate", 
###                          "Exists-adequate", "Recurring", "Comments"
### VarIDs: 44-49; comments in VarID 50 (V50)
 
mynames<-c("rowID", "No process", "Inconsistent", "Planned", "Exists-inadequate", 
           "Exists-adequate", "Recurring")
myoutdata<-impLongFactWComm(mydata, x,"QuesID.22", 44,50, mynames, scoring)
mydata<-cbind(mydata, myoutdata)

rm(myoutdata)

### QuesID = 26 (QID010) ###
### XLS Cols: CC-DH  Levels: "No process", "Under development", "Initiated", "Progressing", 
###                          "Well established", "Fully implemented"
### VarIDs: 81-86; no comments field

mynames<-nonetofullimplNames

myoutdata<-impLongFactor(mydata, x,"QuesID.26", 81,86, mynames, scoring)
mydata<-cbind(mydata, myoutdata)


### QuesID = 27 (QID012) ###
### XLS Cols: CI-CN  Levels: "No information", "Best available", "1-25%", "25-50%", 
###                           "50-75%", "75-100%"
### VarIDs: 87-92; no comments field

mynames<-c("rowID", "No information", "Best available", "1-25%", "25-50%", 
           "50-75%", "75-100%")

myoutdata<-impLongFactor(mydata, x,"QuesID.27", 87, 92, mynames, scoring)
mydata<-cbind(mydata, myoutdata)
rm(myoutdata)

### QuesID = 28 (QID012) ###
### XLS Cols: CO-CU  Levels: "Desired", "Planned-no resrcs", "Planned-resrcs avail", "In progress-partial resrcs", 
###                           "In progress-full resrcs", "Fully implemented", "NA"
### no comments field
### VarIDS:
varIDstr<-93
varIDend<-99
 
mynames<-resources

myoutdata<-impLongFactor(mydata, x,"QuesID.28", varIDstr, varIDend, mynames, scoring)
mydata<-cbind(mydata, myoutdata)
rm(myoutdata)

### QuesID = 30 (QID012) ###
### XLS Cols: Cy-CZ  Levels: "FGDC", "other"
### no comments field
### VarIDS:
varIDstr<-102
varIDend<-103
ques<-c("QuesID.30")

mynames<-fgdcOther

myoutdata<-impLongFactor(mydata, x,ques, varIDstr, varIDend, mynames, scoring)
mydata<-cbind(mydata, myoutdata)
rm(myoutdata)

### QuesID = 31 (QID011) ###
### XLS Cols: CZ-DE  Levels: "None", "Under development", "Initiated", "Progressing", 
###                          "Well established", "Fully implemented"
### no comments field
### VarIDs:
varIDstr<-104
varIDend<-109
ques<-c("QuesID.31")

mynames<-nonetofullimplNames

myoutdata<-impLongFactor(mydata, x,ques, varIDstr, varIDend, mynames, scoring)
mydata<-cbind(mydata, myoutdata)
rm(myoutdata)

### QuesID = 32 (QID012) ###
### XLS Cols: DF-DH  Levels: "Internal", "External", "other"
### no comments field
### VarIDs:
varIDstr<-110
varIDend<-112
ques<-c("QuesID.32")

mynames<-c("rowID", "Internal", "External", "other")

myoutdata<-impLongFactor(mydata, x,ques, varIDstr, varIDend, mynames, scoring)
mydata<-cbind(mydata, myoutdata)
rm(myoutdata)

### QuesID = 33 (QID013) ###
### XLS Cols: DI-DO  Levels: "None", "Under development", "Initiated", "Progressing", 
###                          "Well established", "Fully implemented"
### Has comments field
### VarIDs:
varIDstr<-113
varIDend<-119
ques<-c("QuesID.33")

mynames<-nonetofullimplNames

myoutdata<-impLongFactWComm(mydata, x,ques, varIDstr, varIDend, mynames, scoring)
mydata<-cbind(mydata, myoutdata)
rm(myoutdata)

### QuesID = 34 (QID101) ###
### XLS Cols: DP-DU  Levels: "Desired", "Planned-no resrcs", "Planned-resrcs avail", "In progress-partial resrcs", 
###                           "In progress-full resrcs", "Fully implemented"
### no comments field
### VarIDS:
varIDstr<-120
varIDend<-125
ques<-c("QuesID.34")

mynames<-resources[-8]

myoutdata<-impLongFactor(mydata, x, ques, varIDstr, varIDend, mynames, scoring)
mydata<-cbind(mydata, myoutdata)
rm(myoutdata)

### QuesID = 35 (QID016) ###
### XLS Cols: DV-DZ  Levels: "None", "Under development", "Initiated", "Progressing", 
###                          "Well established", "Fully implemented"
### No comments field
### VarIDs:
varIDstr<-126
varIDend<-131
ques<-c("QuesID.35")

mynames<-nonetofullimplNames

myoutdata<-impLongFactor(mydata, x,ques, varIDstr, varIDend, mynames, scoring)
mydata<-cbind(mydata, myoutdata)
rm(myoutdata)

### QuesID = 37 (QID018) ###
### XLS Cols: EI-EN  Levels: "None", "Under development", "Initiated", "Progressing", 
###                          "Well established", "Fully implemented"
### No comments field
### VarIDs:
varIDstr<-139
varIDend<-144
ques<-c("QuesID.37")

mynames<-nonetofullimplNames

myoutdata<-impLongFactor(mydata, x,ques, varIDstr, varIDend, mynames, scoring)
mydata<-cbind(mydata, myoutdata)
rm(myoutdata)

### QuesID = 38 (QID019) Optional Question ###
### XLS Cols: Eo-ES  Levels: "Purchase", "Alter existing", "Share-aggregate existing", "Create-collect"
###                   
### Has comments field contained in an "Other" choice that imported in a second step.
### VarIDs:
varIDstr<-145
varIDend<-149
ques<-c("QuesID.38")

mynames<-c("rowID","Purchase", "Alter existing", "Share-aggregate existing", "Create-collect", "Other")

myoutdata<-impLongFactor(mydata, x,ques, varIDstr, varIDend, mynames, scoring) #Levels are incorrect due to the "other" = 0 value
mylevels<-c("missing",mynames[6],mynames[2:5]) # Correcting the levels here
levels(myoutdata$QuesID.38)<-mylevels

mydata<-cbind(mydata, myoutdata)

cols<-c(varIDstr:varIDend)
comments<-data.frame(x[,cols[length(cols)]], stringsAsFactors = FALSE)
names(comments)<-c(paste(ques,"Comments",sep="."))
mydata<-cbind(mydata,comments)

rm(myoutdata, mylevels, cols, comments) 

### QuesID = 39 (QID020) ###
### XLS Cols: ET-EV  Levels: "Not OGIC", "Under development", "Fully implemented"
### No comments field
### VarIDs:
varIDstr<-150
varIDend<-152
ques<-c("QuesID.39")

mynames<-c("rowID", "Not OGIC", "Under development", "Fully implemented")

myoutdata<-impLongFactor(mydata, x,ques, varIDstr, varIDend, mynames, scoring)
mydata<-cbind(mydata, myoutdata)
rm(myoutdata) 

### QuesID = 40 (QID021) ###
### XLS Cols: EW-FB  Levels: "0%", "1 to <25%", "25 to <50%", "50 to <75%", "75 to <100%", "Fully complete"
### No comments field
### VarIDs:
varIDstr<-153
varIDend<-158
ques<-c("QuesID.40")

mynames<-c("rowID", "0%", "1 to <25%", "25 to <50%", "50 to <75%", "75 to <100%", "Fully complete")

myoutdata<-impLongFactor(mydata, x,ques, varIDstr, varIDend, mynames, scoring)
mydata<-cbind(mydata, myoutdata)
rm(myoutdata) 

### QuesID = 41 (QID022) Optional Question ###
### XLS Cols: FC-FI  Levels: "Global", "US 50 states", "Oregon (all)", "Oregon (specific extents)",
###                           "Oregon coast", "Oregon waterbodies", "Other"
###                   
### Has comments field contained in an "Other" choice that imported in a second step.
### VarIDs:
varIDstr<-159
varIDend<-165
ques<-c("QuesID.41")

mynames<-c("rowID", "Global", "US 50 states", "Oregon (all)", "Oregon (specific extents)", 
           "Oregon coast", "Oregon waterbodies", "Other")

myoutdata<-impLongFactor(mydata, x,ques, varIDstr, varIDend, mynames, scoring) #Levels are incorrect due to the "other" = 0 value
mylevels<-c("missing",mynames[length(mynames)],mynames[2:length(mynames)]) # Correcting the levels here
levels(myoutdata[,1])<-mylevels

mydata<-cbind(mydata, myoutdata)

cols<-c(varIDstr:varIDend)
comments<-data.frame(x[,cols[length(cols)]], stringsAsFactors = FALSE)
names(comments)<-c(paste(ques,"Comments",sep="."))
mydata<-cbind(mydata,comments)

rm(myoutdata, mylevels, cols, comments) 

### QuesID = 42 (QID023) ###
### XLS Cols: FJ-FO
### Levels: "0%", "1 to <25%", "25 to <50%", "50 to <75%", "75 to <100%", "Fully complete"
### No comments field
### VarIDs:
varIDstr<-166
varIDend<-171
ques<-c("QuesID.42")

mynames<-c("rowID", "0%", "1 to <25%", "25 to <50%", "50 to <75%", "75 to <100%", "Fully complete")

myoutdata<-impLongFactor(mydata, x,ques, varIDstr, varIDend, mynames, scoring)
mydata<-cbind(mydata, myoutdata)
rm(myoutdata) 

### QuesID = 43 (QID004) ###
### XLS Cols: FP-FR
### Levels: "Not shared", "Secured", "Public"
### No comments field
### VarIDs:
varIDstr<-172
varIDend<-174
ques<-c("QuesID.43")

mynames<-c("rowID", "Not shared", "Secured", "Public")

myoutdata<-impLongFactor(mydata, x,ques, varIDstr, varIDend, mynames, scoring)
mydata<-cbind(mydata, myoutdata)
rm(myoutdata)

### QuesID = 44 (QID025) Access to data element via OSDL ###
### XLS Cols: FS-FY  
### Levels: "Desired", "Planned-no resrcs", "Planned-resrcs avail", "In progress-partial resrcs",
###         "In progress-full resrcs", "Fully implemented", "Not Applicable"
### Has comments field (use impLongFactWComm function)
### VarIDs:
varIDstr<-175
varIDend<-182
ques<-c("QuesID.44")

mynames<-resources

myoutdata<-impLongFactWComm(mydata, x,ques, varIDstr, varIDend, mynames, scoring) #Levels are incorrect due to the absence of 
                                                                               # level 5 values ("In progress-full resrcs")
                                                                               # Future enhancement: use ordered factor to ensure
                                                                               # proper order maintained relative to other data (maybe)
# Correcting the levels here
mylevels<-c("missing","Desired", "Planned-no resrcs", "Planned-resrcs avail", "In progress-partial resrcs",
            "Fully implemented", "Not Applicable", "In progress-full resrcs") 
levels(myoutdata[,1])<-mylevels
myoutdata[[1]]<-factor(myoutdata[[1]], 
                       ordered = TRUE, 
                       levels = c("missing","Not Applicable","Desired", "Planned-no resrcs", "Planned-resrcs avail", 
                                  "In progress-partial resrcs","In progress-full resrcs","Fully implemented"))
# myoutdata[[1]]<-recode_factor(myoutdata[[1]], 'missing' = NA_character_)   # Future enhancement: recode "missings" to NA_character_

mydata<-cbind(mydata, myoutdata)
rm(myoutdata, mylevels) 


### Question 45 (QID025.2) Levels: Yes, No (in this order)  VarIDs: 183-184 ###
mydata<-yesno(x,"QuesID.45", 183, 184)


### QuesID = 46 (QID025.1) Process for accessing the data element ###
### XLS Cols: GC-GI  
### Levels: "rowID", "No process", "Ad-hoc", "Repeatable", "Defined", "Managed", "Optimized"
### Has comments field 
### VarIDs:
varIDstr<-185
varIDend<-191
ques<-c("QuesID.46")

mynames<-processScale

myoutdata<-impLongFactWComm(mydata, x,ques, varIDstr, varIDend, mynames, scoring) 
mydata<-cbind(mydata, myoutdata)

### QuesID = 47 (QID026) Stewardship Process status ###
### XLS Cols: GC-GI  
### Levels: "rowID", "Desired", "Planned-no resrcs", "Planned-resrcs avail", 
###         "In progress-partial resrcs", "In progress-full resrcs", 
###         "Fully implemented"
### Comments field: Comments are in the "Fully implemented" column
### VarIDs:
varIDstr<-192
varIDend<-197
ques<-c("QuesID.47")

mynames<-resources[-length(resources)]

myoutdata<-impLongFactor(mydata, x,ques, varIDstr, varIDend, mynames, scoring) 
mydata<-cbind(mydata, myoutdata)

cols<-c(varIDstr:varIDend)
comments<-data.frame(x[,cols[length(cols)]], stringsAsFactors = FALSE)
names(comments)<-c(paste(ques,"Comments",sep="."))
mydata<-cbind(mydata,comments)

rm(myoutdata, mylevels, cols, comments) 


### QuesID = 48 (QID027) Existing Stewardship Process ###
### XLS Cols: GP-GV  
### Levels: "rowID", "ad-hoc", "single", "two", "more than two", "informal", "formal", "other"
### No comments field
### VarIDs:
varIDstr<-198
varIDend<-204
ques<-c("QuesID.48")

mynames<-c("rowID", "ad-hoc", "single", "two", "more than two", "informal", "formal", "other")

myoutdata<-impLongFactor(mydata, x,ques, varIDstr, varIDend, mynames, scoring) 
mydata<-cbind(mydata, myoutdata)


### QuesID = 49 (QID028) QA/QC process ###
### XLS Cols: GW-HB  
### Levels: "rowID", "None", "Under development", "Initiated", "Progressing",
###         "Well established", "Fully implemented"
### No comments field
### VarIDs:
varIDstr<-205
varIDend<-210
ques<-c("QuesID.49")

mynames<-nonetofullimplNames

myoutdata<-impLongFactor(mydata, x,ques, varIDstr, varIDend, mynames, scoring) 
mydata<-cbind(mydata, myoutdata)

### QuesID = 50 (QID103) Horizontal integration procedures ###
### XLS Cols: HC-HI  
### Levels: "rowID", "No process", "Ad-hoc", "Repeatable", "Defined", "Managed", "Optimized"
### Has comments field 
### VarIDs:
varIDstr<-211
varIDend<-217
ques<-c("QuesID.50")

mynames<-processScale

myoutdata<-impLongFactWComm(mydata, x,ques, varIDstr, varIDend, mynames, scoring) 
mydata<-cbind(mydata, myoutdata)

### QuesID = 51 (QID104) Vertical integration procedures ###
### XLS Cols: HC-HI  
### Levels: "rowID", "No process", "Ad-hoc", "Repeatable", "Defined", "Managed", "Optimized"
### Has comments field 
### Note: V224 contains only the label for the question and is useless, so I omit it to create
###       "y" data set. The varIDend indicates true column number, so is V225-1 = 224.
### VarIDs:
varIDstr<-218
varIDend<-224

y<-x[,-224]

ques<-c("QuesID.51")

mynames<-processScale

myoutdata<-impLongFactWComm(mydata, y,ques, varIDstr, varIDend, mynames, scoring) 
mydata<-cbind(mydata, myoutdata)

rm(y)

### QuesID = 52 (QID029) Process for meeting user needs ###
### XLS Cols: HR-HX  
### Levels: "rowID", "None", "Under development", "Initiated", "Progressing",
###         "Well established", "Fully implemented"
### Has comments field 
### VarIDs:
varIDstr<-226
varIDend<-232

ques<-c("QuesID.52")

mynames<-nonetofullimplNames

myoutdata<-impLongFactWComm(mydata, x,ques, varIDstr, varIDend, mynames, scoring) 
mydata<-cbind(mydata, myoutdata)

### QuesID = 53 (QID030) Process for helping users evaluate and use data ###
### XLS Cols: HY-IE  
### Levels: "rowID", "None", "Under development", "Initiated", "Progressing",
###         "Well established", "Fully implemented"
### Has comments field 
### VarIDs:
varIDstr<-233
varIDend<-239

ques<-c("QuesID.53")

mynames<-nonetofullimplNames

myoutdata<-impLongFactWComm(mydata, x,ques, varIDstr, varIDend, mynames, scoring) 
mydata<-cbind(mydata, myoutdata)

### QuesID = 54 (QID031) Appraising process (optional) ###
### XLS Cols: IF-IL  
### Levels: "rowID", "None", "Under development", "Initiated", "Progressing",
###         "Well established", "Fully implemented"
### Has comments field 
### VarIDs:
varIDstr<-240
varIDend<-246

ques<-c("QuesID.54")

mynames<-nonetofullimplNames

myoutdata<-impLongFactWComm(mydata, x,ques, varIDstr, varIDend, mynames, scoring) 
mydata<-cbind(mydata, myoutdata)

### QuesID = 55 (QID032) Archived data location (optional) ###
### XLS Cols: IM-IR  
### Levels: "rowID", "No data", "Archived", "Local", "Other", "Not applicable"
### Has comments field 
### VarIDs:
varIDstr<-247
varIDend<-252

ques<-c("QuesID.55")

mynames<-c("rowID", "No data", "Archived", "Local", "Other", "Not applicable")

myoutdata<-impLongFactWComm(mydata, x,ques, varIDstr, varIDend, mynames, scoring) 
mydata<-cbind(mydata, myoutdata)


### MERGE UNITED SURVEY DATA WITH THEMES & OTHER IDENTIFYING INFO ##

selcols<-c("ID", "V10") # ID and data element names
sel<-select(x, selcols)
# names(sel)[1:2]<-c("ID","DataElem")
mydata<-cbind(sel,mydata)
thms<-c("ID","V10","Theme", "Element")
mydata<-merge(mydata,surveyDataThm[,thms], by=c("ID","V10"), all.x = TRUE)
# names(mydata)[5:13]<-outnames

### Select some records from the merged survey data
as.matrix(mydata[mydata$QuesID.13=="No","V10"])
as.matrix(mydata[mydata$Theme,"V10"])

rm(q,ques,mynames,outnames,processScale,resources,selcols,newlevels, 
   nonetofullimplNames,outfile,myfile,thms,varIDend,varIDstr, 
   varNames, varNames2, QuesID.13, QuesID.14, QuesID.15, QuesID.16, 
   QuesID.17,QuesID.18, QuesID.19, QuesID.20, QuesID.21)

