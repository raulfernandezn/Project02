rm(list=ls())
setwd("Define/a/directory")
#Libraries
library(readxl)
library(xlsx)
library(zoo)
library(ggplot2)
library(scales)
library(reshape2)
library(dbscan)
library(stringi)
library(stringr)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidytext)
library(scales)
library(tidyr)
library(tm)
library(knitr)
library(e1071)
library(pROC)
library(hydroGOF)
#Data
Data <- read_excel("Data .xlsx",1)
#Dictionary data
#Specification
D.Spec <- read.xlsx("synth-hospital-codebook.xlsx",sheetName = "Fake_Spec",startRow = 3,header = F)
D.Spec$X1 <- trimws(D.Spec$X1)
D.Spec$X2 <- trimws(D.Spec$X2)
#Join Specification
Data$fake_spec <- trimws(Data$fake_spec)
Data$Var.Spec <- D.Spec[match(Data$fake_spec,D.Spec$X1),"X2"]
#Diagnostic
D.Diag <- as.data.frame(read_xlsx("synth-hospital-codebook.xlsx","Fake_Diag"))
names(D.Diag) <- D.Diag[1,]
D.Diag <- D.Diag[2:dim(D.Diag)[1],]
D.Diag$ICD10code <- trimws(D.Diag$ICD10code)
D.Diag$Diagnosisname <- trimws(D.Diag$Diagnosisname)
#Join Diagnostic
Data$fake_shortdiag <- trimws(Data$fake_shortdiag)
Data$Var.Diagnostic <- D.Diag[match(Data$fake_shortdiag,D.Diag$ICD10code),"Diagnosisname"]
#Marital status
Marital.DF <- data.frame(v1=c(1,2,3,8,9),v2=c("Never married","Married","Widowed","Other","Not known"),
                         stringsAsFactors = F)
#Join marital status
Data$Var.Marital <- Marital.DF[match(Data$fake_marital,Marital.DF$v1),"v2"]
#Significant facilities
D.Sigfac <- read.xlsx("synth-hospital-codebook.xlsx","Fake_Sigfac",startRow = 3,header = F)
D.Sigfac$X1 <- trimws(D.Sigfac$X1)
D.Sigfac$X2 <- trimws(D.Sigfac$X2)
#Join Significant facilities
Data$fake_sigfac <- trimws(Data$fake_sigfac)
Data$Var.Sigfac <- D.Sigfac[match(Data$fake_sigfac,D.Sigfac$X1),"X2"]
#Fake wait
D.Fake.Wait <- data.frame(v1=c(1,2,8,9),v2=c("True waiting list","Planned repeat waiting list",
                                             "Not on waiting list","Not known"),stringsAsFactors = F)
#Join wait
Data$fake_wait <- trimws(Data$fake_wait)
Data$Var.Wait <- D.Fake.Wait[match(Data$fake_wait,D.Fake.Wait$v1),"v2"]
#Fake admission
D.Admission <- read.xlsx("synth-hospital-codebook.xlsx","Fake_Admission",startRow = 3,header = F)
D.Admission$X3 <- NA
D.Admission$X3 <- ifelse(is.na(D.Admission$X1),as.character(D.Admission$X2),D.Admission$X3)
D.Admission$X2 <- as.character(D.Admission$X2)
D.Admission$X3 <- na.locf(D.Admission$X3)
D.Admission <- D.Admission[!is.na(D.Admission$X1),]
names(D.Admission)[c(2,3)] <- c("Var.Admission","Var.Admission.Group")
#Join admission
Data <- merge(Data,D.Admission,by.x = "fake_admission", by.y = "X1",all.x = T,sort = F)
#Gender
Data$Var.Gender <- ifelse(Data$fake_sex==1,"Male","Female")
#Grouping vars
PData <- Data[,c(3,4,5,6,11,14,15:23)]
#Structure for separating text mining approach
TextBase <- PData[,c(7,9)]
names(TextBase)[2] <- "text"
#Clean
clean=function(DF)
{
  #Remove points
  DF$text=gsub(".","",DF$text,fixed=T)
  #Remove commas
  DF$text=gsub(",","",DF$text,fixed=T)
  #Remove tabulation symbol \r
  DF$text=gsub("\r","",DF$text,fixed=T)
  #Remove tabulation symbol \n
  DF$text=gsub("\n","",DF$text,fixed=T)
  #Remove backslash
  DF$text=gsub("\\","",DF$text,fixed=T)
  #Remove parentheses
  DF$text=gsub("[(]","",DF$text)
  DF$text=gsub("[)]","",DF$text)
  #Remove accents from words
  DF$text=stri_trans_general(DF$text,"Latin-ASCII")
  #Make all strings lower case
  DF$text=tolower(DF$text)
  DF$text=gsub("-"," ",DF$text)
  return(DF)
}
Macro_n2=clean(TextBase)
Macro_n2$line <- 1:dim(Macro_n2)[1]
#Unnest
Macro_n2 %>% unnest_tokens(word,text) -> l1
#Stop words
vec=stopwords()
#Remove stop words
l1=l1[!(l1$word %in% vec),]
#Cumulative count by word frequency
l1 %>% group_by(word) %>% dplyr::summarise(N=n()) -> r1
r1$Group <- cut(r1$N,breaks=c(0,quantile(r1$N,probs = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)),
                              max(r1$N)),include.lowest = T,right = T,dig.lab = 10,
                labels = paste0("G",1:10))
#Splitting vectors of frequency
r1$Group <- as.character(r1$Group)
v1 <- r1$word[r1$Group=="G1"]
v2 <- r1$word[r1$Group=="G2"]
v3 <- r1$word[r1$Group=="G3"]
v4 <- r1$word[r1$Group=="G4"]
v5 <- r1$word[r1$Group=="G5"]
v6 <- r1$word[r1$Group=="G6"]
v7 <- r1$word[r1$Group=="G7"]
v8 <- r1$word[r1$Group=="G8"]
v9 <- r1$word[r1$Group=="G9"]
v10 <- r1$word[r1$Group=="G10"]
#Add var
Macro_n2$G1 <- as.numeric(grepl(paste0(v1, collapse = "|"), Macro_n2$text))
Macro_n2$G2 <- as.numeric(grepl(paste0(v2, collapse = "|"), Macro_n2$text))
Macro_n2$G3 <- as.numeric(grepl(paste0(v3, collapse = "|"), Macro_n2$text))
Macro_n2$G4 <- as.numeric(grepl(paste0(v4, collapse = "|"), Macro_n2$text))
Macro_n2$G5 <- as.numeric(grepl(paste0(v5, collapse = "|"), Macro_n2$text))
Macro_n2$G6 <- as.numeric(grepl(paste0(v6, collapse = "|"), Macro_n2$text))
Macro_n2$G7 <- as.numeric(grepl(paste0(v7, collapse = "|"), Macro_n2$text))
Macro_n2$G8 <- as.numeric(grepl(paste0(v8, collapse = "|"), Macro_n2$text))
Macro_n2$G9 <- as.numeric(grepl(paste0(v9, collapse = "|"), Macro_n2$text))
Macro_n2$G10 <- as.numeric(grepl(paste0(v10, collapse = "|"), Macro_n2$text))
#Addition to define group
Macro_n2$Var <- apply(Macro_n2[,c(7:16)],1,function(x) names(x)[min(which(x==1))])
#Add cluster var
PData <- cbind(PData,Macro_n2[,c(6,17)])
#Hospital analysis
PData %>% group_by(Fakename) %>% dplyr::summarize(N=n()) -> hosp
hosp$group <- cut(hosp$N,breaks = c(0,quantile(hosp$N,probs = c(0.25,0.5,0.75,0.9)),max(hosp$N)),
                  include.lowest = T,right = T,dig.lab = 10,
                  labels = c("L","IL","M","IM","H"))
hosp <- as.data.frame(hosp)
#Group variable at hospital
PData$GroupH <- hosp[match(PData$Fakename,hosp$Fakename),"group"]
#Contrast over variables
s1 <- as.matrix(as.data.frame.matrix(table(PData$Var,PData$Var.Admission.Group)))
chisq.test(s1)
s3 <- as.matrix(as.data.frame.matrix(table(PData$GroupH,PData$Var.Admission.Group)))
chisq.test(s3)
####################################National statistics########################################
#Hospitals
hds <- length(unique(PData$Fakename))
hns <- 278
dif1 <- 1-hds/hns
#Length of stay by year available at National stats
NPData <- PData
NPData$Date <- as.Date(NPData$fake_doa,'%d/%m/%Y')
NPData$Year <- format(NPData$Date,"%Y")
NSLOS <- read_excel("Average-Length-of-Stay-by-Health-Board-and-Specialty-Sep18.xlsx","Data")
NSLOS$Year <- substring(NSLOS$finyear,1,4)
NSLOS %>% group_by(Year) %>% dplyr::summarise(cis=sum(cis),los=sum(los)) -> nss1
nss1$avglos <- nss1$los/nss1$cis
#Synthetic data
NPData %>% group_by(Year) %>% dplyr::summarise(cis=sum(fake_cis),los=sum(fake_los)) -> ssd1
ssd1 <- ssd1[(dim(ssd1)[1]-4):dim(ssd1)[1],]
ssd1$avglos <- ssd1$los/ssd1$cis
dp <- ssd1$avglos/nss1$avglos
variationlos <- mean(1-dp)
#Patient and day case
i1 <- read_excel("Inpatient-Day-Case-Activity-Council-Area-Sep18.xlsx","Data")
i2 <- read_excel("Inpatient-Day-Case-Activity-Health-Board-of-Residence-Sep18.xlsx","Data")
i3 <- read_excel("Inpatient-Day-Case-Activity-Health-Board-of-Treatment-Sep18.xlsx","Data")
im <- rbind(i1,i2,i3)
im <- im[im$admtype!="All Admission Types",]
im$admtype[im$admtype!="Daycase"] <- "Inpatient"
im$Year <- substr(im$time_period,1,4)
im <- im[im$spec!='All Specialties',]
im$spec <- sub("\\s+", ' ', im$spec)
im$Speciality <- sub("^\\S+\\s+", '', im$spec)
im %>% group_by(Year,admtype,Speciality) %>% dplyr::summarise(stays=sum(stays))->sdc1
sdc3 <- dcast(Year+Speciality~admtype,data = sdc1,fun.aggregate = sum,value.var = c("stays"))
sdc3 <- sdc3[sdc3$Year>=2013,]
#Format data
NPData %>% group_by(Year,Var.Spec,fake_ipdc) %>% dplyr::summarise(stays=n()) -> sd2
sdstays <- dcast(Year+Var.Spec~fake_ipdc,data=sd2,fun.aggregate = sum,value.var = "stays")
sdstays$N <- as.numeric(sdstays$Year)
sdstays <- sdstays[sdstays$N>=13,]
sdstays$N <- NULL
sdstays$Year <- gsub("00","20",sdstays$Year)
mpatient <- merge(sdc3,sdstays,by.x=c("Year","Speciality"),by.y=c("Year","Var.Spec"))
#Check for differences
ks.test(mpatient$Daycase,mpatient$D,alternative="tw")
ks.test(mpatient$Inpatient,mpatient$I,alternative = "tw")
#Diagnosis
DD <- read.xlsx("Diagnosis-by-Health-Board-of-Residence-Sep18.xlsx","Hospital stays",startRow = 23,
                colIndex = 2:8,header = F)
DD$X2<-trimws(as.character(DD$X2))
DD$I <- ifelse(substr(DD$X2,1,1)=="-",1,0)
DD <- DD[DD$I==1,]
DD$X2 <- gsub("-","",DD$X2)
DD$X3 <- NULL
DD$I <- NULL
#Grouping
NPData %>% group_by(Year,Var.Diagnostic) %>% dplyr::summarise(Diag=n()) -> sddiag
sddiag2 <- dcast(Var.Diagnostic~Year,data = sddiag,fun.aggregate = sum,value.var = "Diag")
sddiag2 <- sddiag2[,c(1,14:18)]
#Join
tsdiag <- merge(DD,sddiag2,by.x="X2",by.y="Var.Diagnostic")
#Check differences
ks.test(tsdiag$X4,tsdiag$`0013`,alternative="tw")
ks.test(tsdiag$X5,tsdiag$`0014`,alternative="tw")
ks.test(tsdiag$X6,tsdiag$`0015`,alternative="tw")
ks.test(tsdiag$X7,tsdiag$`0016`,alternative="tw")
ks.test(tsdiag$X8,tsdiag$`0017`,alternative="tw")
######################################Intro stats##############################################
#Extraction
PData %>% filter(fake_ipdc=="I") %>% group_by(Var.Admission.Group,Var.Gender,fake_los) %>% dplyr::summarise(N=n()) -> r1
names(r1)[1] <- "Admission"
r1$Admission <- gsub(" Admission","",r1$Admission)
#Plot sketch Days vs Gender
ggplot(r1, aes(x = fake_los,y = N,fill = Admission)) + 
  facet_wrap(Var.Gender~.)+
  geom_bar(stat = "identity",color="black")+
  scale_x_continuous(breaks = pretty_breaks(10)) + ggtitle("Days as inpatient vs Gender by Admission")+scale_fill_manual(values=c("#E69F00", "#56B4E9", "#999999"))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        legend.position="top") + xlab("Days")
#Spec, average days, status
PData$Var.Spec[is.na(PData$Var.Spec)] <- "Others"
PData %>% filter(fake_ipdc=="I") %>%group_by(Var.Admission.Group,Var.Spec,Var.Gender) %>%
  dplyr::summarise(Avg.Days=mean(fake_los))-> r2
names(r2)[1] <- "Admission"
r2$Admission <- gsub(" Admission","",r2$Admission)
#Plot 2
ggplot(r2, aes(x = Var.Spec,y = Avg.Days,fill = Admission,label=round(Avg.Days,1))) + 
  facet_grid(Var.Gender~.)+
  geom_bar(stat = "identity",color="black")+
  geom_text(size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position="top")+
  ggtitle("Average waiting time by Specification and Gender")+
  xlab("Specification")+
  ylab("Average Days")
#Plot 3
PData %>% filter(fake_ipdc=="I") %>%group_by(Var.Admission.Group,Var.Sigfac,Var.Gender) %>%
  dplyr::summarise(Avg.Days=mean(fake_los))-> r3
r3$Var.Sigfac[grepl("Other",r3$Var.Sigfac)] <- "Other"
names(r3)[1] <- "Admission"
r3$Admission <- gsub(" Admission","",r3$Admission)
ggplot(r3, aes(x = Var.Sigfac,y = Avg.Days,fill=Admission,label=round(Avg.Days,1))) + 
  facet_grid(Var.Gender~.)+
  geom_bar(stat = "identity",color="black")+
  geom_text(size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"))+
  ggtitle("Average staying time by Gender for Significant facilities and Admission")+
  xlab("Significant facilities")+
  ylab("Average Days")
#Plot sketch 4
PData$Var.Admission[is.na(PData$Var.Admission)] <- "Others"
PData %>% filter(fake_ipdc=="I") %>%group_by(Var.Admission,Var.Gender) %>%
  dplyr::summarise(Avg.Days=mean(fake_los))-> r4
r4$Var.Admission <- gsub("\\s*\\([^\\)]+\\)","",r4$Var.Admission)
r4$Var.Admission <- ifelse(grepl("Patient admitted on day of decision to admit",r4$Var.Admission),
                           "Patient admitted on day of decision to admit",r4$Var.Admission)
ggplot(r4, aes(x = Var.Admission,y = Avg.Days,fill=Var.Gender,label=round(Avg.Days,1))) + 
  geom_bar(stat = "identity",color="black")+
  geom_text(size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"))+
  ggtitle("Average waiting time by Admission and Gender")+
  xlab("Admission")+
  ylab("Average Days")
#plot 5 marital status
PData %>% filter(fake_ipdc=="I") %>%group_by(Var.Admission.Group,Var.Spec,Var.Marital) %>%
  dplyr::summarise(Avg.Days=mean(fake_los))-> r5
names(r5)[1]<-"Admission"
ggplot(r5, aes(x = Var.Spec,y = Avg.Days,fill = Admission)) + 
  facet_grid(Var.Marital~.)+
  geom_bar(stat = "identity",color="black")+
  #geom_text(size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  geom_text(data=subset(r5,Avg.Days>=10),aes(y=Avg.Days,label=round(Avg.Days,1)),size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position="top")+
  ggtitle("Average waiting time by Specification and Marital Status")+
  xlab("Specification")+
  ylab("Average Days")
#plot 6 marital status
PData %>% filter(fake_ipdc=="I") %>%group_by(Var.Admission.Group,Var.Sigfac,Var.Marital) %>%
  dplyr::summarise(Avg.Days=mean(fake_los))-> r6
names(r6)[1] <- "Admission"
r6$Admission <- gsub(" Admission","",r6$Admission)
r6$Var.Sigfac[grepl("Other ",r6$Var.Sigfac)] <- "Other" 
ggplot(r6, aes(x = Var.Sigfac,y = Avg.Days,fill=Admission,label=round(Avg.Days,1))) + 
  facet_grid(Var.Marital~.)+
  geom_bar(stat = "identity",color="black")+
  geom_text(size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position="top")+
  ggtitle("Average stay by Facilities and Marital status")+
  xlab("Facilities")+
  ylab("Average Days")
#Plot gender and marital status
PData %>% filter(fake_ipdc=="I") %>%group_by(Var.Admission.Group,Var.Gender,Var.Marital) %>%
  dplyr::summarise(Avg.Days=mean(fake_los))-> r7
names(r7)[1] <- "Admission"
r7$Admission <- gsub(" Admission","",r7$Admission)
ggplot(r7, aes(x = Var.Marital,y = Avg.Days,fill=Admission,label=round(Avg.Days,1))) + 
  facet_grid(Var.Gender~.)+
  geom_bar(stat = "identity",color="black")+
  geom_text(size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position="top")+
  ggtitle("Average stay by Gender and Marital status")+
  xlab("Marital Status")+
  ylab("Average Days")
#Hospitals plot
#Gender hospital
PData %>% filter(fake_ipdc=="I") %>%group_by(Var.Admission.Group,Var.Gender,Fakename) %>%
  dplyr::summarise(Avg.Days=mean(fake_los))-> r8
names(r8)[1] <- "Admission"
r8$Admission <- gsub(" Admission","",r8$Admission)
ggplot(r8, aes(x = reorder(Fakename,-Avg.Days),y = Avg.Days,fill=Admission,label=round(Avg.Days,0))) + 
  facet_grid(Var.Gender~.)+
  geom_bar(stat = "identity",color="black")+
  geom_text(size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position="top")+
  ggtitle("Average stay by Gender and Hospital")+
  xlab("Hospital")+
  ylab("Average Days")
#Number of visits filter(fake_ipdc=="I") %>%
PData %>% group_by(Var.Admission.Group,Var.Gender,Fakename) %>%
  dplyr::summarise(N=n())-> r9
names(r9)[1] <- "Admission"
r9$Admission <- gsub(" Admission","",r9$Admission)
ggplot(r9, aes(x = reorder(Fakename,-N),y = N,fill=Admission,label=N)) + 
  facet_grid(Var.Gender~.)+
  geom_bar(stat = "identity",color="black")+
  geom_text(size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position="top")+
  ggtitle("Number of visits by Gender and Hospital")+
  xlab("Hospital")+
  ylab("Number of visits")

#Marital status and hospital
PData %>% filter(fake_ipdc=="I") %>%group_by(Var.Admission.Group,Var.Marital,Fakename) %>%
  dplyr::summarise(Avg.Days=mean(fake_los))-> r10
names(r10)[1] <- "Admission"
r10$Admission <- gsub(" Admission","",r10$Admission)
r10$Fakename <- gsub("_hospital","",r10$Fakename)
ggplot(r10, aes(x = reorder(Fakename,-Avg.Days),y = Avg.Days,fill=Admission,label=round(Avg.Days,0))) + 
  facet_grid(Var.Marital~.)+
  geom_bar(stat = "identity",color="black")+
  geom_text(data=subset(r10,Avg.Days>=10),aes(y=Avg.Days,label=round(Avg.Days)),size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position="top")+
  ggtitle("Average stay by Marital status and Hospital")+
  xlab("Hospital")+
  ylab("Average Days")

#Number of visits per marital status filter(fake_ipdc=="I") %>%
PData %>% group_by(Var.Admission.Group,Var.Marital,Fakename) %>%
  dplyr::summarise(N=n())-> r11
names(r11)[1] <- "Admission"
r11$Admission <- gsub(" Admission","",r11$Admission)
r11$Fakename <- gsub("_hospital","",r11$Fakename)
r11$Fakename <- gsub("_hospita","",r11$Fakename)
ggplot(r11, aes(x = reorder(Fakename,-N),y = N,fill=Admission,label=N)) + 
  facet_grid(Var.Marital~.)+
  geom_bar(stat = "identity",color="black")+
  geom_text(data=subset(r11,N>=15),aes(y=N,label=N),size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position="top")+
  ggtitle("Number of visits by Marital status and Hospital")+
  xlab("Hospital")+
  ylab("Number of visits")
#Plots with avg age and number by day of week and month
PData$Date <- as.Date(PData$fake_doa,format = '%d/%m/%Y')
PData$DW <- format(PData$Date,"%u")
PData$Month <- format(PData$Date,"%m")
#Number of visits by day of week by gender an admission type filter(fake_ipdc=="I") %>%
PData %>% group_by(Var.Admission.Group,Var.Gender,DW) %>%
  dplyr::summarise(N=n())-> r12
names(r12)[1] <- "Admission"
r12$Admission <- gsub(" Admission","",r12$Admission)
ggplot(r12, aes(x = DW,y = N,fill=Admission,label=N)) + 
  facet_wrap(Var.Gender~.)+
  geom_bar(stat = "identity",color="black")+
  geom_text(data=subset(r12,N>=0),aes(y=N,label=N),size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position="top")+
  ggtitle("Number of visits by Gender and Day of Week")+
  xlab("Day of Week")+
  ylab("Number of visits")
#Plot average stay and day
PData %>% group_by(Var.Admission.Group,Var.Gender,DW) %>%
  dplyr::summarise(N=n())-> r12
names(r12)[1] <- "Admission"
r12$Admission <- gsub(" Admission","",r12$Admission)
ggplot(r12, aes(x = DW,y = N,fill=Admission,label=N)) + 
  facet_wrap(Var.Gender~.)+
  geom_bar(stat = "identity",color="black")+
  geom_text(data=subset(r12,N>=0),aes(y=N,label=N),size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position="top")+
  ggtitle("Number of visits by Gender and Day of Week")+
  xlab("Day of Week")+
  ylab("Number of visits")
#Plot day status
PData %>% group_by(Var.Admission.Group,Var.Gender,DW) %>%
  dplyr::summarise(N=n())-> r13
names(r13)[1] <- "Admission"
r13$Admission <- gsub(" Admission","",r13$Admission)
ggplot(r13, aes(x = DW,y = N,fill=Admission,label=N)) + 
  facet_wrap(Var.Gender~.)+
  geom_bar(stat = "identity",color="black")+
  geom_text(data=subset(r13,N>=0),aes(y=N,label=N),size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position="top")+
  ggtitle("Number of visits by Marital status and Day of Week")+
  xlab("Day of Week")+
  ylab("Number of visits")
#Plot visits marital status
PData %>% group_by(Var.Admission.Group,Var.Marital,DW) %>%
  dplyr::summarise(N=n())-> r15
names(r15)[1] <- "Admission"
r15$Admission <- gsub(" Admission","",r15$Admission)
ggplot(r15, aes(x = DW,y = N,fill=Admission,label=N)) + 
  facet_grid(Var.Marital~.)+
  geom_bar(stat = "identity",color="black")+
  geom_text(data=subset(r15,N>=25),aes(y=N,label=N),size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position="top")+
  ggtitle("Number of visits by Marital status and Day of Week")+
  xlab("Day of Week")+
  ylab("Number of visits")
#Plot avg stay vs day
PData %>% filter(fake_ipdc=="I") %>% group_by(Var.Admission.Group,Var.Marital,DW) %>%
  dplyr::summarise(Avg.Days=mean(fake_los))-> r16
names(r16)[1] <- "Admission"
r16$Admission <- gsub(" Admission","",r16$Admission)
ggplot(r16, aes(x = DW,y = Avg.Days,fill=Admission)) + 
  facet_grid(Var.Marital~.)+
  geom_bar(stat = "identity",color="black")+
  geom_text(data=subset(r16,Avg.Days>=0),aes(y=Avg.Days,label=round(Avg.Days)),size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position="top")+
  ggtitle("Length of stay by Marital status and Day of Week")+
  xlab("Day of Week")+
  ylab("Average length of stay")
#Plots with avg age and number by day of week and month
#Plot avg age
PData %>% group_by(Var.Admission.Group,Var.Marital,DW) %>%
  dplyr::summarise(Avg.Age=mean(fake_age))-> r17
names(r17)[1] <- "Admission"
r17$Admission <- gsub(" Admission","",r17$Admission)
ggplot(r17, aes(x = DW,y = Avg.Age,fill=Admission)) + 
  facet_grid(Var.Marital~.)+
  geom_bar(stat = "identity",color="black")+
  geom_text(data=subset(r17,Avg.Age>=0),aes(y=Avg.Age,label=round(Avg.Age)),size = 3, position = position_stack(vjust = 0.5),fontface="bold")+
  theme(axis.text.x = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5,size=14,face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position="top")+
  ggtitle("Average age by Marital status and Day of Week")+
  xlab("Day of Week")+
  ylab("Average Age")
#Reshape for statistical contrast
#Global structure 1
PData %>% group_by(Fakename,Var.Gender,fake_ipdc) %>% dplyr::summarise(N=n(),Avg.Days=mean(fake_los),Avg.Age=mean(fake_age)) -> m1
#Global structure 2
PData %>% group_by(Var.Gender,fake_ipdc) %>% dplyr::summarise(Avg.Age=mean(fake_age)) -> m2
#Data reshape for gender
#Gender
t1 <- dcast(Var.Gender~fake_ipdc,data = m1[,c(2,3,4)],value.var = "N",fun.aggregate = sum)
rownames(t1) <- t1$Var.Gender
t1$Var.Gender <- NULL
t1 <- as.matrix(t1)
#* t gender
tc1 <- chisq.test(t1)
uc1 <- wilcox.test(t1[,1],t1[,2],paired = T,alternative = "two.sided")
#Avg days
t2 <- dcast(Var.Gender~fake_ipdc,data = m1[,c(2,3,5)],value.var = "Avg.Days",fun.aggregate = sum)
#Avg age
m2 <- as.data.frame(m2)
t3 <- reshape(data = m2,idvar = "Var.Gender",timevar = "fake_ipdc",direction = "wide")
vc1 <- wilcox.test(t3$Avg.Age.D,t3$Avg.Age.I,paired = T,alternative = "two.sided")
#Number visits and age by marital status
#Visits
t4 <- as.matrix(table(PData$Var.Marital,PData$fake_ipdc))
#* marital status
tc2 <- chisq.test(t4)
uc2 <- wilcox.test(t4[,1],t4[,2],paired = T,alternative = "two.sided")
#Avg age
PData %>% group_by(Var.Marital,fake_ipdc) %>% dplyr::summarise(Avg.Age=mean(fake_age)) -> n5 
n5 <- as.data.frame(n5)
t5 <- reshape(data = n5,idvar = "Var.Marital",timevar = "fake_ipdc",direction = "wide")
vc2 <- wilcox.test(t5$Avg.Age.D,t5$Avg.Age.I,paired = T,alternative = "two.sided")
#Number of visits and age by Specification
#Visits
t6 <- as.matrix(table(PData$Var.Spec,PData$fake_ipdc))
#* specification
tc3 <- chisq.test(t6)
uc3 <- wilcox.test(t6[,1],t6[,2],paired = T,alternative = "two.sided")
#Avg age
PData %>% group_by(Var.Spec,fake_ipdc) %>% dplyr::summarise(Avg.Age=mean(fake_age)) -> n7 
n7 <- as.data.frame(n7)
t7 <- reshape(data = n7,idvar = "Var.Spec",timevar = "fake_ipdc",direction = "wide")
t7[is.na(t7)] <- 0
vc3 <- wilcox.test(t7$Avg.Age.D,t7$Avg.Age.I,paired = T,alternative = "two.sided")
#Number of visits and age by facilities
#Visits
t8 <- as.matrix(table(PData$Var.Sigfac,PData$fake_ipdc))
#* facilities
tc4 <- chisq.test(t8)
uc4 <- wilcox.test(t8[,1],t8[,2],paired = T,alternative = "two.sided")
#Age
PData %>% group_by(Var.Sigfac,fake_ipdc) %>% dplyr::summarise(Avg.Age=mean(fake_age)) -> n8 
n8 <- as.data.frame(n8)
t9 <- reshape(data = n8,idvar = "Var.Sigfac",timevar = "fake_ipdc",direction = "wide")
t9[is.na(t9)] <- 0
vc4 <- wilcox.test(t9$Avg.Age.D,t9$Avg.Age.I,paired = T,alternative = "two.sided")
#Number of visits and age by Var.Wait
#Visits
t10 <- as.matrix(table(PData$Var.Wait,PData$fake_ipdc))
#* wait
tc5 <- chisq.test(t10)
uc5 <- wilcox.test(t10[,1],t10[,2],paired = T,alternative = "two.sided")
#Age
PData %>% group_by(Var.Wait,fake_ipdc) %>% dplyr::summarise(Avg.Age=mean(fake_age)) -> n10 
n10 <- as.data.frame(n10)
t11 <- reshape(data = n10,idvar = "Var.Wait",timevar = "fake_ipdc",direction = "wide")
vc5 <- wilcox.test(t11$Avg.Age.D,t11$Avg.Age.I,paired = T,alternative = "two.sided")
#Number of visits and age by admission group *Add tables instead
#Visits
t12 <- as.matrix(table(PData$Var.Admission.Group,PData$fake_ipdc))
#* admission group
tc6 <- chisq.test(t12)
uc6 <- wilcox.test(t12[,1],t12[,2],paired = T,alternative = "two.sided")
#Age
PData %>% group_by(Var.Admission.Group,fake_ipdc) %>% dplyr::summarise(Avg.Age=mean(fake_age)) -> n11 
n11 <- as.data.frame(n11)
t13 <- reshape(data = n11,idvar = "Var.Admission.Group",timevar = "fake_ipdc",direction = "wide")
t13[is.na(t13)] <- 0
vc6 <- wilcox.test(t13$Avg.Age.I,t13$Avg.Age.D,paired = T,alternative = "two.sided")
#Number of visits and age by grouped diagnostic less common to more common
#Visits
t14 <- as.matrix(table(PData$Var,PData$fake_ipdc))
#* grouped diagnostic
tc7 <- chisq.test(t14)
uc7 <- wilcox.test(t14[,1],t14[,2],paired = T,alternative = "two.sided")
#Avg age
PData %>% group_by(Var,fake_ipdc) %>% dplyr::summarise(Avg.Age=mean(fake_age)) -> n12 
n12 <- as.data.frame(n12)
t15 <- reshape(data = n12,idvar = "Var",timevar = "fake_ipdc",direction = "wide")
vc7 <- wilcox.test(t15$Avg.Age.D,t15$Avg.Age.I,paired = T,alternative = "two.sided")
#N visits and age by day of week
#N visits
t16 <- as.matrix(table(PData$DW,PData$fake_ipdc))
#* day of week
tc8 <- chisq.test(t16)
uc8 <- wilcox.test(t16[,1],t16[,2],paired = T,alternative = "two.sided")
#Avg age
PData %>% group_by(DW,fake_ipdc) %>% dplyr::summarise(Avg.Age=mean(fake_age)) -> n13 
n13 <- as.data.frame(n13)
t17 <- reshape(data = n13,idvar = "DW",timevar = "fake_ipdc",direction = "wide")
vc8 <- wilcox.test(t17$Avg.Age.D,t17$Avg.Age.I,paired = T,alternative = "two.sided")
#N visits and age month of year
#Nvisits
t18 <- as.matrix(table(PData$Month,PData$fake_ipdc))
#* month of year
tc9 <- chisq.test(t18)
uc9 <- wilcox.test(t18[,1],t18[,2],paired = T,alternative = "two.sided")
#Avg age
PData %>% group_by(Month,fake_ipdc) %>% dplyr::summarise(Avg.Age=mean(fake_age)) -> n14 
n14 <- as.data.frame(n14)
t19 <- reshape(data = n14,idvar = "Month",timevar = "fake_ipdc",direction = "wide")
vc9 <- wilcox.test(t19$Avg.Age.D,t19$Avg.Age.I,paired = T,alternative = "two.sided")
################################Stage by hospital##############################################
#Number of visits and age by hospital
#N visits
t20 <- as.matrix(table(PData$Fakename,PData$fake_ipdc))
#* hospital
tc10 <- chisq.test(t20)
uc10 <- wilcox.test(t20[,1],t20[,2],paired = T,alternative = "two.sided")
#Avg age
PData %>% group_by(Fakename,fake_ipdc) %>% dplyr::summarise(Avg.Age=mean(fake_age)) -> n15 
n15 <- as.data.frame(n15)
t21 <- reshape(data = n15,idvar = "Fakename",timevar = "fake_ipdc",direction = "wide")
t21[is.na(t21)] <- 0
uc11 <- wilcox.test(t21$Avg.Age.D,t21$Avg.Age.I,paired = T,alternative = "two.sided")
#Length of stay
PData %>% group_by(Fakename,fake_ipdc) %>% dplyr::summarise(Avg.Stay=mean(fake_los)) -> n16 
n16 <- as.data.frame(n16)
t22 <- reshape(data = n16,idvar = "Fakename",timevar = "fake_ipdc",direction = "wide")
t22[is.na(t22)] <- 0
t22$Avg.Stay.D <- NULL
wc11 <- wilcox.test(t22$Avg.Stay.I,alternative = "two.sided")
#Dataframe design count
Variable <- c("Gender","Marital status","Specification","Facilities","Waiting list","Admission group","Diagnostic","Day of week","Month of year")
Chi.Squared.Test <- c(tc1$p.value,tc2$p.value,tc3$p.value,tc4$p.value,tc5$p.value,tc6$p.value,tc7$p.value,tc8$p.value,tc9$p.value)
Wilcoxon.Test <- c(uc1$p.value,uc2$p.value,uc3$p.value,uc4$p.value,uc5$p.value,uc6$p.value,uc7$p.value,uc8$p.value,uc9$p.value)
DF1 <- data.frame(Variable,Chi.Squared.Test,Wilcoxon.Test,stringsAsFactors = F)
#Dataframe design age
Wilcoxon.Testa <- c(vc1$p.value,vc2$p.value,vc3$p.value,vc4$p.value,vc5$p.value,vc6$p.value,vc7$p.value,vc8$p.value,vc9$p.value)
DF2 <- data.frame(Variable,Wilcoxon.Testa,stringsAsFactors = F)
#Small dataframe hospitals
Variable2 <- c("Visits", "Age", "Length of stay")
Chi.squared.Test2 <- c(tc10$p.value,NA,NA)
Wilcoxon.Test2 <- c(uc10$p.value,uc11$p.value,wc11$p.value)
DF3 <- data.frame(Variable2,Chi.squared.Test2,Wilcoxon.Test2,stringsAsFactors = F)
#Variables previous modeling
PData$Date <- as.Date(PData$fake_doa,format = '%d/%m/%Y')
PData$DW <- format(PData$Date,"%u")
PData$Month <- format(PData$Date,"%m")
#Format dataset for SVM
DataM <- PData[,c(3,5,6,8,10,11,12,14,15,17,18,20,21)]
#Grouping specification
PData %>% group_by(Var.Spec) %>% dplyr::summarise(N=n()) %>% arrange(-N) -> pb1
pb1$Group.Spec <- cut(pb1$N,breaks = c(0,quantile(pb1$N,c(0.4,0.9)),max(pb1$N)),right = T,include.lowest = T,dig.lab=10,labels = c("S1","S2","S3"))
DataM <- merge(DataM,pb1[,c(1,3)],by="Var.Spec")
tt1 <- as.matrix(table(DataM$Group.Spec,DataM$Var.Admission.Group))
rft <- fisher.test(tt1)
#Additional adjust for variables
DataM <- DataM[,-1]
names(DataM)[c(9,10,11)] <- c("Group.Diagnostic","Group.Hospital","Day")
#Format of variables
DataM$fake_ipdc <- as.factor(DataM$fake_ipdc)
DataM$Var.Marital <- as.factor(DataM$Var.Marital)
DataM$Var.Sigfac <- as.factor(DataM$Var.Sigfac)
DataM$Var.Wait <- as.factor(DataM$Var.Wait)
DataM$Var.Admission.Group <- as.factor(DataM$Var.Admission.Group)
DataM$Var.Gender <- as.factor(DataM$Var.Gender)
DataM$Group.Hospital <- as.factor(DataM$Group.Hospital)
DataM$Group.Diagnostic <- as.factor(DataM$Group.Diagnostic)
DataM$Day <- factor(DataM$Day,levels = sort(unique(DataM$Day)),ordered = T)
DataM$Month <- factor(DataM$Month,levels = sort(unique(DataM$Month)),ordered = T)
#Format model
Var.Contain <- DataM$fake_ipdc
Var.los <- DataM$fake_los
DataM <- DataM[,-c(2,3)]
#Preliminar test
vecv <- c(2:4,6:11)
empty1 <- c()
empty2 <- c()
for(i in 1:length(vecv))
{
  dm <- as.matrix(table(DataM[,vecv[i]],DataM[,5]))
  empty1[i] <- names(DataM)[vecv[i]]
  empty2[i] <- fisher.test(dm,simulate.p.value = T)$p.value
}
#Display 01
TF <- data.frame(Name=empty1,p.val=empty2,stringsAsFactors = F)
#Additional remove
DataM <- DataM[,-c(8,9,10)]
#SVM sketch 01
#Classification Var admission group
set.seed(1)
d = sample(nrow(DataM), nrow(DataM)*0.6) 
train<-DataM[d,] 
test<-DataM[-d,]
#linear svm
#Best model tuning parameters Good
# tune.out=tune(svm,Var.Admission.Group~.,data=train,kernel="linear",type="C-classification",
# ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)),probability=TRUE)
# bestmod <- tune.out$best.model
# save(bestmod,tune.out, file="svm.linear.RData")
load("svm.linear.RData")
#Prediction
pred1 <- predict(bestmod, test, probability = TRUE)
#Display 02
TF2 <- as.data.frame.matrix(table(predict=pred1, truth=test$Var.Admission.Group))
TF2$"predict|truth" <- rownames(TF2)
TF2 <- TF2[,c(4,1:3)]
rownames(TF2) <- NULL
#Prediction
Variables=data.frame(test[,c(5),drop=F],pred1)
prob1=data.frame(attr(pred1, "probabilities"))
Variables$max=pmax(prob1[,1],prob1[,2],prob1[,3])
auroc1=multiclass.roc(Variables$Var.Admission.Group,Variables$max)
auroc1
#Radial svm
# tune.out.rad=tune(svm,Var.Admission.Group~.,data=train,kernel="radial",type="C-classification",
# ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)),probability=TRUE)
# bestmod.rad <- tune.out.rad$best.model
# save(bestmod.rad,tune.out.rad,file="svm.rad.RData")
load("svm.rad.RData")
# #Prediction radial
pred1r <- predict(bestmod.rad, test, probability = TRUE)
#Display 03
TF3 <- as.data.frame.matrix(table(predict=pred1r, truth=test$Var.Admission.Group))
TF3$"predict|truth" <- rownames(TF3)
TF3 <- TF3[,c(4,1:3)]
rownames(TF3) <- NULL
#Prediction
Variables=data.frame(test[,c(5),drop=F],pred1r)
prob1r=data.frame(attr(pred1r, "probabilities"))
Variables$max=pmax(prob1r[,1],prob1r[,2],prob1r[,3])
auroc1r=multiclass.roc(Variables$Var.Admission.Group,Variables$max)
#Display 04
TF4 <- data.frame(Measure="AUROC",Linear=as.numeric(auroc1$auc),Radial=as.numeric(auroc1r$auc),
                  stringsAsFactors = F)
#Gradient boosting machine classification
#Format data
train$Var.Admission.Group <- as.character(train$Var.Admission.Group)
train$Var.Admission.Group <- substring(train$Var.Admission.Group,1,1)
train$Var.Admission.Group <- as.factor(train$Var.Admission.Group)
# #Control parameters
# fitControl <- trainControl(method = "cv",number = 10,selectionFunction = "tolerance",
#                            classProbs=T)
# # #Grid
# gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
#                         n.trees = (1:30)*50, 
#                         shrinkage = 0.1,
#                         n.minobsinnode = 20)
# # #Train model
# set.seed(678)
# gbmFit <- train(Var.Admission.Group ~ ., data = train, 
#                  method = "gbm", 
#                  trControl = fitControl, 
#                  verbose = FALSE, 
#                  tuneGrid = gbmGrid,metric="ROC")
# save(gbmFit,file = "gbm.RData")
# #Best model
# whichTwoPct <- tolerance(gbmFit$results, metric = "Accuracy",tol = 2, maximize = TRUE)  
# cat("best model within 2 pct of best:\n")
load("gbm.RData")
#Predict
test$Var.Admission.Group <- as.character(test$Var.Admission.Group)
test$Var.Admission.Group <- substring(test$Var.Admission.Group,1,1)
test$Var.Admission.Group <- as.factor(test$Var.Admission.Group)
Predictgbm <- predict(gbmFit,test[,-5],type="prob")
Variablesgbm <- data.frame(Var=test$Var.Admission.Group,predclass=predict(gbmFit,test[,-5]),stringsAsFactors = F)
table(predict=Variablesgbm$predclass, truth=Variablesgbm$Var)
#Display 05
TF5 <- as.data.frame.matrix(table(predict=Variablesgbm$predclass, truth=Variablesgbm$Var))
TF5$"predict|truth" <- c("Emergency Admission","Routine Admission","Urgent Admission")
TF5 <- TF5[,c(4,1:3)]
names(TF5) <- names(TF3)
rownames(TF5) <- NULL
Predictgbm$Max <- pmax(Predictgbm[,1],Predictgbm[,2],Predictgbm[,3])
aurocgbm=multiclass.roc(test$Var.Admission.Group,Predictgbm$Max)
###Neural Networks use DataM and then DataMR
set.seed(123)
#Variable preparation
vars <- c("Var.Marital","Var.Sigfac","Var.Wait","Var.Gender","Group.Diagnostic","Group.Spec")
#Treatment
treatplan <- designTreatmentsZ(DataM, vars)
scoreFrame <- treatplan %>%
  use_series(scoreFrame) %>%
  select(varName, origName, code)
newvars <- scoreFrame %>%
  filter(code %in% c("clean", "lev")) %>%
  use_series(varName)
dframe.treat <- prepare(treatplan, DataM, varRestriction = newvars)
index <- which(names(DataM) %in% vars)
DataM2 <- DataM[,-c(index)]
nb <- cbind(DataM2,dframe.treat)
#E=1,R=2,U=3
nb$Var.Admission.Group <- as.numeric(nb$Var.Admission.Group)
#NN process
nb.y <- decodeClassLabels(nb[,2])
set.seed(1)
nb.trimmed <- splitForTrainingAndTest(nb[,-2],nb.y, 0.4)
nb.trimmed <- normTrainingAndTestSet(nb.trimmed)
# #Perceptron
# mlpmodel <- mlp(nb.trimmed$inputsTrain,
#                 nb.trimmed$targetsTrain, size=5,
#                 learnFuncParams=c(0.1),maxit=100,
#                 inputsTest=nb.trimmed$inputsTest,
#                 targetsTest=nb.trimmed$targetsTest)
# save(mlpmodel,file="nnc.RData")
load("nnc.RData")
#Display 06
TF6 <- as.data.frame.matrix(confusionMatrix(as.factor(preds2), as.factor(truth2)))
names(TF6) <- c("Emergency Admission", "Routine Admission", "Urgent Admission")
dummyds <- data.frame(v1=0,v2=0,v3=0)
names(dummyds) <- names(TF6)
TF6 <- rbind(TF6,dummyds)
TF6$"predict|truth" <- c("Emergency Admission", "Routine Admission", "Urgent Admission")
TF6 <- TF6[,c(4,1:3)]
rownames(TF6) <- NULL
#AUROC
TestDF <- as.data.frame(nb.trimmed$targetsTest)
TestDF$Class <- apply(TestDF,1,function(x) names(TestDF)[which.max(x)])
Prednn <- as.data.frame(mlpmodel$fittedTestValues)
Prednn$Max <- pmax(Prednn[,1],Prednn[,2],Prednn[,3])
TestDF$Pred <- Prednn$Max
aurocnnc=multiclass.roc(as.factor(TestDF$Class),TestDF$Pred)
#Generalized regression model DataMR
los.glm <- glm(formula = Var.los~.,family = poisson(identity),data=DataMR,start = rep(0.1,37))
DataMR2 <- DataMR
DataMR2$Fitted <- predict(los.glm,DataMR2)
RMSEpr <- rmse(DataMR2$Var.los,DataMR2$Fitted)
#Sketch for logistic regression LRData and var.contain
Var.Contain2 <- as.numeric(Var.Contain)-1
LRData2 <- cbind(LRData,Var.Contain2)
#Optional
#LRData2$Var.Contain2 <- factor(LRData2$Var.Contain2)
#Sample
set.seed(1)
d4 = sample(nrow(LRData2), nrow(LRData2)*0.6) 
train4<-LRData2[d4,] 
test4<-LRData2[-d4,]
rtrain4 <- train4
rtest4 <- test4
# #Train
# fitC <- trainControl(method = "cv",number = 10,classProbs=T)
# lrfit <- caret::train(Var.Contain2 ~ fake_age+Var.Marital+Var.Sigfac+Var.Wait+Var.Admission.Group+Var.Gender+Group.Diagnostic+Group.Spec,  data=train4, method="glm", family="binomial",trControl = fitC)
# save(lrfit,file="lrmodel.RData")
load("lrmodel.RData")
#Predict
test4$Prob <- predict(lrfit,test4[,-9])
#ROC
predlr<-prediction(test4$Prob,test4$Var.Contain2)
AUC=performance(predlr,"auc")
AUROC=as.numeric(AUC@y.values)
#Cutoff
perf <- performance(predlr,"tpr","fpr")
cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
cutoffs2 <- cutoffs[cutoffs$fpr<0.2,]
#New class
test4$New.Class <- ifelse(test4$Prob>0.4,1,0)
#Matrix
lrt <- as.data.frame.matrix(table(test4$New.Class,test4$Var.Contain2))
names(lrt) <- c("D","I")
lrt$"predict|truth" <- c("D","I")
lrt <- lrt[,c(3,1,2)]
rownames(lrt) <- NULL
#Train svm
#Best model tuning parameters Good
# tune.out.kp=tune(svm,Var.Contain2~.,data=train4,kernel="linear",type="C-classification",
# ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100,500)),probability=TRUE)
# bestmodkp <- tune.out.kp$best.model
# save(bestmodkp,tune.out.kp, file="svm.linear.kp.RData")
load("svm.linear.kp.RData")
#Prediction
pred1sv <- predict(bestmodkp, rtest4[,-9], probability = TRUE)
#Display 02
TFkp <- as.data.frame.matrix(table(predict=pred1sv, truth=rtest4$Var.Contain2))
TFkp$"predict|truth" <- c("D","I")
TFkp <- TFkp[,c(3,1:2)]
rownames(TFkp) <- NULL
names(TFkp)[c(2,3)] <- c("D","I")
#table(predict=pred1, truth=test$Var.Admission.Group)
Variableskp=data.frame(rtest4[,c(9),drop=F],pred1sv)
prob1kp=data.frame(attr(pred1sv, "probabilities"))
Variableskp$max=pmax(prob1kp[,1],prob1kp[,2])
aurockp=multiclass.roc(Variableskp$Var.Contain2,Variableskp$max)
#Predict admission group class and probability
SDataM <- RDataM
predg <- predict(bestmod, SDataM[,-5], probability = TRUE)
probglobal=data.frame(attr(predg, "probabilities"))
SDataM$New.Admission.Type <- predict(bestmod, SDataM[,-5])
SDataM$Probability <- pmax(probglobal[,1],probglobal[,2],probglobal[,3])
#Sketch for logistic regression
SDataM$Probability.ipdc <- predict(lrfit,SDataM[,-c(9,10)])
SDataM$New.ipdc <- ifelse(SDataM$Probability.ipdc>0.4,"I","D")
#Structure to predict los
TDataM <- cbind(RDataM,Var.Contain,Var.los)
TDataM$Level.Age <- cut(TDataM$fake_age,breaks=c(0,10,20,30,40,50,60,70,80,90),include.lowest = T,right = T,dig.lab = 10)
#Level dataframe homologation facilities
SFDF <- data.frame(Facility=levels(TDataM$Var.Sigfac),Code=paste0("SF",1:15),stringsAsFactors = F)
veclev <- paste0("SF",1:15)
for(j in 1:15)
{
  levels(TDataM$Var.Sigfac)[j] <- veclev[j]
}
#Sketch model
#Split datasets
set.seed(1)
d5 = sample(nrow(TDataM), nrow(TDataM)*0.6) 
train5<-TDataM[d5,] 
test5<-TDataM[-d5,]
#Essay 01
tree.los = tree (Var.los ~ .,data = train5[,-1])
# summary(tree.los)
#plot(tree.los)
#text(tree.los, pretty=0)
#Contrast
yhat = predict(tree.los, newdata=test5[,-c(1,10)])
los.test = test5$Var.los
RMSE.tree <- rmse(yhat,los.test)
#CART ,control=rpart.control(minsplit=20,cp = 0.001,maxdepth=5)
rtree<-rpart(formula =Var.los~., method="anova",data=train5[,-1],control=rpart.control(minsplit=20,maxdepth=7,cp=0.01))
#fancyRpartPlot(rtree,sub = "")
yhat2 = predict(rtree, newdata=test5[,-c(1,10)])
RMSE.rpart <- rmse(yhat2,los.test)
#GRM
los.glm <- glm(formula = Var.los~.,family = poisson(identity),data=train5[,-1],start = rep(0.1,45))
test5$Fitted <- predict(los.glm,test5[,-c(1,10)])
RMSEpr <- rmse(test5$Var.los,test5$Fitted)
#Final structure
VDataM <- cbind(SDataM,Var.los,Var.Contain)
#Format
VDataM$Level.Age <- cut(VDataM$fake_age,breaks=c(0,10,20,30,40,50,60,70,80,90),include.lowest = T,right = T,dig.lab = 10)
#Age levels
veclev2 <- paste0("SF",1:15)
for(k in 1:15)
{
  levels(VDataM$Var.Sigfac)[k] <- veclev2[k]
}
#Save
VDataM$Predicted.los <- predict(rtree,VDataM[,-c(1,9,10,11,12,13)])
