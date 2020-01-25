###########################
# Analysis of fNIRS data  #
# Single Channel Analysis #
# Ian McCulloh 8/21/18    #
###########################

rm(list=ls())

#Required libraries
library(data.table)
library(vioplot)

#Load data
VT<-read.csv("VideoTimes.csv",header=TRUE) #start/stop times baseline, start video
VO<-read.csv("VideoOrder.csv",header=FALSE, stringsAsFactors = FALSE) #video randomized order by video length

##Read filenames
datafiles<-list.files(pattern="*.txt")

##Specify the fNIRS channel being analyzed
channel<-13

##Load filenames for available channels
fileindex<-grep(paste0("-",channel),datafiles) #determine which files correspond to channel
for(i in 1:length(fileindex)){                 #read in all files with channel, data is S01,S02,...
  print(i)
  assign(unlist(strsplit(datafiles[fileindex[i]],"-"))[1],
         read.table(datafiles[fileindex[i]],
                    header=FALSE, 
                    quote="",
                    stringsAsFactors=FALSE))
}

#Create a function to calculate the block average of a channel
##input includes S - the subject from the file name S01, S02, etc
#                ID - the row in the VT/VO file with start/stop times
#                times - the start/stop times in the VT file
#                vl - the length of each video
block.avg<-function(S,ID, times, vl){
  s<-55   #Creates a delay to account for the hemodynamic response 7.8*7~55
  assign(paste0(deparse(substitute(S)),".","V1B",".","HbO"),
         mean(as.numeric(S[2,round(7.8*times[ID,2]+s):round(7.8*times[ID,3])])[]))
  assign(paste0(deparse(substitute(S)),".","V1",".","HbO"),
         mean(as.numeric(S[2,round(7.8*times[ID,3]+s):round(7.8*(times[ID,3]+as.numeric(vl[ID,2])))])[]))
  assign(paste0(deparse(substitute(S)),".","V2B",".","HbO"),
         mean(as.numeric(S[2,round(7.8*times[ID,4]+s):round(7.8*times[ID,5])])[]))
  assign(paste0(deparse(substitute(S)),".","V2",".","HbO"),
         mean(as.numeric(S[2,round(7.8*times[ID,5]+s):round(7.8*(times[ID,5]+as.numeric(vl[ID,3])))])[]))
  assign(paste0(deparse(substitute(S)),".","V3B",".","HbO"),
         mean(as.numeric(S[2,round(7.8*times[ID,6]+s):round(7.8*times[ID,7])])[]))
  assign(paste0(deparse(substitute(S)),".","V3",".","HbO"),
         mean(as.numeric(S[2,round(7.8*times[ID,7]+s):round(7.8*(times[ID,7]+as.numeric(vl[ID,4])))])[]))
  assign(paste0(deparse(substitute(S)),".","V4B",".","HbO"),
         mean(as.numeric(S[2,round(7.8*times[ID,8]+s):round(7.8*times[ID,9])])[]))
  assign(paste0(deparse(substitute(S)),".","V4",".","HbO"),
         mean(as.numeric(S[2,round(7.8*times[ID,9]+s):round(7.8*(times[ID,9]+as.numeric(vl[ID,5])))])[]))
  
  df<-data.frame(eval(parse(text = paste0(deparse(substitute(S)),".","V1B",".","HbO"))),
                 eval(parse(text = paste0(deparse(substitute(S)),".","V1",".","HbO"))),
                 eval(parse(text = paste0(deparse(substitute(S)),".","V2B",".","HbO"))),
                 eval(parse(text = paste0(deparse(substitute(S)),".","V2",".","HbO"))),
                 eval(parse(text = paste0(deparse(substitute(S)),".","V3B",".","HbO"))),
                 eval(parse(text = paste0(deparse(substitute(S)),".","V3",".","HbO"))),
                 eval(parse(text = paste0(deparse(substitute(S)),".","V4B",".","HbO"))),
                 eval(parse(text = paste0(deparse(substitute(S)),".","V4",".","HbO"))))
  names(df)<-c("V1B.HbO", "V1.HbO","V2B.HbO", "V2.HbO","V3B.HbO", "V3.HbO","V4B.HbO", "V4.HbO")
  return(df)
}

#Create a data frame of engagement scores
blks<-data.frame(matrix(vector(), 0, 8, dimnames=list(c(),c("V1B.HbO", "V1.HbO","V2B.HbO", "V2.HbO","V3B.HbO", "V3.HbO","V4B.HbO", "V4.HbO"))))

#Create a data frame of block averages
for(i in 1:length(fileindex)){
  Subject<-eval(parse(text=paste0(unlist(strsplit(datafiles[fileindex[i]], "-"))[1])))
  Index<-as.numeric(unlist(strsplit(paste0(unlist(strsplit(datafiles[fileindex[i]], "-"))[1]),"S"))[2])
  print(Index)
  blks[Index,]<-block.avg(Subject,Index,VT,VO)
}
blks[is.na(blks)]<-NA

#Sort the block averages for HbO, so that each column represents a different video
##A=Chess 103s, B=Nice 115s, C=Grad 57s, D=Bashir 227s
HbO <- data.frame( A=rep(NA, dim(blks)[1]), 
                   B=rep(NA, dim(blks)[1]), 
                   C=rep(NA, dim(blks)[1]), 
                   D=rep(NA, dim(blks)[1]))
for(i in 1:dim(blks)[1]){
  for(j in 1:4){
    if(VO[i,(j+1)]==103) HbO[i,1]<-blks[i,(2*j)]-blks[i,(2*j-1)]
    if(VO[i,(j+1)]==115) HbO[i,2]<-blks[i,(2*j)]-blks[i,(2*j-1)]
    if(VO[i,(j+1)]==57) HbO[i,3]<-blks[i,(2*j)]-blks[i,(2*j-1)]
    if(VO[i,(j+1)]==227) HbO[i,4]<-blks[i,(2*j)]-blks[i,(2*j-1)]
  }
}

vioplot(na.omit(HbO$A), na.omit(HbO$B), na.omit(HbO$C), na.omit(HbO$D), names=c("Chess", "Nice","Grad","Bashir"), 
        col="gold")
title("MPFC Change Compared to Baseline")

#RQ5 Increased counter arguing
t.test(HbO$A, mu=0)
t.test(HbO$B, mu=0)
t.test(HbO$C, mu=0)
t.test(HbO$D, mu=0)

HbO.aov.dat<-stack(HbO)
aov.1<-aov(values~ind, HbO.aov.dat)
summary(aov.1)

#Load Survey Data
surv.data<-read.csv("SD.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)
sec1<-scoreItems(c(-1,-1,1,1,1,1,1,1),surv.data[,13:20],totals = FALSE)
sec<-sec1$scores[1:23,]
sec[4]<-sec1$scores[5,]
sec[5]<-sec1$scores[4,]
sec[22]<-sec1$scores[24,]
sec1.sect<-surv.data[1:23,3]
sec1.sect[4]<-surv.data[5,3]
sec1.sect[5]<-surv.data[4,3]
sec1.sect[22]<-surv.data[24,3]
sec.test<-data.frame(sec[1:23],sec1.sect)
names(sec.test)<-c("score","sect")
t.test(score~sect,sec.test) #Is there a difference in sectarianism Shi'a Sunni

surv.data.2<-read.csv("SD2.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)
sec2<-scoreItems(c(-1,-1,1,1,1,1,1,1),surv.data.2[71:94,12:19],totals = FALSE)
sec2b<-sec2$scores[1:23,]
sec2b[4]<-sec2$scores[5,]
sec2b[5]<-sec2$scores[4,]
sec2b[22]<-sec2$scores[24,]
ch.sec<-sec2b-sec

#Test effect of neural activity on change in behavior (sectarianism)
mod1<-lm(ch.sec~HbO$A + sec.test$sect)  
summary(mod1)
mod2<-lm(ch.sec~HbO$B + sec.test$sect)  
summary(mod2)
mod3<-lm(ch.sec~HbO$C + sec.test$sect)  
summary(mod3)
mod4<-lm(ch.sec~HbO$D + sec.test$sect)  
summary(mod4)

#Test impact of age, education, and identification
dem<-surv.data[,c(11,12,25,30,35,40)]
dem1<-dem[1:23,]
dem1[4,]<-dem[5,]
dem1[5,]<-dem[4,]
dem1[22,]<-dem[24,]
dem.test<-data.frame(sec[1:23],sec1.sect,dem1)
names(dem.test)<-c("score","sect","age","educ","ID1","ID2","ID3","ID4")
mod5<-lm(HbO$C~dem.test$age + dem.test$educ + dem.test$ID3 + dem.test$sect +dem.test$score)
summary(mod5)
mod6<-lm(HbO$C~ dem.test$ID3)
summary(mod6)
ch.sec.cat<-c("d","d","d","d","d","d","d","u","d","u","d","u","u","u","d","u","u","d","u","d","d","u","d")
ch.sec.cat<-c(1,1,1,1,1,1,1,0,1,0,1,0,0,0,1,0,0,1,0,1,1,0,1)
dem.test2<-cbind(dem.test,ch.sec.cat)
names(dem.test2)<-c("score","sect","age","educ","ID1","ID2","ID3","ID4","ChSect")
mod8<-glm(factor(dem.test2$ChSect)~dem.test$age + dem.test$educ + dem.test$ID3 + dem.test$sect +dem.test$score,
                  family=binomial(link='logit'))
summary(mod8)

mod9<-lm(HbO$B~dem.test2$age + dem.test2$educ + dem.test2$ID2 + dem.test2$sect +dem.test2$score + dem.test2$ChSect)
summary(mod9)
mod10<-lm(HbO$D~dem.test2$ID4)
summary(mod10)
mod11<-lm(HbO$D~dem.test2$educ)
summary(mod11)


rm(S01,S02,S03,S04S05,S06,S07,S08,S09,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21,S22,S23)

