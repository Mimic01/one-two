library(stringr)
library(qdapRegex)
library(stringdist)
library(plyr)
library(qdap)
setwd("C:/Users/Alex/Documents/Sentiment_Analysis/Corpus")
corps=read.csv("Corpus_Oficial_fixed.csv",header=TRUE,encoding="UTF-8",stringsAsFactors=FALSE,sep=",")
#cleanString = gsub("[[:alnum:] ]", "", dirtyString)
colnames(corps)
#Transformamos el nombre de ID por simpleza
colnames(corps)[colnames(corps)=="X.U.FEFF.IDS"]<-"IDS"
unx<-unlist(corps)
#mode(unx)
#length(unx)
#head(unx)
#Split de cada ID
corps_split=split(corps,f=corps$IDS)
#Root tweets (dividido por IDs) FRASES ENTERAS DE ROOTS
roots<-lapply(corps_split,'[',1,2)
#Replies tweets (dividido por IDs) FRASES ENTERAS DE REPLIES
reps<-lapply(corps_split,'[', ,3)
#Sacamos IDE & CAD de tweets roots
#root_ide<-lapply(roots,ex_between,c("<COREF",">"),c(">","</COREF>"),extract=TRUE)
root_tw<-lapply(roots,rm_between,c("<COREF"),c("/COREF>"),extract=TRUE)
#Sacamos CAD & IDE de replies (divididos por ID)
#reply_ide<-lapply(reps,ex_between,c("<COREF",">"),c(">","</COREF>"),extract=TRUE)
reply_tw<-lapply(reps,rm_between,c("<COREF"),c("/COREF>"),extract=TRUE)
#Extraemos por parte
#rootcads mas largo es de 8
rootcads<-lapply(root_ide,str_extract_all,"CAD=[0-9]",TRUE)
replycads<-lapply(reply_ide,str_extract_all,"CAD=[0-9]",TRUE)

rootides<-lapply(root_ide,str_extract_all,"IDE=[1-9]",TRUE)
replyides<-lapply(reply_ide,str_extract_all,"IDE=[1-9]",TRUE)

rootwords<-rm_between(root_ide,c(">"),c("<"),extract=TRUE)
replywords<-rm_between(reply_ide,c(">"),c("<"),extract=TRUE)
###############################################################################################################
grepl(vec2,vec1,fixed=TRUE)

#########
nti<-identical(rootcads[[1]][1],replycads[[1]][2,1])
if(nti==TRUE){
  #
  vctr<-c(rootcads[[1]][1],rootides[[1]][1],replyides[[1]][2,1],rootwords[[1]][1],replywords[[1]][1])
}
vctr

#vc1<-mapply(identical,rootcads[[1]][1],replycads[[1]][1,])
#####
nam<-grepl(vec2,vec1,fixed=TRUE)

