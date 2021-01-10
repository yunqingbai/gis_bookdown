#Read listings_detail.csv file
df<-read.csv(file.choose(),encoding="UTF-8",stringsAsFactors=F)

#Query the data that is not listed in the wrong column
df2<-subset(df,(df$instant_bookable=="t"|df$instant_bookable=="f"|df$instant_bookable=="") & 
              (df$host_has_profile_pic=="t"|df$host_has_profile_pic=="f"|df$host_has_profile_pic=="") &
              (df$has_availability=="t"|df$has_availability=="f"|df$has_availability==""))
dim(df2)
#Convert number_of_reviews to numeric
df2$number_of_reviews<-as.numeric(df2$number_of_reviews)
#Query data with number_of_reviews greater than 0
df3<-subset(df2,df2$number_of_reviews>0)
dim(df3)


#Delete columns with too many missing values
re<-numeric(74)
for(i in 1:74){
  a<-df3[,i]
  re[i]<-length(a[is.na(a)|a==""|a=="N/A"])/length(a)}
re
df4<-df3[,-which(re>0.1)]
#Output deleted column names
names(df3)[which(re>0.1)]


#Delete columns with only one level factor
re<-numeric(63)
for(i in 1:63){
  a<-df4[,i]
  re[i]<-length(unique(a))}
re
df5<-df4[,-which(re<=1)]
#Output deleted column names
names(df4)[which(re<=1)]
del<-c("listing_url","picture_url","host_id","host_url","host_name","host_since",
       "host_thumbnail_url","host_picture_url","first_review","last_review")#没有意义的列
df6<-df5[,!(names(df5)%in%del)]


#Texts which need to do text analysis
#amenities
#property_type
#host_verifications
#name
#description
#host_location
a<-character(nrow(df6))
#Text column merge
for(i in c(2,3,4,8,14,20)){a<-paste(a,df6[,i],sep=" ")}
#Text information processing
library(jiebaRD)
library(jiebaR)
library(NLP)
library(tm)
#Split vocabulary
keyword<-segment(a,worker(byline=T))
#Generate thesaurus
Thesaurus<-VCorpus(VectorSource(keyword))
#Generate document term frequency matrix
keywordmatrix<-DocumentTermMatrix(Thesaurus)
#Remove sparse word frequency
keywordmatrix2<-removeSparseTerms(keywordmatrix, sparse=0.95)
keywordmarix3<-as.matrix(keywordmatrix2)
dim(keywordmarix3)
#Delete text information column
df7<-df6[,-c(2,3,4,8,14,20)]


#bathrooms_text needs special processing of text information
unique(df7$bathrooms_text)
library(stringr)
df7$bathsum<-as.numeric(str_extract(df7$bathrooms_text,"\\d\\.{0,1}\\d{0,1}"))
df7$bathclass<-ifelse(str_detect(df7$bathrooms_text,"share"),1,0)
df7$price<-as.numeric(str_replace_all(df7$price,"\\$|,",""))
#Delete the bathrooms_text column
df8<-df7[,-12]


#Change the order of the columns to make the factor type and the data set separate
df8<-df8[,c(1,2,5:7,37,10,3:4,8:9,11:26,28:36,38:44,27)]
#Turn to factor variable
for(i in 2:7){df8[,i]<-as.factor(df8[,i])}
#Turn to numeric variable
for(i in 8:41){df8[,i]<-as.numeric(df8[,i])}
#Merge data set and word frequency matrix
df9<-cbind(df8,keywordma3)
keywordma3[1:100,1:6]

#Delete lines with missing values
df10<-na.omit(df9)
dim(df10)
df10<-df10[order(-df10$number_of_reviews),]
df10$number_of_reviews<-rep(c(1,0),c(100,10120))
names(df10)[44]<-"istop100"
df10<-df10[sample(nrow(df10),nrow(df10)),]
write.csv(df10,"cleaned_data.csv",row.names=F)


#Read cleaned_data.csv file
df<-read.csv("cleaned_data.csv")
dim(df)
summary(df)
str(df)
library(ggplot2)
#Factor Drawing
for(i in 2:7){
  png(paste(i,names(df)[i],".png",sep=""))
  print({
    ggplot()+geom_bar(aes(x=df[,i],fill=as.factor(df$istop100)),position="fill")+
      labs(x=names(df)[i])+coord_flip()
  })
  dev.off()
}
#Numerical variables
for(i in 8:43){
  png(paste(i,".0",names(df)[i],".png",sep=""))
  print({
    ggplot()+geom_density(aes(x=df[,i],color=as.factor(df$istop100)))+
      labs(x=paste(i,names(df)[i],sep=" "))
  })
  dev.off()
}
#Logarithmic Numerical Variables
for(i in c(8,9,13,15,16,18,19,20,21,22,23,29,37,38,39,40,42)){
  png(paste(i,".1",names(df)[i],".png",sep=""))
  print({
    ggplot()+geom_density(aes(x=log(df[,i]+1),color=as.factor(df$istop100)))+
      labs(x=paste(i,"log",names(df)[i],sep=" "))
  })
  dev.off()
}
#Text
for(i in 45:434){
  png(paste(i,names(df)[i],".png",sep=""))
  print({
    ggplot()+geom_bar(aes(x=df[,i],fill=as.factor(df$istop100)),position="fill")+
      labs(x=paste(i,names(df)[i],sep=" "))+coord_flip()
  })
  dev.off()
}


#Load drawing-related packages
library(jsonlite)
install.packages("tidyverse")
library(tidyverse)
library("jsonlite")
library("ggplot2")
library(plyr)
library(dplyr)
install.packages("leaflet")
library(leaflet)
#Read map data neighborhood.geojson
geoData2 <- readLines(file.choose(),warn=FALSE) %>%
  paste(collapse = "\n")%>%fromJSON(simplifyVector = FALSE)
#Non-top100 points
other <- df %>% 
  filter(istop100 == 0)
#top100 points
top_100 <- df %>% 
  filter(istop100 == 1) 
#Drawing a map
leaflet() %>% setView(lng = 151.1, lat = -33.8,zoom=10) %>%
  addCircleMarkers(lng = other$longitude, lat = other$latitude,radius = 2, 
                   stroke = FALSE,color = "green",fillOpacity = 0.5, group = "Other") %>%
  addCircleMarkers(lng = top_100$longitude, lat = top_100$latitude, radius = 3, 
                   stroke = FALSE,color = "red",fillOpacity = 0.9, group = "Top 100")%>%
  addGeoJSON(geoData2)


#Use random number seed 123 for stratified sampling
dfistop100<-subset(df,df$istop100==1)
dfnotop100<-subset(df,df$istop100==0)
set.seed(123)
sam1<-sample(100,60)
set.seed(123)
sam2<-sample(nrow(dfnotop100),nrow(dfnotop100)*0.6)
dftrain<-rbind(dfistop100[sam1,],dfnotop100[sam2,])
dftest<-rbind(dfistop100[-sam1,],dfnotop100[-sam2,])

library(gbm)
modelgbm<-gbm(istop100~.,dftrain[,-1],distribution = "bernoulli",interaction.depth=6,
              shrinkage=0.01,n.trees=500,n.cores=6,cv.fold=3)
bestntrees<-gbm.perf(modelgbm,method="cv")
summary(modelgbm,n.trees=bestntrees)
dfht<-summary(modelgbm,n.trees=bestntrees)
ggplot(aes(x=reorder(var,rel.inf),y=rel.inf),data=dfht[1:30,])+
  geom_bar(stat="identity",fill="blue")+coord_flip() #Independent variable importance drawing
head(dfht,40)
pretest<-predict(modelgbm,n.trees=bestntrees,dftest)
library(pROC)
roc1<-roc(dftest$istop100,pretest,print.auc=T,plot=T)
bestdd<-coords(roc1,"best")[1,1]
ma<-table(dftest$istop100,ifelse(pretest>bestdd,1,0))
ma
sum(diag(ma))/sum(ma)
coords(roc1,"best")

