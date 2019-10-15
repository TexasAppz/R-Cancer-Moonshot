### Cancer Moonshot ###
####################### Question 1 & 2 ######################################################################
mydata <- read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Assignment4_5/EMIS5357-7357-FL18-Assignments45_3/Cancer_Data12A.csv")
str(mydata)
set.seed(1000)
# extracting the year from the date of patent filing
# other methods are possible, such as using package lubridate
yt <- strptime(mydata$Filing_Date, format ='%Y%m%d')
mydata$year <- as.integer(format(yt,"%Y"))
time_assigned = 1976
mydata=subset(mydata,mydata$year>=time_assigned)

### Preparing the Document Term Matrix for clustering
# loading and installing the TM package for text mining
#install.packages("tm") 
library(ggplot2)
library(cluster)
library(factoextra) 
library(caret) 
library(lattice)
library(tm)
library(NbClust)


### creating the document term matrix (DTM) on the titles of the patent
mydatatitle <-mydata$Patent_Title
ndocs <-length(mydatatitle)
#setting the parameters below helps keep the DTM of reasonable size
minf=ndocs*0.01 # minimum occurrence of words to be used in the DTM
#minf 2693.53
maxf=ndocs*0.40 # maximum occurrence of words to be used in the DTM
#maxf 107741.2

# putting the input data in a format the main function for DTM can read
corpus=Corpus(VectorSource(mydatatitle))

# main command line to create the DTM with parameters listed in control
dtm = DocumentTermMatrix(corpus,control=list(stopwords=TRUE, wordLengths=c(4,25),removePunctuation = FALSE,removeNumbers = FALSE, bounds = list(global=c(minf,maxf))))
dtm
#display info on the DTM
inspect(dtm)

# outputting the DTM to a CSV file if we want
dtmm=as.matrix(dtm)
dtmm
write.csv(dtmm,"/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Assignment4_5/EMIS5357-7357-FL18-Assignments45_3/cancerdtm.csv")

# exploring the data
# finding the most frequent words in the DTM matrix
FreqMat <- data.frame(ST = colnames(dtmm), Freq = colSums(dtmm))
FreqMat
y <- as.vector(order(-FreqMat$Freq))
FreqMat[y,]
head(FreqMat[y,],n=10)
# with n=10 we get a lot of useless words like "method" and "methods"
head(FreqMat[y,],n=30)

# adding the DTMs to the rest of the data frame and outputting that
bigdata <- cbind(mydata$Family_ID,mydata$Patent_or_Publication_ID,as.data.frame(as.matrix(dtm)),make.row.names=TRUE)


# if you want to output your big data frame to a csv file 
write.csv(bigdata,"/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Assignment4_5/EMIS5357-7357-FL18-Assignments45_3/cancerbigdtm.csv")

# now we can start clustering using the DTM!

#########################################
### K-means clustering
# for question (2)
# nc is the number of clusters you want to have
#nc <- 10
bigdata<-bigdata[c(-1,-2)]
preproc_cancer = preProcess(dtmm) #preparation of the transformation of the data (scaling, centering)
ClusterNorm_cancer = predict(preproc_cancer, dtmm) #applying the transformation to a data set
str(ClusterNorm_cancer)

#write.csv(bigdata,"/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Assignment4_5/EMIS5357-7357-FL18-Assignments45_3/bigdata.csv")

kmc.cancer = kmeans(ClusterNorm_cancer, centers = 10)
table(kmc.cancer$cluster)
#1      2      3      4      5      6      7      8      9     10 
#8732   8333   7183   4133   6079   3811   3759   8740 209107   8623

#kmc.cancer$centers
#kmc.cancer$size
#str(kmc.cancer)
#kmc.cancer


#K-means clustering with 3 clusters of sizes 9, 7, 16
#--- lengthy output omitted

#kmc.cancer= # FINISH THIS LINE OF CODE TO DO K-MEANS CLUSTERING WITH nc CLUSTERS
  


# below I provide a code that gives you the most frequent words per cluster
# I made a loop so that it's not too painful to do for many clusters
#mu_cancer = preproc_cancer$mean
#mu_cancer
nb = 10 # most common words
nc = 10
topwords <-matrix(0,nrow=nc,ncol=nb)
for (i in 1:nc){
  clusterdata = subset(as.data.frame(dtmm), kmc.cancer$cluster == i)
  vv <- as.data.frame(tail(sort(colMeans(clusterdata)), n=nb))
  topwords[i,]<- rownames(vv)
}
topwords
#[,1]         [,2]           [,3]         [,4]             [,5]          [,6]             [,7]           [,8]           [,9]          [,10]        
#[1,] "uses"       "compositions" "method"     "receptor"       "human"       "thereof"        "methods"      "kinase"       "inhibitors"  "protein"    
#[2,] "using"      "cancer"       "human"      "thereof"        "method"      "methods"        "anti"         "delivery"     "antibody"    "drug"       
#[3,] "thereof"    "human"        "molecules"  "proteins"       "methods"     "polypeptides"   "encoding"     "acid"         "acids"       "nucleic"    
#[4,] "diseases"   "derivatives"  "method"     "compounds"      "related"     "compositions"   "treating"     "methods"      "treatment"   "disorders"  
#[5,] "compounds"  "containing"   "production" "pharmaceutical" "producing"   "derivatives"    "preparing"    "thereof"      "preparation" "process"    
#[6,] "using"      "uses"         "compounds"  "substituted"    "thereof"     "treatment"      "methods"      "derivatives"  "receptor"    "antagonists"
#[7,] "method"     "thereof"      "protein"    "treating"       "tumor"       "treatment"      "cancer"       "diseases"     "methods"     "associated" 
#[8,] "antibodies" "uses"         "thereof"    "cell"           "receptor"    "tumor"          "methods"      "peptides"     "growth"      "factor"     
#[9,] "uses"       "inhibitors"   "treatment"  "derivatives"    "compounds"   "cancer"         "compositions" "thereof"      "method"      "methods"    
#[10,] "compounds"  "thereof"      "cancer"     "method"         "composition" "pharmaceutical" "methods"      "compositions" "based"       "comprising" 


nb = 15 # 15 most frequent words per cluster
nc = 10
topwords <-matrix(0,nrow=nc,ncol=nb)
for (i in 1:nc){
  clusterdata = subset(as.data.frame(dtmm), kmc.cancer$cluster == i)
  vv <- as.data.frame(tail(sort(colMeans(clusterdata)), n=nb))
  topwords[i,]<- rownames(vv)
}
topwords

nb = 2 # when f = 2, most frequent words per cluster
nc = 10
topwords <-matrix(0,nrow=nc,ncol=nb)
for (i in 1:nc){
  clusterdata = subset(as.data.frame(dtmm), kmc.cancer$cluster == i)
  vv <- as.data.frame(tail(sort(colMeans(clusterdata)), n=nb))
  topwords[i,]<- rownames(vv)
}
topwords

####################################### QUESTION 3 #################################################################
# For this questions, it was required to minize the size of the data so that we could calculate the most optimal number of clusters
# Hence, we are only using data for 2015, and 2016

mydata <- read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Assignment4_5/EMIS5357-7357-FL18-Assignments45_3/Cancer_Data12A.csv")
str(mydata)
set.seed(1000)
yt <- strptime(mydata$Filing_Date, format ='%Y%m%d')
mydata$year <- as.integer(format(yt,"%Y"))
time_assigned = 2015
mydata=subset(mydata,mydata$year>=time_assigned)
mydatatitle <-mydata$Patent_Title
ndocs <-length(mydatatitle)
minf=ndocs*0.01 # minimum occurrence of words to be used in the DTM
maxf=ndocs*0.40 # maximum occurrence of words to be used in the DTM
corpus=Corpus(VectorSource(mydatatitle))
dtm = DocumentTermMatrix(corpus,control=list(stopwords=TRUE, wordLengths=c(4,25),removePunctuation = FALSE,removeNumbers = FALSE, bounds = list(global=c(minf,maxf))))
dtm
inspect(dtm)
dtmm=as.matrix(dtm)
dtmm
preproc_cancer = preProcess(dtmm) #preparation of the transformation of the data (scaling, centering)
ClusterNorm_cancer = predict(preproc_cancer, dtmm) #applying the transformation to a data set
str(ClusterNorm_cancer)
kmc.cancer = kmeans(ClusterNorm_cancer, centers = 10)
kmax <- 10 # Assuming this is the max number of custer
fviz_nbclust(ClusterNorm_cancer, FUN = kmeans, method = "wss", k.max=kmax, nstart=5) # default k.max is 10
fviz_nbclust(ClusterNorm_cancer, FUN = kmeans, method = "wss", k.max=kmax) # default k.max is 10
# k=?  --> Error: vector memory exhausted (limit reached?)
fviz_nbclust(ClusterNorm_cancer, FUN = kmeans, method = "silhouette", k.max=kmax, nstart=5)
fviz_nbclust(ClusterNorm_cancer, FUN = kmeans, method = "silhouette", k.max=kmax)
# k=?  --> Error: vector memory exhausted (limit reached?)

nc_cancer <- NbClust(ClusterNorm_cancer, distance="euclidean", min.nc=2, max.nc=kmax, method="ward.D2")
# Error: vector memory exhausted (limit reached?)
length(unique(nc_cancer$Best.partition)) # returns optimal number of clusters as selected by majority

#Assuming k = 2 is the most optimal number of cluster, now we will calculate the size of each cluster:
kmc.cancer = kmeans(ClusterNorm_cancer, centers = 2)
table(kmc.cancer$cluster)

nb = 2 # 15 most frequent words per cluster
nc = 2
topwords <-matrix(0,nrow=nc,ncol=nb)
for (i in 1:nc){
  clusterdata = subset(as.data.frame(dtmm), kmc.cancer$cluster == i)
  vv <- as.data.frame(tail(sort(colMeans(clusterdata)), n=nb))
  topwords[i,]<- rownames(vv)
}
topwords

################################## for question (4)  ###############################################
# below we identify all the rows where FDA_Applicant contains "NOVARTIS"
mydata <- read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Assignment4_5/EMIS5357-7357-FL18-Assignments45_3/Cancer_Data12A.csv")
str(mydata)
set.seed(1000)
yt <- strptime(mydata$Filing_Date, format ='%Y%m%d')
mydata$year <- as.integer(format(yt,"%Y"))
#time_assigned = 1976
#mydata=subset(mydata,mydata$year>=time_assigned)
indicesnovartis=grep("NOVARTIS",mydata$FDA_Applicant)
mydatanova=mydata[indicesnovartis,]
mydatanovartis=subset(mydatanova,mydatanova$year>=2015)
str(mydatanovartis)

#####################
mydata_FDA <-mydatanovartis$FDA_Applicant
mydata_FDA
ndocs_FDA <-length(mydata_FDA)
ndocs_FDA
#81
#setting the parameters below helps keep the DTM of reasonable size
minf_FDA=ndocs_FDA*0.01 # minimum occurrence of words to be used in the DTM
minf_FDA
#minf 0.81
maxf_FDA=ndocs_FDA*0.40 # maximum occurrence of words to be used in the DTM
maxf_FDA
#32.4

corpus_FDA=Corpus(VectorSource(mydata_FDA))
corpus_FDA
# main command line to create the DTM with parameters listed in control
dtm_FDA = DocumentTermMatrix(corpus_FDA,control=list(stopwords=TRUE, wordLengths=c(4,25),removePunctuation = FALSE,removeNumbers = FALSE, bounds = list(global=c(minf_FDA,maxf_FDA))))
dtm_FDA
#display info on the DTM
inspect(dtm_FDA)

# outputting the DTM to a CSV file if we want
dtmm_FDA=as.matrix(dtm_FDA)
dtmm_FDA
#write.csv(dtmm_FDA,"/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Assignment4_5/EMIS5357-7357-FL18-Assignments45_3/cancerdtm_FDA.csv")

# exploring the data
# finding the most frequent words in the DTM matrix
FreqMat_FDA <- data.frame(ST = colnames(dtmm_FDA), Freq = colSums(dtmm_FDA))
FreqMat_FDA
y_FDA <- as.vector(order(-FreqMat_FDA$Freq))
FreqMat_FDA[y_FDA,]

head(FreqMat_FDA[y_FDA,],n=10)
# with n=10 we get a lot of useless words like "method" and "methods"
head(FreqMat_FDA[y_FDA,],n=30)

# adding the DTMs to the rest of the data frame and outputting that
bigdata_FDA <- cbind(mydatanovartis$Family_ID,mydatanovartis$FDA_Applicant,as.data.frame(as.matrix(dtm_FDA)),make.row.names=TRUE)

# if you want to output your big data frame to a csv file 
write.csv(bigdata_FDA,"/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Assignment4_5/EMIS5357-7357-FL18-Assignments45_3/cancerbigdtm_FDA.csv")

bigdata_FDA<-bigdata_FDA[c(-1,-2)]
bigdata_FDA
preproc_cancer_FDA = preProcess(dtmm_FDA) #preparation of the transformation of the data (scaling, centering)
preproc_cancer_FDA
ClusterNorm_cancer_FDA = predict(preproc_cancer_FDA, dtmm_FDA) #applying the transformation to a data set
ClusterNorm_cancer_FDA
str(ClusterNorm_cancer_FDA)
#write.csv(bigdata_FDA,"/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Assignment4_5/EMIS5357-7357-FL18-Assignments45_3/bigdata_FDA.csv")

kmc.cancer_FDA = kmeans(ClusterNorm_cancer_FDA, centers = 7)
table(kmc.cancer_FDA$cluster)
#1  2  3  4  5  6  7 
#1  7  6  1  3 11 52  

nb_FDA = 6 # most common words
nc_FDA = 7
topwords_FDA <-matrix(0,nrow=nc_FDA,ncol=nb_FDA)
for (i in 1:nc_FDA){
  clusterdata_FDA = subset(as.data.frame(dtmm_FDA), kmc.cancer_FDA$cluster == i)
  vv_FDA <- as.data.frame(tail(sort(colMeans(clusterdata_FDA)), n=nb_FDA))
  topwords_FDA[i,]<- rownames(vv_FDA)
}
topwords_FDA

#       [,1]      [,2]              [,3]              [,4]              [,5]       [,6]             
#[1,] "pharm"   "beecham"         "smithkline"      "glaxo"           "wellcome" "glaxosmithkline"
#[2,] "pharm"   "beecham"         "smithkline"      "glaxo"           "wellcome" "glaxosmithkline"
#[3,] "pharm"   "glaxosmithkline" "glaxo"           "wellcome"        "beecham"  "smithkline"     
#[4,] "pharm"   "beecham"         "smithkline"      "glaxosmithkline" "glaxo"    "wellcome"       
#[5,] "beecham" "smithkline"      "glaxosmithkline" "glaxo"           "wellcome" "pharm"          
#[6,] "pharm"   "beecham"         "smithkline"      "glaxo"           "wellcome" "glaxosmithkline"
#[7,] "pharm"   "beecham"         "smithkline"      "glaxosmithkline" "glaxo"    "wellcome" 

#library(cluster)
#library(factoextra)
kmax <- 7
fviz_nbclust(ClusterNorm_cancer_FDA, FUN = kmeans, method = "wss", k.max=kmax)
#k = unknown
fviz_nbclust(ClusterNorm_cancer_FDA, FUN = kmeans, method = "wss", k.max=kmax, nstart=2) 
#k = unknown
fviz_nbclust(ClusterNorm_cancer_FDA, FUN = kmeans, method = "silhouette", k.max=kmax)
# k=7
fviz_nbclust(ClusterNorm_cancer_FDA, FUN = kmeans, method = "silhouette", k.max=kmax, nstart=5)
# k=7

gap_stat_FDA <- clusGap(ClusterNorm_cancer_FDA, FUN = kmeans, nstart = 25, K.max = kmax, B = 10) # B=50 better
fviz_gap_stat(gap_stat_FDA)
# k= 7

#########  Question 5  ###############################################
# below we identify all the rows where FDA_Applicant contains "NOVARTIS"
mydata <- read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Assignment4_5/EMIS5357-7357-FL18-Assignments45_3/Cancer_Data12A.csv")
str(mydata)
set.seed(1000)
yt <- strptime(mydata$Filing_Date, format ='%Y%m%d')
mydata$year <- as.integer(format(yt,"%Y"))
indicesnovartis=grep("NOVARTIS",mydata$FDA_Applicant)
mydatanova=mydata[indicesnovartis,]
mydatanovartis=subset(mydatanova,mydatanova$year>=2011)
str(mydatanovartis)
mydata_FDA <-mydatanovartis$FDA_Applicant
mydata_FDA
ndocs_FDA <-length(mydata_FDA)
ndocs_FDA
#81
#setting the parameters below helps keep the DTM of reasonable size
minf_FDA=ndocs_FDA*0.01 # minimum occurrence of words to be used in the DTM
minf_FDA
#minf 0.05
maxf_FDA=ndocs_FDA*0.4 # maximum occurrence of words to be used in the DTM
maxf_FDA
#8
corpus_FDA=Corpus(VectorSource(mydata_FDA))
corpus_FDA
dtm_FDA = DocumentTermMatrix(corpus_FDA,control=list(stopwords=TRUE, wordLengths=c(4,25),removePunctuation = FALSE,removeNumbers = FALSE, bounds = list(global=c(minf_FDA,maxf_FDA))))
dtmm_FDA=as.matrix(dtm_FDA)
FreqMat_FDA <- data.frame(ST = colnames(dtmm_FDA), Freq = colSums(dtmm_FDA))
FreqMat_FDA
y_FDA <- as.vector(order(-FreqMat_FDA$Freq))
FreqMat_FDA[y_FDA,]
preproc_cancer_FDA = preProcess(dtmm_FDA) #preparation of the transformation of the data (scaling, centering)
preproc_cancer_FDA
ClusterNorm_cancer_FDA = predict(preproc_cancer_FDA, dtmm_FDA) #applying the transformation to a data set
ClusterNorm_cancer_FDA
str(ClusterNorm_cancer_FDA)
kmc.cancer_FDA = kmeans(ClusterNorm_cancer_FDA, centers = 3)
table(kmc.cancer_FDA$cluster)
#1  2  3  4  5  6  7 
#11  3  1  1  6 52  7   

nb_FDA = 2 # most common words
nc_FDA = 3
topwords_FDA <-matrix(0,nrow=nc_FDA,ncol=nb_FDA)
for (i in 1:nc_FDA){
  clusterdata_FDA = subset(as.data.frame(dtmm_FDA), kmc.cancer_FDA$cluster == i)
  vv_FDA <- as.data.frame(tail(sort(colMeans(clusterdata_FDA)), n=nb_FDA))
  topwords_FDA[i,]<- rownames(vv_FDA)
}
topwords_FDA
#[,1]                     [,2]             
#[1,] "glaxosmithkline" "pharm"          
#[2,] "pharm"           "glaxosmithkline"
#[3,] "pharm"           "glaxosmithkline"

kmax <- 3
fviz_nbclust(ClusterNorm_cancer_FDA, FUN = kmeans, method = "wss", k.max=kmax)
#k = unknown
fviz_nbclust(ClusterNorm_cancer_FDA, FUN = kmeans, method = "wss", k.max=kmax, nstart=2) 
#k = unknown
fviz_nbclust(ClusterNorm_cancer_FDA, FUN = kmeans, method = "silhouette", k.max=kmax)
# k=3
fviz_nbclust(ClusterNorm_cancer_FDA, FUN = kmeans, method = "silhouette", k.max=kmax, nstart=5)
# k=3

gap_stat_FDA <- clusGap(ClusterNorm_cancer_FDA, FUN = kmeans, nstart = 25, K.max = kmax, B = 10) # B=50 better
fviz_gap_stat(gap_stat_FDA)
# k= 3

nc_cancer <- NbClust(ClusterNorm_cancer_FDA, distance="euclidean", min.nc=2, max.nc=kmax, method="ward.D2")
# Error: vector memory exhausted (limit reached?)
length(unique(nc_cancer$Best.partition)) # returns optimal number of clusters as selected by majority

####################################################################################################
################################### Question # 6  ##################################################
mydata <- read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Assignment4_5/EMIS5357-7357-FL18-Assignments45_3/Cancer_Data12A.csv")
str(mydata)
set.seed(1000)
yt <- strptime(mydata$Filing_Date, format ='%Y%m%d')
mydata$year <- as.integer(format(yt,"%Y"))
mydata_1999=subset(mydata,mydata$year>=2015)
str(mydata_1999)

Inventive <-mydata_1999$CPC_Inventive
Inventive
head(Inventive)

n_Inventive <-length(Inventive)
n_Inventive
# 7296

#setting the parameters below helps keep the DTM of reasonable size
minf_Inventive=n_Inventive*0.01 # minimum occurrence of words to be used in the DTM
minf_Inventive
#minf 72.96
maxf_Inventive=n_Inventive*0.40 # maximum occurrence of words to be used in the DTM
maxf_Inventive
#maxf 2918.4

# putting the input data in a format the main function for DTM can read
corpus_Inventive=Corpus(VectorSource(Inventive))
corpus_Inventive

# main command line to create the DTM with parameters listed in control
dtm_Inventive = DocumentTermMatrix(corpus_Inventive,control=list(stopwords=TRUE, wordLengths=c(4,25),removePunctuation = FALSE,removeNumbers = FALSE, bounds = list(global=c(minf_Inventive,maxf_Inventive))))
dtm_Inventive
inspect(dtm_Inventive)


# outputting the DTM to a CSV file if we want
dtmm_Inventive=as.matrix(dtm_Inventive)
str(dtmm_Inventive)

write.csv(dtmm_Inventive,"/Users/jaimegarcia/Desktop/dtmm_Inventive.csv")
FreqMat_Inventive <- data.frame(ST = colnames(dtmm_Inventive), Freq = colSums(dtmm_Inventive))
FreqMat_Inventive
y_Inventive <- as.vector(order(-FreqMat_Inventive$Freq))
FreqMat_Inventive[y_Inventive,]
head(FreqMat_Inventive[y_Inventive,], n= 10)

#head(FreqMat_Inventive[y_Inventive,],n=10)
# with n=10 we get a lot of useless words like "method" and "methods"
#head(FreqMat_Inventive[y_Inventive,],n=30)

# adding the DTMs to the rest of the data frame and outputting that
#bigdata_Inventive <- cbind(mydata_1999$Family_ID,mydata_1999$CPC_Inventive,as.data.frame(as.matrix(dtmm_Inventive)),make.row.names=TRUE)
#bigdata_Inventive
# if you want to output your big data frame to a csv file 
#write.csv(bigdata_Inventive,"/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Assignment4_5/EMIS5357-7357-FL18-Assignments45_3/cancerbigdata_Inventive.csv")

#bigdata_Inventive<-bigdata_Inventive[c(-1,-2)]
#bigdata_Inventive


preproc_Inventive = preProcess(dtmm_Inventive) #preparation of the transformation of the data (scaling, centering)
preproc_Inventive
ClusterNorm_Inventive = predict(preproc_Inventive, dtmm_Inventive) #applying the transformation to a data set
ClusterNorm_Inventive
str(ClusterNorm_Inventive)

#write.csv(bigdata_FDA,"/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Assignment4_5/EMIS5357-7357-FL18-Assignments45_3/bigdata_FDA.csv")

kmc.cancer_Inventive = kmeans(ClusterNorm_Inventive, centers = 7)
table(kmc.cancer_Inventive$cluster)
#1    2    3    4    5    6    7 
#392  101   93  115 6454   47   94 

nb_Inventive = 7 # most common words
nc_Inventive = 7
topwords_Inventive <-matrix(0,nrow=nc_Inventive,ncol=nb_Inventive)
for (i in 1:nc_Inventive){
  clusterdata_Inventive = subset(as.data.frame(dtmm_Inventive), kmc.cancer_Inventive$cluster == i)
  vv_Inventive <- as.data.frame(tail(sort(colMeans(clusterdata_Inventive)), n=nb_Inventive))
  topwords_Inventive[i,]<- rownames(vv_Inventive)
}
topwords_Inventive
#[,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]    
#[1,] "c07d409" "c07d417" "c07d403" "c07d413" "c07d405" "c07d401" "a61k31"
#[2,] "g06f19"  "6886"    "c12q1"   "c07k16"  "a61k31"  "57484"   "g01n33"
#[3,] "c07k14"  "a61k38"  "a61k39"  "a61k31"  "2866"    "a61k9"   "c07k16"
#[4,] "a61k45"  "a61k47"  "a61k38"  "0053"    "a61k35"  "a61k9"   "a61k31"
#[5,] "a61k39"  "a61k9"   "a61k38"  "g01n33"  "a61k47"  "c07k16"  "a61k31"
#[6,] "c07d207" "c07d307" "c07d209" "c07d231" "c07d295" "c07d213" "a61k31"
#[7,] "a61k31"  "a61n5"   "g06f19"  "a61b6"   "a61b5"   "0012"    "g06t7" 

kmax <- 7
fviz_nbclust(ClusterNorm_Inventive, FUN = kmeans, method = "wss", k.max=kmax)
#k = unknown
fviz_nbclust(ClusterNorm_Inventive, FUN = kmeans, method = "wss", k.max=kmax, nstart=1) 
#k = unknown
fviz_nbclust(ClusterNorm_Inventive, FUN = kmeans, method = "silhouette", k.max=kmax)
# k=3
fviz_nbclust(ClusterNorm_Inventive, FUN = kmeans, method = "silhouette", k.max=kmax, nstart=1)
# k=3

gap_stat_Inventive <- clusGap(ClusterNorm_Inventive, FUN = kmeans, nstart = 1, K.max = kmax, B = 3) # B=50 better
fviz_gap_stat(gap_stat_Inventive)
# k= 3

#library(NbClust)
nc_Inventive <- NbClust(ClusterNorm_Inventive, distance="euclidean", min.nc=2, max.nc=kmax, method="ward.D2")
# Error: vector memory exhausted (limit reached?)
length(unique(nc_Inventive$Best.partition)) # returns optimal number of clusters as selected by majority

nc_Inventive <- NbClust(ClusterNorm_Inventive, distance="euclidean", min.nc=2, max.nc=kmax, method="ward.D2")
# Error: vector memory exhausted (limit reached?)
length(unique(nc_cancer$Best.partition)) # returns optimal number of clusters as selected by majority

##### Repeating the same process with FDA_Applicants #######################################################
mydata2 <- read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Assignment4_5/EMIS5357-7357-FL18-Assignments45_3/FDA_Applicant.csv")
str(mydata2)
set.seed(1000)

Applicant <-mydata2$FDA_Applicant
Applicant
str(Applicant)
head(Applicant)

n_Applicant <-length(Applicant)
n_Applicant
# 1090

minf_Applicant=n_Applicant*0.01 # minimum occurrence of words to be used in the DTM
minf_Applicant
#minf 10.9
maxf_Applicant=n_Applicant*0.9 # maximum occurrence of words to be used in the DTM
maxf_Applicant
#maxf 981

# putting the input data in a format the main function for DTM can read
corpus_Applicant=Corpus(VectorSource(Applicant))
corpus_Applicant

# main command line to create the DTM with parameters listed in control
dtm_Applicant = DocumentTermMatrix(corpus_Applicant,control=list(stopwords=TRUE, wordLengths=c(4,25),removePunctuation = FALSE,removeNumbers = FALSE, bounds = list(global=c(minf_Applicant,maxf_Applicant))))
dtm_Applicant
inspect(dtm_Applicant)


# outputting the DTM to a CSV file if we want
dtmm_Applicant=as.matrix(dtm_Applicant)
str(dtmm_Applicant)

FreqMat_Applicant <- data.frame(ST = colnames(dtmm_Applicant), Freq = colSums(dtmm_Applicant))
FreqMat_Applicant
y_Applicant <- as.vector(order(-FreqMat_Applicant$Freq))
FreqMat_Applicant[y_Applicant,]
head(FreqMat_Applicant[y_Applicant,], n= 10)


preproc_Applicant = preProcess(dtmm_Applicant) #preparation of the transformation of the data (scaling, centering)
preproc_Applicant
ClusterNorm_Applicant = predict(preproc_Applicant, dtmm_Applicant) #applying the transformation to a data set
ClusterNorm_Applicant
str(ClusterNorm_Applicant)
#'data.frame':	num 1:33777, 1:88

kmc.cancer_Applicant = kmeans(ClusterNorm_Applicant, centers = 7)
table(kmc.cancer_Applicant$cluster)
#1   2   3   4   5   6   7 
#237  13  11  76 710  28  15

nb_Applicant = 5 # most common words
nc_Applicant = 7
topwords_Applicant <-matrix(0,nrow=nc_Applicant,ncol=nb_Applicant)
for (i in 1:nc_Applicant){
  clusterdata_Applicant = subset(as.data.frame(dtmm_Applicant), kmc.cancer_Applicant$cluster == i)
  vv_Applicant <- as.data.frame(tail(sort(colMeans(clusterdata_Applicant)), n=nb_Applicant))
  topwords_Applicant[i,]<- rownames(vv_Applicant)
}
topwords_Applicant
#     [,1]            [,2]              [,3]            [,4]          [,5]        
#[1,] "wyeth"         "salix"           "valeant"       "astrazeneca" "pharms"    
#[2,] "sharp"         "vertex"          "pharmacyclics" "pharmacia"   "upjohn"    
#[3,] "pharms"        "novartis"        "corp"          "beecham"     "smithkline"
#[4,] "genzyme"       "glaxosmithkline" "pharms"        "corp"        "novartis"  
#[5,] "abbvie"        "labs"            "celgene"       "hlthcare"    "merck"     
#[6,] "sharp"         "vertex"          "pharmacyclics" "pharms"      "takeda"    
#[7,] "pharmacyclics" "janssen"         "prods"         "endocrine"   "abbvie"   


kmax <- 7
fviz_nbclust(ClusterNorm_Applicant, FUN = kmeans, method = "wss", k.max=kmax)
#k = unknown
fviz_nbclust(ClusterNorm_Applicant, FUN = kmeans, method = "wss", k.max=kmax, nstart=1) 
#k = unknown
fviz_nbclust(ClusterNorm_Applicant, FUN = kmeans, method = "silhouette", k.max=kmax)
# k=3
fviz_nbclust(ClusterNorm_Applicant, FUN = kmeans, method = "silhouette", k.max=kmax, nstart=1)
# k=3

gap_stat_Inventive <- clusGap(ClusterNorm_Applicant, FUN = kmeans, nstart = 1, K.max = kmax, B = 3) # B=50 better
fviz_gap_stat(gap_stat_Inventive)
# k= 3

#### COMBINING THE 2 DTM's
bigdata_FDA <- cbind(mydata$Inventive,as.data.frame(as.matrix(dtm_Applicant)),mydata2$Applicant,as.data.frame(as.matrix(dtm_Inventive)),make.row.names=TRUE)
# Error in data.frame(..., check.names = FALSE) : 
#arguments imply differing number of rows: 0, 1090, 7296, 1

#Unfortunetely, These two data frames cannot be cbind because they have different number of rows.


