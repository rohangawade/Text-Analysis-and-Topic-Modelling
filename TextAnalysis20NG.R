#import
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(NbClust)
library(fpc)
library(cluster)
library(factoextra)
library(lsa)
library(lda)
library(topicmodels)
#require(tm)



#list of groups that we are selecting
folderlist <-c("D://IITC//Study//Spring 2017//CS 522//hw1//20news-19997//20_newsgroups//alt.atheism",
               "D://IITC//Study//Spring 2017//CS 522//hw1//20news-19997//20_newsgroups//comp.graphics",
               "D://IITC//Study//Spring 2017//CS 522//hw1//20news-19997//20_newsgroups//comp.windows.x",
               "D://IITC//Study//Spring 2017//CS 522//hw1//20news-19997//20_newsgroups//sci.med")

#loading the folderlist to corpus
newsgroup <-  Corpus(DirSource(folderlist,encoding = "UTF-8"),readerControl = list(reader = readPlain,language = "en"))

#Creating DTM
ogdatadtm <- DocumentTermMatrix(newsgroup)
dim(ogdatadtm)

cleannewsgroup <- newsgroup
cleannewsgroup <- tm_map(cleannewsgroup,removeWords,"NNTP-Posting-Host")
cleannewsgroup <- tm_map(cleannewsgroup,removeWords,"Subject")
cleannewsgroup <- tm_map(cleannewsgroup,removeWords,"article")
cleannewsgroup <- tm_map(cleannewsgroup,removeWords,"From")


cleannewsgroup <- tm_map(cleannewsgroup,removeWords,"writes")
cleannewsgroup <- tm_map(cleannewsgroup,removeWords,"Organization")
cleannewsgroup <- tm_map(cleannewsgroup,removeWords,"Expires")
cleannewsgroup <- tm_map(cleannewsgroup,removeWords,"Keywords")

cleannewsgroup <- tm_map(cleannewsgroup,removePunctuation)
cleannewsgroup <- tm_map(cleannewsgroup,removeNumbers)
cleannewsgroup <- tm_map(cleannewsgroup,content_transformer(tolower))
cleannewsgroup <- tm_map(cleannewsgroup, removeWords, c("nntppostinghost","subject","article","from",
                                                        "writes","organization","expires","keywords",
                                                        "messageid","will","newsgroup"))
cleannewsgroup <- tm_map(cleannewsgroup,removeWords,"nntppostinghost")

cleannewsgroup <- tm_map(cleannewsgroup,removeWords,stopwords("english"))


cleannewsgroup <- tm_map(cleannewsgroup,removeWords,"nntppostinghost")

#after clearing stopwords
datapruned <- DocumentTermMatrix(cleannewsgroup)
dim(datapruned)
#datastpwords <- as.matrix(dataafterstopwords)
#length(colSums(datastpwords))

cleannewsgroup <- tm_map(cleannewsgroup,stemDocument)
cleannewsgroup <- tm_map(cleannewsgroup, stripWhitespace)
datapruned <- DocumentTermMatrix(cleannewsgroup)

dim(datapruned)
#datastemmed <-as.matrix(dataafterstem)
#length(colSums(datastemmed))

#the below code will keep only characterlist
#cleannewsgroup <- tm_map(cleannewsgroup,PlainTextDocument)


#afterpruning length
#dtmafterprune <- DocumentTermMatrix(cleannewsgroup, control = list(wordLengths=c(4,300),weighting = weightTfIdf, stopwords = TRUE))
datapruned <- DocumentTermMatrix(cleannewsgroup, control = list(wordLengths=c(3,20),bounds = list(global = c(10,600)), stopwords = TRUE))
dim(datapruned)
dataprunedmat<-as.matrix(datapruned)


#wordcloud
length(colSums(dataprunedmat))
freq <- colSums(dataprunedmat)
wordcloud(names(freq), freq = freq,
          max.words=100, random.order=FALSE, rot.per=0.2, 
          colors=brewer.pal(8, "Dark2"))


freqdecr<-sort(freq,decreasing = TRUE)
freqdf<-data.frame(names(freqdecr),freqdecr)
head(freqdf,10)

#clustering using wss
set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 8
# Maximal number of clusters 
wss <- sapply(1:k.max, function(k){kmeans(dataprunedmat, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")
abline(v = 10, lty =2)

dtmtfidf <- weightTfIdf(datapruned)
dim(dtmtfidf)

#nbclust
nc<-NbClust(dataprunedmat,distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "silhouette")
nc$Best.nc
#fviz_nbclust(dataprunedmat[1:500][1:500], kmeans, method = "silhouette")
fviz_nbclust(as.matrix(dtmtfidf)[1:500,1:500], kmeans, method = "wss")




nckl<-NbClust(dataprunedmat,distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "kl")
nckl$Best.nc

ncball<-NbClust(dataprunedmat,distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "ball")
 ncball$Best.nc

 ncch<-NbClust(dataprunedmat,distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "ball")
ncch$Best.nc
 
nchub<-NbClust(dataprunedmat,distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "Hubert")
nchub$Best.nc

dtmtfidf <- weightTfIdf(datapruned)
dim(dtmtfidf)

dtrms<-removeSparseTerms(dtmtfidf,0.99)
dtrms
dtrmsmat<-as.matrix(dtrms)

#kmeans
clus<-kmeans(dtmtfidf,2)
plotcluster(dtmtfidf, clus$cluster)

clusplot(dtmtfidf,clus$cluster,color = T,labels=0,lines = 0)






#lda
setwd("D:/IITC/Study/Spring 2017/CS 522/hw1/")
#Number of topics
k <- 4
#Run LDA using Gibbs sampling
ldaOut <-LDA(datapruned,k)
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste('LDA',k,'DocsToTopics4NG.csv'))

ldaOut.terms <- as.matrix(terms(ldaOut,100))
write.csv(ldaOut.terms,file=paste('LDA',k,'TopicsToTerms4NG.csv'))

topicProbabilities <- as.data.frame(ldaOut@gamma)
head(topicProbabilities)


#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtmafterprune),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])

topic2ToTopic3 <- lapply(1:nrow(dtmafterprune),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])


#write to file
write.csv(topic1ToTopic2,file=paste('LDA',k,'4NGTopic1ToTopic2.csv'))
write.csv(topic2ToTopic3,file=paste('LDA',k,'4NGTopic2ToTopic3.csv'))


###normalize the vectors so Euclidean makes sense
#norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
#m_norm <- norm_eucl(m)


nc<-NbClust(datapruned_mat,distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "ccc")
nc$Best.nc



nckl<-NbClust(dataprunedmat,distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "kl")
nckl$Best.nc

ncch<-NbClust(dataprunedmat,distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "ch")
ncch$Best.nc

ncch<-NbClust(dataprunedmat,distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "ch")
ncch$Best.nc

 




 
#-----------------------------------------------------------------------------------
ldaOut <-LDA(datapruned,k,method="Gibbs")
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste('LDA',k,'DocsToTopics4NGGibbs.csv'))

ldaOut.terms <- as.matrix(terms(ldaOut,100))
write.csv(ldaOut.terms,file=paste('LDA',k,'TopicsToTerms4NG.csv'))




#prunedDTM<-DocumentTermMatrix(textCorpusModified, control = list(removePunctuation = FALSE, bounds = list(global = c(4,300))))

dtmtfidf <- weightTfIdf(datapruned)
dim(dtmtfidf)
dftfidf<-as.matrix(dtmtfidf)

write.csv(head(as.matrix(datapruned),10),"dtm.csv")
write.csv(head(dftfidf,10),"tfidf.csv")


dtrms<-removeSparseTerms(dtmtfidf,0.99)
dtrms
dtrmsmat<-as.matrix(dtrms)
#View(dtrmsmat)

#sparse
#dtmprune <- as.matrix(dtmafterprune)
#length(colSums(dtmprune))
length(colSums(dtrmsmat))
#wordcloud for prune data
#freq <- colSums(dtmprune)
freq <- colSums(dtrmsmat)
wordcloud(names(freq), freq = freq,
          max.words=100, random.order=FALSE, rot.per=0.2, 
          colors=brewer.pal(8, "Dark2"))

set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 8
# Maximal number of clusters 
wss <- sapply(1:k.max, function(k){kmeans(dtrmsmat, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")
abline(v = 10, lty =2)

#nbclust
nc<-NbClust(dtrmsmat,distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "silhouette")
nc$Best.nc
fviz_nbclust(dtrmsmat, kmeans, method = "silhouette")


#fviz_nbclust(dtrmsmat,nc)
#kmeans
#clus<-kmeans(dtrmsmat,3)
clus<-kmeans(dtrmsmat,8)
plotcluster(dtrmsmat, clus$cluster)


### k-means (this uses euclidean distance)
m <- as.matrix(datapruned)
myLSAspace = lsa(datapruned, dims=dimcalc_raw())





dim_reduce<-function(dtmprune,dimension){
    svd_dtm<-svd(dtmprune)
  u<-as.matrix(svd_dtm$u[,1:dimension])
  v<-as.matrix(svd_dtm$v[,1:dimension])
  d<-as.matrix(diag(svd_dtm$d)[1:dimension,1:dimension])
  return(as.matrix(u%*%d%*%t(v),type="green"))
  #return(d)
}




reduce_50 <- dim_reduce(dtmtfidf,50)
reduce_100 <- dim_reduce(dtmtfidf,100)
reduce_200 <- dim_reduce(dtmtfidf,200)

rdtm<-reduce_100

m = as.matrix(rdtm)


rownames(m) = 1:nrow(m)
norm_eucl = function(m) {
  m/apply (m, MARGIN = 1, FUN = function(x){
    sum(x^2)^.5
  })
}
rdtm<-reduce_200

m_norm = norm_eucl(m)
clust_200 = kmeans(m_norm, 2)
plotcluster(m_norm,clust_200$cluster)



svddtrms<-svd(dtmtfidf)
head(svddtrms$u)






#LDA
#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

setwd("D:/IITC/Study/Spring 2017/CS 522/hw1/")
#Number of topics
k <- 3
#Run LDA using Gibbs sampling
ldaOut <-LDA(dtmafterprune,k)
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste('LDA',k,'DocsToTopics20NG.csv'))

ldaOut.terms <- as.matrix(terms(ldaOut,100))
write.csv(ldaOut.terms,file=paste('LDA',k,'TopicsToTerms20NG.csv'))

topicProbabilities <- as.data.frame(ldaOut@gamma)
head(topicProbabilities)

clusldang<-kmeans(topicProbabilities,2)




#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtmafterprune),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])

topic2ToTopic3 <- lapply(1:nrow(dtmafterprune),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])


#write to file
write.csv(topic1ToTopic2,file=paste('LDA',k,'20NGTopic1ToTopic2.csv'))
write.csv(topic2ToTopic3,file=paste('LDA',k,'20NGTopic2ToTopic3.csv'))



cl_lda<-CrossTable(temp$V1,temp$lda, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
temp_lda<-as.data.frame(cl_lda$t)
group_lda <- group_by(temp_lda, x)
summry_lda<-summarize(group_lda, right = max(Freq),wroug=sum(Freq)-max(Freq))
accuracy<-sum(summry_lda$right)*100/(sum(summry_lda$wroug)+sum(summry_lda$right))


###normalize the vectors so Euclidean makes sense
#norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
#m_norm <- norm_eucl(m)

#clus<-kmeans(m_norm,3)
#plot(prcomp(m_norm)$x, col=clus$cluster)

#dis <- dist(dtmprune, method = "euclidean",upper=TRUE)
#dis <- dist(dtrmsmat, method = "euclidean",upper=TRUE)
#NbClust(dtmprune, diss = dis, distance = "NULL", min.nc = 2, max.nc = 10, method = "complete", index = "alllong")
#NbClust(dtrmsmat, diss = dis, distance = "NULL", min.nc = 2, max.nc = 10, method = "complete", index = "alllong")
