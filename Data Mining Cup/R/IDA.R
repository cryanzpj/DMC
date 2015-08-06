setwd("G:/Google Drive/Data Mining Cup")
build_set <- read.csv("SEM_DAILY_BUILD.csv", header=T, stringsAsFactors=F)
valid_set <- read.csv("SEM_DAILY_VALIDATION.csv", header=T, stringsAsFactors=F)
keyword <- read.csv("SEM_KEYWORD_SUMMARY_S.csv", header=T, stringsAsFactors=F)
build_set[is.na(build_set)] <- 0
valid_set[is.na(valid_set)] <- 0
keyword[is.na(keyword)] <- 0

summary(build_set)
summary(valid_set)

posrev <- union(which(build_set$PROD_1_REVENUE>0),
                union(which(build_set$PROD_2_REVENUE>0),
                      union(which(build_set$PROD_3_REVENUE>0),
                            union(which(build_set$PROD_4_REVENUE>0),
                                  union(which(build_set$PROD_5_REVENUE>0),
                                        which(build_set$PROD_6_REVENUE>0))))))

posapp <- union(which(build_set$PROD_1_APPROVED>0),
                union(which(build_set$PROD_2_APPROVED>0),
                      union(which(build_set$PROD_3_APPROVED>0),
                            union(which(build_set$PROD_4_APPROVED>0),
                                  union(which(build_set$PROD_5_APPROVED>0),
                                        which(build_set$PROD_6_APPROVED>0))))))

build_set$PROD_APPROVED_TOTAL <- numeric(NROW(build_set))
build_set$PROD_APPROVED_TOTAL[posapp] <- 1

imp <- glm(PROD_APPROVED_TOTAL~IMPRESSIONS, family=binomial, data=build_set)
keywords <- character(0)
for(i in 1:NROW(keyword))
{
  keywords <- unique(c(keywords, strsplit(keyword$KEYWD_TXT[i], split="+", fixed=T)))
}

keywords <- unique(unlist(keywords))

kwds <- strsplit(keyword$KEYWD_TXT, split="+", fixed=T)
avg <- 0
for(i in 1:length(kwds)){
  avg <- avg+length(kwds[[i]])
}
avg <- avg/length(kwds)-1

TOTAL_REVENUE <- rowSums(build_set[,44:49])
cases <- which(TOTAL_REVENUE>0)

qs <- lm(TOTAL_REVENUE[cases]~build_set$TOTAL_QUALITY_SCORE[cases]+build_set$IMPRESSION_TOTAL_RANK[cases]+build_set$IMPRESSIONS[cases])
summary(qs)
plot(y=TOTAL_REVENUE[cases], x=build_set$TOTAL_QUALITY_SCORE[cases])
abline(a=coef(qs)[1], b=coef(qs)[2])
boxcox(qs, lambda=seq(-5,0,1/10))
gm <- exp(sum(log(TOTAL_REVENUE[cases]))/length(cases))



#--------------------clust-----------------------------
clustset <- read.csv("Clust.csv", header=T, stringsAsFactors=F)
kwds <- unique(union(unique(clustset[,6]), 
                     union(unique(clustset[,7]),
                           union(unique(clustset[,8]),
                                 union(unique(clustset[,9]),
                                       union(unique(clustset[,10]), 
                                             unique(clustset[,11])))))))
kwds <- sort(kwds)[2:493]

campaign <- unique(clustset[,4])

kxc <- matrix(0, nrow=492, ncol=38)
rownames(kxc) <- kwds
colnames(kxc) <- campaign
for(i in 1:NROW(clustset))
{
  for(j in 6:11){
    if(clustset[i, j]=="") next
    else
    {
      kwd <- clustset[i, j]
      r <- which(kwds==kwd)
      c <- which(campaign==clustset[i,4])
      if(kxc[r, c]>0) next
      else kxc[r, c] <- 1
    }
  }
}

write.csv(kxc, "keywordvscampain.csv")

dkxc <- dist(data.frame(kxc))
hckxc <- hclust(dkxc, method="ward.D2")
plot(hckxc)
kxclusts <- cutree(hckxc, h=11)
kxcc <- data.frame(cbind(kxc), kxclusts)

write.csv(kxcc, "kxclust.csv")

dcxk <- dist(data.frame(t(kxc)))
hcxk <- hclust(dcxk, method="ward.D2")
plot(hcxk)
kxclusts <- cutree(hckxc, h=11)
kxcc <- data.frame(cbind(kxc), kxclusts)

write.csv(kxcc, "kxclust.csv")