setwd("G:/Google Drive/Data Mining Cup")
build_set <- read.csv("SEM_DAILY_BUILD.csv", header=T, stringsAsFactors=F)
valid_set <- read.csv("SEM_DAILY_VALIDATION.csv", header=T, stringsAsFactors=F)
# kwd_grouping_build <- read.csv("KEYWORD_GROUPING_BUILD.csv", header=F, stringsAsFactors=F) # Need Olga's clustering - keyword combination to cluster number; build
# kwd_grouping_valid <- read.csv("KEYWORD_GROUPING_VALID.csv", header=F, stringsAsFactors=F) #validation
kwd_product_build <- read.csv("KEYWORD_TO_PRODUCT_BUILD.csv", header=F, stringsAsFactors=F) # Need Mengye's keyword prediction to product number; build
kwd_product_valid <- read.csv("KEYWORD_TO_PRODUCT_VALID.csv", header=F, stringsAsFactors=F) #validation
build_set[is.na(build_set)] <- 0
valid_set[is.na(valid_set)] <- 0


kwd_product_build <- cbind(kwd_product_build, rep(0, NROW(kwd_product_build)))
kwd_product_valid <- cbind(kwd_product_valid, rep(0, NROW(kwd_product_valid)))

#as.factor(KWD_GROUP) + 
#--------ERASE!!!
kwd_grouping_build <- rep(0, nrow=NROW(build_set), ncol=1)
kwd_grouping_valid <- rep(0, nrow=NROW(valid_set), ncol=1)
names(kwd_grouping_build) <- names(kwd_grouping_valid) <- "KWD_GROUP"
#---------------

build_set <- cbind(build_set, kwd_grouping_build, kwd_product_build)
valid_set <- cbind(valid_set, kwd_grouping_valid, kwd_product_valid)

names(build_set)[50] <- names(valid_set)[25] <- "KWD_GROUP"
names(build_set)[51:56] <- names(valid_set)[26:31] <- c("PRED_PROD_1", "PRED_PROD_2",
                                                        "PRED_PROD_3", "PRED_PROD_4",
                                                        "PRED_PROD_5", "PRED_PROD_6")


predapp <- lm(APPLICATIONS ~ VISITS + as.factor(ENGN_ID) + as.factor(LANG_ID) + as.factor(DVIC_ID), 
                data=build_set)
summary(predapp)

build_visited <- build_set[which(build_set$VISITS>0),]


#apprate2 <- predict(predapp, newdata=build_visited)

build_applied <- build_visited[which(build_visited$APPLICATIONS>0),]


predapp <- lm(APPLICATIONS ~ VISITS + as.factor(ENGN_ID) + as.factor(LANG_ID) + 
                as.factor(DVIC_ID), 
              data=build_visited)
summary(predapp)

#test presicion
nvisited <- NROW(build_visited)
buy_prob <- numeric(100)
nobuy_prob <- numeric(100)
for(i in 1:100){
  A <- sample(1:nvisited, size=floor(nvisited/2))
  B <- (1:nvisited)[-A]
  train <- build_visited[A,]
  test <- build_visited[B,]
  predmodel <- lm(APPLICATIONS ~ VISITS + as.factor(ENGN_ID) + as.factor(LANG_ID) + 
                    as.factor(DVIC_ID), 
                  data=train)
  predapprate <- predict(predmodel, newdata=test)
  errorratebuy <- sum(abs((round(predapprate,0)>0)-(test[,28]>0)))/length((round(predapprate,0)>0))
  buy_prob[i] <- 1-errorratebuy
  errorratenobuy <- sum(abs((round(predapprate,0)==0)-(test[,28]==0)))/length((round(predapprate,0)==0))
  nobuy_prob[i] <- 1-errorratenobuy
}
mean(buy_prob)
mean(nobuy_prob)



#make predations-----------------------------------------------

prediction_model <- lm(APPLICATIONS ~ VISITS + as.factor(ENGN_ID) + as.factor(LANG_ID) + 
                         as.factor(DVIC_ID), 
                       data=build_visited)
#predict application rate
predapprate <- predict(prediction_model, newdata=valid_set)
predapprate[union(which(valid_set$VISITS<=0), which(predapprate<=0))] <- 0
#application distribution - probably of applying to product k given application > 0
applypred <- matrix(0, nrow=NROW(valid_set), ncol=6)
for(i in 1:5){
  applypred[,i] <- predapprate*valid_set[,25+i]/rowSums(valid_set[,26:31])
}#normalized


colnames(applypred) <- names(build_set)[32:37]

#approve rate for each product
approverate <- numeric(5)
for(i in 1:5){
  approverate[i] <- sum(build_set[,37+i])/sum(build_set[,28])
}
approverate <- c(approverate,0); approverate

approvepred <- cbind(predapprate)%*%rbind(approverate)
colnames(approvepred) <- names(build_set)[38:43]

#product specific average revenue
prodrevenue <- c(colSums(build_visited[,44:48])/colSums(build_visited[,38:42]),0)

#exptcted product specific revenue given applied
revenuepred <- matrix(0, nrow=NROW(valid_set), ncol=6)
for(i in 1:5){
  revenuepred[,i] <- approvepred[,i]*prodrevenue[i]
}

colnames(revenuepred) <- names(build_set)[44:49]
valid_set <- cbind(valid_set, applypred, approvepred, revenuepred)
valid_set = cbind(valid_set,rep(0,NROW(valid_set)))
valid_set[50] = rowSums(approvepred*(revenuepred))
names(valid_set)[50] = "MAX_BID"



write.csv(valid_set, "SOLVED_VALID.csv")
