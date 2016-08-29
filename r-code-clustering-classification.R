##
data("USairpollution", package = "HSAUR2")

pairs(USairpollution)


##
data("pottery", package = "HSAUR2")

pairs(pottery)
plot(pottery[,c(1,2)])

## install.packages("scatterplot3d")
## library("scatterplot3d")
scatterplot3d(pottery[,c(1,2,10)])
scatterplot3d(pottery[,c(1,2,3,10)])


measure <- structure(list(V1 = 1:20,
                          V2 = c(34L, 37L, 38L, 36L, 38L, 43L, 40L, 38L, 40L, 41L,
                                 36L, 36L, 34L, 33L, 36L, 37L, 34L, 36L, 38L, 35L),
                          V3 = c(30L, 32L, 30L, 33L, 29L, 32L, 33L, 30L, 30L, 32L,
                                 24L, 25L, 24L, 22L, 26L, 26L, 25L, 26L, 28L, 23L),
                          V4 = c(32L, 37L, 36L, 39L, 33L, 38L, 42L, 40L, 37L, 39L,
                                 35L, 37L, 37L, 34L, 38L, 37L, 38L, 37L, 40L, 35L)),
                     .Names = c("V1", "V2", "V3","V4"), class = "data.frame",
                     row.names = c(NA, -20L))
measure <- measure[,-1]
names(measure) <- c("chest", "waist", "hips")
measure$gender <- gl(2, 10)
levels(measure$gender) <- c("male", "female")

## covariance
cov(measure[, c("chest", "waist", "hips")])
cov(subset(measure, gender == "female")[,
                                        c("chest", "waist", "hips")])
cov(subset(measure, gender == "male")[,
                                      c("chest", "waist", "hips")])

## correlation
cor(measure[, c("chest", "waist", "hips")])

## Euclidean distance

x <- dist(scale(measure[, c("chest", "waist", "hips")],
                center = FALSE))
as.dist(round(as.matrix(x), 2)[1:12, 1:12])


##
##
## k means clustering (choice of k - simulated two clusters first 25 observations
##        mean=c(3,-4), observation 26 to 50 mean=c(0,0))

set.seed(02022015)
x <- matrix(rnorm(50*2), ncol=2)
x[1:25,1] <- x[1:25,1]+3
x[1:25,2] <- x[1:25,2]-4

plot(x[1:25,],xlim=c(-3,7),ylim=c(-7,3),col=1,cex=1.5,xlab="x",ylab="y",cex.lab=1.5,cex.axis=1.5)
points(x[26:50,],xlim=c(-3,7),ylim=c(-7,3),col=1,cex=1.5)

my.km <- kmeans(x,centers=2,nstart=20)
my.km$cluster

points(x[my.km$cluster==1,],xlim=c(-3,7),ylim=c(-7,3),col=3,pch="x",cex=1)
points(x[my.km$cluster==2,],xlim=c(-3,7),ylim=c(-7,3),col=4,pch="+",cex=1)


plot(x[1:25,],xlim=c(-3,7),ylim=c(-7,3),col=1,cex=1.5,xlab="x",ylab="y",cex.lab=1.5,cex.axis=1.5)
points(x[26:50,],xlim=c(-3,7),ylim=c(-7,3),col=1,cex=1.5)
my.km <- kmeans(x,centers=3,nstart=20)
my.km$cluster
points(x[my.km$cluster==1,],xlim=c(-3,7),ylim=c(-7,3),col=3,pch="x",cex=1)
points(x[my.km$cluster==2,],xlim=c(-3,7),ylim=c(-7,3),col=4,pch="+",cex=1)
points(x[my.km$cluster==3,],xlim=c(-3,7),ylim=c(-7,3),col=5,pch="s",cex=1)


plot(x[1:25,],xlim=c(-3,7),ylim=c(-7,3),col=1,cex=1.5,xlab="x",ylab="y",cex.lab=1.5,cex.axis=1.5)
points(x[26:50,],xlim=c(-3,7),ylim=c(-7,3),col=1,cex=1.5)
my.km <- kmeans(x,centers=4,nstart=20)
my.km$cluster
points(x[my.km$cluster==1,],xlim=c(-3,7),ylim=c(-7,3),col=3,pch="x",cex=1)
points(x[my.km$cluster==2,],xlim=c(-3,7),ylim=c(-7,3),col=4,pch="+",cex=1)
points(x[my.km$cluster==3,],xlim=c(-3,7),ylim=c(-7,3),col=5,pch="s",cex=1)
points(x[my.km$cluster==4,],xlim=c(-3,7),ylim=c(-7,3),col=6,pch="*",cex=1)


plot(x[1:25,],xlim=c(-3,7),ylim=c(-7,3),col=1,cex=1.5,xlab="x",ylab="y",cex.lab=1.5,cex.axis=1.5)
points(x[26:50,],xlim=c(-3,7),ylim=c(-7,3),col=1,cex=1.5)
my.km <- kmeans(x,centers=5,nstart=200)
my.km$cluster
points(x[my.km$cluster==1,],xlim=c(-3,7),ylim=c(-7,3),col=3,pch="x",cex=1)
points(x[my.km$cluster==2,],xlim=c(-3,7),ylim=c(-7,3),col=4,pch="+",cex=1)
points(x[my.km$cluster==3,],xlim=c(-3,7),ylim=c(-7,3),col=5,pch="s",cex=1)
points(x[my.km$cluster==4,],xlim=c(-3,7),ylim=c(-7,3),col=6,pch="*",cex=1)
points(x[my.km$cluster==5,],xlim=c(-3,7),ylim=c(-7,3),col=2,pch="=",cex=1)

##
##
## hierarchical clustering example

s.pottery <- scale(pottery[,c(1:9)])
d.pottery <- dist(s.pottery)

plot(hclust(d.pottery, method="complete"),labels=(as.character(pottery$kiln)),
     cex=.75, main="",xlab="complete-linkage",ylab="level",sub="")



par(mfrow=c(1,1))
plot(hclust(d.pottery, method="complete"),labels=(as.character(pottery$kiln)),
     hang=-1,cex=.75, main="",xlab="complete-linkage",ylab="level",sub="")

par(mfrow=c(2,2))
plot(hclust(d.pottery, method="complete"),labels=(as.character(pottery$kiln)),
     hang=-1,cex=.5, main="",xlab="complete-linkage",ylab="level",sub="")
plot(hclust(d.pottery, method="single"),labels=(as.character(pottery$kiln)),
     hang=-1,cex=.5, main="",xlab="single-linkage",ylab="level",sub="")
plot(hclust(d.pottery, method="average"),labels=(as.character(pottery$kiln)),
     hang=-1,cex=.5, main="",xlab="average-linkage",ylab="level",sub="")
plot(hclust(d.pottery, method="centroid"),labels=(as.character(pottery$kiln)),
     hang=-1,cex=.5, main="",xlab="centroid-linkage",ylab="level",sub="")


par(mfrow=c(1,1))
d.pottery <- as.dist(1- cor(pottery[,c(1:9)])^2)
plot(hclust(d.pottery, method="average"),
     hang=-1,cex=.75, main="",xlab="average-linkage",ylab="level",sub="")


library(clue)
plot(ls_fit_addtree(d.pottery))


##
## ##
## ##
## ##
## ##
##  Classification

data("Default", package = "ISLR")
str(Default)

##
## Plot of random 5% sample of data (runif(10000)>.95)

plot(income ~ balance , data=Default[(Default$default=="No") & (runif(10000)>.95),],pch="o",col=4,
     xlim=c(0,3000), ylim=c(0,70000), xlab="balance",ylab="income",main="(a)",cex.lab=1.5)
points(income ~ balance , data=Default[Default$default=="Yes",],pch="+",col=2)



par(mfrow=c(1,2))
boxplot(balance ~ default, data=Default,ylab="balance",main="(b)",xlab="default",
        col=c(4,2),cex.lab=1.5)
boxplot(income ~ default, data=Default,ylab="income",xlab="default",
        col=c(4,2),cex.lab=1.5)


par(mfrow=c(1,1))

mylinearclassifier <- lm((as.numeric(default) - 1)~ balance, data=Default)
summary(mylinearclassifier)

plot((as.numeric(default) -1) ~ balance, data=Default,cex=.5,col=4,
     ylab="probability of default",main="(a)",cex.lab=1.5,xlim=c(0,3000))
abline(mylinearclassifier,lwd=4,col=1)


mylogisticclassifier <- glm(default ~ balance,data=Default,family=binomial )
summary(mylogisticclassifier)

plot((as.numeric(default) -1) ~ balance, data=Default,cex=.5,col=4,
     ylab="probability of default",main="(b)",cex.lab=1.5,xlim=c(0,3000))
curve(predict(mylogisticclassifier,data.frame(balance=x),type="resp"),
      lwd=4,add=TRUE,col=1)





hist(Default$balance[Default$default=="No"],breaks=c(200*(0:15)),col=4,density=10,
     xlim=c(0,3000),ylim=c(0,1500),angle = -45,
     freq=TRUE,xlab="balance",ylab="frequency",cex.lab=1.5,main="(a)")
par(new=TRUE)
hist(Default$balance[Default$default=="Yes"],breaks=c(200*(0:15)), col=2,density=10,
     xlim=c(0,3000), ylim=c(0,1500),angle = 45,
     freq=TRUE,axes=FALSE,ann=FALSE)
text(250,1500,"no default",col=4,cex=1.5)
text(2500,200,"default",col=2,cex=1.5)


hist(Default$balance[Default$default=="No"],breaks=c(200*(0:15)),col=4,density=10,
     xlim=c(0,3000),ylim=c(0,0.0014),angle = -45,
     freq=FALSE,axes=FALSE,ann=FALSE)
par(new=TRUE)
hist(Default$balance[Default$default=="Yes"],breaks=c(200*(0:15)), col=2,density=10,
     xlim=c(0,3000),ylim=c(0,0.0014), angle = 45,
     freq=FALSE,axes=FALSE,ann=FALSE)
par(new=TRUE)
plot(density(Default$balance[Default$default=="Yes"]),
     xlim=c(0,3000),ylim=c(0,0.0014),
     col=2,lwd=4,xlab="balance",ylab="density",cex.lab=1.5,main="(b)")
lines(density(Default$balance[Default$default=="No"]),
      xlim=c(0,3000),ylim=c(0,0.0014),
      col=4,lwd=4)
text(250,.001,"no default",col=4,cex=1.5)
text(2500,.001,"default",col=2,cex=1.5)

##
## LDA

library(MASS)
lda.Default <- lda(default ~ balance + income , data=Default)
lda.Default

default.predicted <- predict(lda.Default)$class
table(default.predicted,Default$default)


table(Default$default)
table(default.predicted)

(table(default.predicted,Default$default)[1,2] +
   table(default.predicted,Default$default)[2,1]) / (dim(Default)[1])

qda.Default <- qda(default ~ balance + income , data=Default)
qda.Default

default.predicted <- predict(qda.Default)$class
table(default.predicted,Default$default)


table(Default$default)
table(default.predicted)

(table(default.predicted,Default$default)[1,2] +
   table(default.predicted,Default$default)[2,1]) / (dim(Default)[1])


##
## LDA

library(MASS)
library(ipred)

mypredict.lda <- function(object, newdata)
  predict(object, newdata = newdata)$class
mypredict.knn <- function(object, newdata)
  predict(object, newdata = newdata)$class

set.seed(25012015)


errorest(default ~ ., data=Default, model=lda,
         estimator = c("cv"),
         est.para=control.errorest(k=10), predict= mypredict.lda)

errorest(default ~ ., data=Default, model=lda,
         estimator = c("boot"),
         est.para=control.errorest(nboot=200,strat=TRUE), predict= mypredict.lda)

##
## CART - tree based classifier

library(tree)


data(GlaucomaMVF)
str(GlaucomaMVF)
tree.glaucoma <- tree(Class ~ ., data=GlaucomaMVF[,c(1:62,67)])
tree.glaucoma
summary(tree.glaucoma)
plot(tree.glaucoma)
text(tree.glaucoma)

prune.glaucoma <- prune.misclass(tree.glaucoma, best=5)
plot(prune.glaucoma)
text(prune.glaucoma)

prune.glaucoma


##
## randomforest

library(MASS)
library(ipred)
library(tree)
library(randomForest)

forest.glaucoma <- randomForest(Class ~ ., data=GlaucomaMVF[,c(1:62,67)])
forest.glaucoma

mypredict.randomForest <- function(object, newdata)
  predict(object, newdata = newdata, type = c("response"))

errorest(Class ~ ., data=GlaucomaMVF[,c(1:62,67)],model=randomForest,
         estimator = "cv", predict= mypredict.randomForest)
errorest(Class ~ ., data=GlaucomaMVF[,c(1:62,67)],model=randomForest,
         estimator = "boot", predict= mypredict.randomForest)


## end of R code for Clustering and Classification with applications in R
## August 2015
