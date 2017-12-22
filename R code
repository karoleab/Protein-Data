#Hierarchical Clustering 

> prodata<-read.csv("prot.csv",header=T)
> str(prodata)

'data.frame':	25 obs. of  10 variables:
 $ Country  : Factor w/ 25 levels "Albania","Austria",..: 1 2 3 4 5 6 7 8 9 10 ...
 $ RedMeat  : num  10.1 8.9 13.5 7.8 9.7 10.6 8.4 9.5 18 10.2 ...
 $ WhiteMeat: num  1.4 14 9.3 6 11.4 10.8 11.6 4.9 9.9 3 ...
 $ Eggs     : num  0.5 4.3 4.1 1.6 2.8 3.7 3.7 2.7 3.3 2.8 ...
 $ Milk     : num  8.9 19.9 17.5 8.3 12.5 25 11.1 33.7 19.5 17.6 ...
 $ Fish     : num  0.2 2.1 4.5 1.2 2 9.9 5.4 5.8 5.7 5.9 ...
 $ Cereals  : num  42.3 28 26.6 56.7 34.3 21.9 24.6 26.3 28.1 41.7 ...
 $ Starch   : num  0.6 3.6 5.7 1.1 5 4.8 6.5 5.1 4.8 2.2 ...
 $ Nuts     : num  5.5 1.3 2.1 3.7 1.1 0.7 0.8 1 2.4 7.8 ...
 $ Fr.Veg   : num  1.7 4.3 4 4.2 4 2.4 3.6 1.4 6.5 6.5 ...

> countries<-dist(prodata,method="euclidean")
Warning message:
In dist(prodata, method = "euclidean") : NAs introduced by coercion
> countries

> tree1<-hclust(countries,method="ward")
The "ward" method has been renamed to "ward.D"; note new "ward.D2"

> plot(tree1, hang = -1, labels=prodata$Country)

> clusternumber_ward<-cutree(tree1, k=6)
> clusternumber_ward
[1] 1 2 2 3 4 5 6 5 2 1 4 2 1 2 5 4 6 3 6 5 2 2 1 2 3

> c1_ward=subset(prodata,clusternumber_ward==1)
> c2_ward =subset(prodata,clusternumber_ward==2)
> c3_ward =subset(prodata,clusternumber_ward==3)
> c4_ward =subset(prodata,clusternumber_ward==4)
> c5_ward =subset(prodata,clusternumber_ward==5)
> c6_ward =subset(prodata,clusternumber_ward==6)

> c1_ward
> c2_ward
> c3_ward
> c4_ward
> c5_ward
> c6_ward
> tree2<-hclust(countries,method="average")

> plot(tree2, hang = -1, labels=prodata$Country)

> clusternumber_avg<-cutree(tree2, k=6)
> clusternumber_avg
 [1] 1 2 2 3 1 2 4 5 2 1 1 2 1 2 2 1 6 3 6 2 2 2 1 2 3

> c1_avg=subset(prodata,clusternumber_avg==1)
> c2_avg=subset(prodata,clusternumber_avg==2)
> c3_avg=subset(prodata,clusternumber_avg==3)
> c4_avg=subset(prodata,clusternumber_avg==4)
> c5_avg=subset(prodata,clusternumber_avg==5)
> c6_avg=subset(prodata,clusternumber_avg==6)

> c1_avg
> c2_avg
> c3_avg
> c4_avg
> c5_avg
> c6_avg

> tree3<-hclust(countries,method="single")

> plot(tree3, hang = -1, labels=prodata$Country)

> clusternumber_sing<-cutree(tree3, k=6)
> clusternumber_sing
 [1] 1 2 2 3 4 2 2 5 2 4 4 2 4 2 2 4 6 3 6 2 2 2 4 2 3

> c1_sing=subset(prodata,clusternumber_sing==1)
> c2_sing =subset(prodata,clusternumber_sing==2)
> c3_sing =subset(prodata,clusternumber_sing==3)
> c4_sing =subset(prodata,clusternumber_sing==4)
> c5_sing =subset(prodata,clusternumber_sing==5)
> c6_sing =subset(prodata,clusternumber_sing==6)

> c1_sing
> c2_sing
> c3_sing
> c4_sing
> c5_sing
> c6_sing

> tree4<-hclust(countries,method="complete")

> plot(tree4, hang = -1, labels=prodata$Country)

> clusternumber_com<-cutree(tree4, k=6)
> clusternumber_com

[1] 1 2 2 3 4 5 6 5 2 1 4 2 1 2 5 4 6 3 6 5 2 2 1 2 3

> c1_com=subset(prodata,clusternumber_com ==1)
> c2_com =subset(prodata,clusternumber_com ==2)
> c3_com =subset(prodata,clusternumber_com ==3)
> c4_com =subset(prodata,clusternumber_com ==4)
> c5_com =subset(prodata,clusternumber_com ==5)
> c6_com =subset(prodata,clusternumber_com ==6)

> c1_com
> c2_com
> c3_com
> c4_com
> c5_com
> c6_com


#AGNES for Hierarchical clustering 

> method <- c( "average", "single", "complete", "ward") 
> names(method) <- c( "average", "single", "complete", "ward")
> ac <- function(x) {
+ agnes(prodata, method = x) $ac }
> map_dbl(method, ac)

#K-means 

> P1=matrix(c(prodata$RedMeat), nrow=25,ncol=1)
> P2=matrix(c(prodata$WhiteMeat), nrow=25,ncol=1)
> P3=matrix(c(prodata$Eggs), nrow=25,ncol=1)
> P4=matrix(c(prodata$Milk), nrow=25,ncol=1)
> P5=matrix(c(prodata$Fish), nrow=25,ncol=1)
> P6=matrix(c(prodata$Cereals), nrow=25,ncol=1)
> P7=matrix(c(prodata$Starch), nrow=25,ncol=1)
> P8=matrix(c(prodata$Nuts), nrow=25,ncol=1)
> P9=matrix(c(prodata$Fr&Veg), nrow=25,ncol=1)

> P<-cbind(P1,P2,P3,P4,P5,P6,P7,P8,P9)

> colnames(P)< c("RedMeat","WhiteMeat","Eggs","Milk","Fish","Cereals","Starch","Nuts","Fr&Veg")

> rownames(P)<-c("Albania","Austria","Belgium","Bulgaria","Czechoslovakia","Denmark","E Germany","Finland","France","Greece","Hungary","Ireland","Italy","Netherlands","Norway","Poland","Portugal","Romania","Spain","Sweden","Switzerland","UK","USSR","W Germany","Yugoslavia")

> (result<-kmeans(P,6))

>plot(RedMeat ~ WhiteMeat, prodata, col=result$cluster)

> table(result$cluster)
1 2 3 4 5 6 
3 2 4 5 4 7

> result$withinss
[1]  47.0000  38.6200 131.7775 253.7320 180.6300 237.9486

> result$betweenss
[1] 4353.706

> result$totss
[1] 5243.414

> result$tot.withinss
[1] 889.7081

#Elbow Method for K-Meands

> ra<-2:20     #K value range from 2 to 20
> count<-100 #Running the K-Means algorithm for 100 times
> avg.totw.ss <-integer(length(ra)) #Setting up an empty vector to store all of the points

> for(v in ra)  # For loop for each value of the range variable
{
+ v.totw.ss <-integer(tries) #Setting up an empty vector to hold the 100 tials

+ for(i in 1:tries) 
{
+ k_temp<-kmeans(P,centers=v)   #Running k-means function for the P matrix
+ v.totw.ss[i] <-k_temp$tot.withinss  } #Storing the total within ss
+ avg.totw.ss[v-1] <-mean(v.totw.ss)  } #Average of the 100 total within ss

> plot(ra,avg.totw.ss, type="b", main="Total Within SS for Different K Values",ylab="Average Total Within Sum of Squares",xlab="Value of K")

#Average Silhouette Method 

k.max <- 15
sil <- rep(0, k.max)

# Compute the average silhouette width for 
# k = 2 to k = 15
for(i in 2:k.max)
{
  km.res <- kmeans(P, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(P))
  sil[i] <- mean(ss[, 3])
}

# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters K", ylab=”silhouette”)

abline(v = which.max(sil), lty = 2)

