library(ggplot2) 
library(purrr)

df <- read.csv("D:\\R_programming\\Mall_Customers.csv") 
head(df)

summary(df)

sum(is.na(df)) 
sum(duplicated(df))

gender <- table(df$Gender) 
print(gender) 
barplot(gender,main='Bar plot of Gender',xlab='Gender',ylab='Count', 
        col=rainbow(2),legend=rownames(gender))

percent <- gender/sum(gender) * 100 
print(percent) 
labels <- paste(c('Female','Male'),percent,'%') 
print(labels) 
pie(percent,col=rainbow(2),labels=labels)

hist(df$Age,breaks=5,col='blue',labels=T)

hist(df$Annual.Income..k..,col='red',labels=T,main='Distribution of Annual Income')

hist(df$Spending.Score..1.100.,col='orange',labels=T,main='Distribution of Spending Amount')

plot(df$Age,df$Annual.Income..k..,col='black')

fun <- function(k){ 
    kmeans(df[,3:5],k,iter.max=100,nstart = 100,algorithm='Lloyd')$tot.withinss 
    } 

k.values <- 1:10 

fun_value <- map_dbl(k.values,fun) 

plot(k.values,fun_value,type='b',xlab='number of clusters',ylab='total sum of squares')

k5<- kmeans(df[,3:5],5,iter.max = 100,nstart = 50,algorithm = 'Lloyd') 
  
print(k5)

ggplot(df, 
       aes(x = Annual.Income..k..,y = Spending.Score..1.100.)) + 
    geom_point(stat = 'identity',aes(col = as.factor(k5$cluster))) + 
    scale_color_discrete(breaks = c('1','2','3','4','5'), 
                         labels = c('C1','C2','C3','C4','C5')) + 
    ggtitle('Customer Segmentation using Kmeans')