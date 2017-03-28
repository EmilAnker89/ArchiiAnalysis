#indhent og gem træningsdata som .rds:
library(cluster)
library(HSAUR)
library(fpc)
library(magrittr)


files <- dir("out/data_import")
files <- files[!grepl(".py",files)]

file <- files[[1]]
file2 <- files[[2]]
train <- read.csv2(paste0("out/data_import/",file), header=F, stringsAsFactors = F)
label <- read.table(paste0("out/data_import/",file2), header=F, stringsAsFactors = F)
contract <- cbind(train,label)
#rm(train, label, file, file2)
train <- lapply(train, as.numeric) %>% as.data.frame

# Kmeans clustre analysis
clus <- kmeans(train, centers=2)

# Fig 01
plotcluster(train, clus$cluster)
clusplot(train, clus$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)


#with(contract, pairs(train, col=c(1:5)[clus$cluster]))


# apply PCA - scale. = TRUE is highly
# advisable, but default is FALSE.
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility),                                   combi$Item_Visibility)

# train2 <- lapply(train, function(x) {
#   m <- median(x,na.rm=T)
#   ifelse(x==0,m,x)
# }) %>% as.data.frame


train.pca <- prcomp(train,
                 center = TRUE,
                 scale. = TRUE, retx=T)
print(train.pca)
plot(train.pca, type = "l", xlim=c(1,10))
summary(train.pca)
train.pca$rotation[1:5,1:10]
sum(train.pca$sdev>5)
label$color <- ifelse(label=="Contract", "red", "blue")
#plot c1, c2 + labels
xlim = c(quantile(train.pca$x[,1],.005),quantile(train.pca$x[,1],.995))
ylim = c(quantile(train.pca$x[,2],.005),quantile(train.pca$x[,2],.995))
plot(train.pca$x[,1],train.pca$x[,2], xlim=xlim,ylim=ylim, col=label$color)
# plot c3, c4 + labels
xlim = c(quantile(train.pca$x[,3],.005),quantile(train.pca$x[,3],.995))
ylim = c(quantile(train.pca$x[,4],.005),quantile(train.pca$x[,4],.995))
plot(train.pca$x[,3],train.pca$x[,4], xlim=xlim,ylim=ylim, col=label$color)

test <- train.pca$x[,1:5]
results <- apply(train.pca$x[,1:5], 2, function(x) {
  con <- median(x[label$color=="red"], na.rm=T)
  noncon <- median(x[label$color=="blue"], na.rm=T)
  c(con=con, noncon=noncon)
})


library(ggplot2)

pca_res <- prcomp(train, center = TRUE, scale. = TRUE)
plot_data <- cbind(as.data.frame(pca_res$x[, 1:2]), labels = label$color)

ggplot(plot_data, aes(x = PC1, y = PC2, colour = labels)) +
  geom_point()


