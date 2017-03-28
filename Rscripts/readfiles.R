#indhent og gem træningsdata som .rds:
library(cluster)
library(HSAUR)
library(fpc)
library(magrittr)


files <- dir("out/data_import")
files <- files[!grepl(".py",files)]

file <- files[[1]]
file2 <- files[[2]]
file3 <- files[[3]]
train <- read.csv2(paste0("out/data_import/",file), header=F, stringsAsFactors = F)
label <- read.table(paste0("out/data_import/",file2), header=F, stringsAsFactors = F)
features <- read.table(paste0("out/data_import/",file3), header=F, stringsAsFactors = F)[,1]

#contract <- cbind(train,label)
train <- lapply(train, as.numeric) %>% as.data.frame
names(train) <- features

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

library(dplyr)
#now make your lovely plot
temp <- data.frame(component = train.pca$x[,1], type=label$V1)
ggplot(temp, aes(component, fill = type)) +
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', bins=100) +
  scale_x_continuous(limits = c(-.5, 1))
#  scale_y_continuous(limits = c(0, 3))


library(ggplot2)

pca_res <- prcomp(train, center = TRUE, scale. = TRUE)
plot_data <- cbind(as.data.frame(pca_res$x[, 1:2]), labels = label$color)

ggplot(plot_data, aes(x = PC1, y = PC2, colour = labels)) +
  geom_point()


