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
train.pca <- prcomp(train,
                 center = TRUE,
                 scale. = TRUE)
print(ir.pca)
plot(ir.pca, type = "l")
summary(ir.pca)

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1,
              groups = ir.species, ellipse = TRUE,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)


require(caret)
trans = preProcess(iris[,1:4],
                   method=c("BoxCox", "center",
                            "scale", "pca"))
PC = predict(trans, iris[,1:4])

