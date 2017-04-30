
#dir("out/data_import") #show files in a directory
train <- "out/data_import/contract_binarytrain_data.csv"
labels <- "out/data_import/contract_binarytrain_labels.txt"
features <- "out/data_import/contract_binarytrain_vocabulary.txt"

df <- read_data(train, labels, features)
non_labels <- setdiff(names(df),"contract_label")
train <- df[,non_labels]

#kunne være en t-SNE i stedet for PCA (måske bedre til plots):
train.pca <- stats::prcomp(train,
                 center = TRUE,
                 scale. = FALSE, retx=T)

# train.tsne <- Rtsne::Rtsne(train,
#                            dims = 10)


#print(train.pca)
plot(train.pca, type = "l", npcs=10)
totvar <- sum(train.pca$sdev^2)
var <- train.pca$sdev^2
pct_var <- var/totvar
#train.pca$rotation[1:5,1:10]

out <- condense(train.pca, 10, 20)


lvls <- unique(df$contract_label)
colors <- grDevices::rainbow(length(lvls))
col_vector <- colors[match(df$contract_label, lvls)]
#plot c1, c2 + labels
#find the two PCs with the highest median difference across:
temp <- most_different_pc(train.pca, df$contract_label, 15)
medians <- temp$medians
pcs <- temp$pcs

xlim <- c(quantile(train.pca$x[,pcs[[1]]],.01),quantile(train.pca$x[,pcs[[1]]],.99))
ylim <- c(quantile(train.pca$x[,pcs[[2]]],.01),quantile(train.pca$x[,pcs[[2]]],.99))
zlim <- c(quantile(train.pca$x[,pcs[[3]]],.01),quantile(train.pca$x[,pcs[[3]]],.99))

plot(train.pca$x[match(df$contract_label,lvls[[1]]) %in% 1,pcs[[1]]],
     train.pca$x[match(df$contract_label,lvls[[1]]) %in% 1,pcs[[2]]],xlim=xlim,ylim=ylim, col=colors[[1]])
abline(v=medians[pcs[[1]]][[1]][[1]], col=colors[[1]])
abline(h=medians[pcs[[2]]][[1]][[1]], col=colors[[1]])

points(train.pca$x[match(df$contract_label,lvls[[2]]) %in% 1,pcs[[1]]],
     train.pca$x[match(df$contract_label,lvls[[2]]) %in% 1,pcs[[2]]], col=colors[[2]])
abline(v=medians[pcs[[1]]][[1]][[2]], col=colors[[2]])
abline(h=medians[pcs[[2]]][[1]][[2]], col=colors[[2]])


plot(train.pca$x[,pcs[[1]]],train.pca$x[,pcs[[2]]], xlim=xlim,ylim=ylim, col=col_vector)
abline(v=medians[pcs[[1]]][[1]][[1]], col=colors[[1]])
abline(v=medians[pcs[[1]]][[1]][[2]], col=colors[[2]])
abline(h=medians[pcs[[2]]][[1]][[1]], col=colors[[1]])
abline(h=medians[pcs[[2]]][[1]][[2]], col=colors[[2]])


library(scatterplot3d)
s3d <- scatterplot3d(train.pca$x[match(df$contract_label,lvls[[1]]) %in% 1,pcs[[1]]], xlim=xlim,
                     train.pca$x[match(df$contract_label,lvls[[1]]) %in% 1,pcs[[2]]], ylim=ylim,
                     train.pca$x[match(df$contract_label,lvls[[1]]) %in% 1,pcs[[3]]], zlim=zlim,
                     color=colors[[1]])
s3d$points3d(train.pca$x[match(df$contract_label,lvls[[2]]) %in% 1,pcs[[1]]],
             train.pca$x[match(df$contract_label,lvls[[2]]) %in% 1,pcs[[2]]],
             train.pca$x[match(df$contract_label,lvls[[2]]) %in% 1,pcs[[3]]],
             col=colors[[2]])

library(rgl)

s3d <- plot3d(train.pca$x[match(df$contract_label,lvls[[1]]) %in% 1,pcs[[1]]], xlim=xlim,
       train.pca$x[match(df$contract_label,lvls[[1]]) %in% 1,pcs[[2]]], ylim=ylim,
       train.pca$x[match(df$contract_label,lvls[[1]]) %in% 1,pcs[[3]]], zlim=zlim,
       col=colors[[1]], size=5,
       xlab=paste0("Component 1: ",round(pct_var[1],4)*100," % Variance explained"),
       ylab=paste0("Component 2: ",round(pct_var[2],4)*100," % Variance explained"),
       zlab=paste0("Component 3: ",round(pct_var[3],4)*100," % Variance explained"))
points3d(train.pca$x[match(df$contract_label,lvls[[2]]) %in% 1,pcs[[1]]],
             train.pca$x[match(df$contract_label,lvls[[2]]) %in% 1,pcs[[2]]],
             train.pca$x[match(df$contract_label,lvls[[2]]) %in% 1,pcs[[3]]],
             col=colors[[2]], size=5)
legend3d("topright", legend = paste('Type:', c(lvls[[1]], lvls[[2]])), pch = 16, col = colors, cex=1, inset=c(0.02))
input <- select3d()

xyz <- c(train.pca$x[match(df$contract_label,lvls[[1]]) %in% 1,pcs[[1]]],
train.pca$x[match(df$contract_label,lvls[[1]]) %in% 1,pcs[[2]]],
train.pca$x[match(df$contract_label,lvls[[1]]) %in% 1,pcs[[3]]])








library(Rcmdr)
s3d <- scatter3d(train.pca$x[match(df$contract_label,lvls[[1]]) %in% 1,pcs[[1]]], xlim=xlim,
          train.pca$x[match(df$contract_label,lvls[[1]]) %in% 1,pcs[[2]]], ylim=ylim,
          train.pca$x[match(df$contract_label,lvls[[1]]) %in% 1,pcs[[3]]], zlim=zlim,
          color=colors[[1]])






#######


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


