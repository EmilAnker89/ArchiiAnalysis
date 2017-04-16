plot_pcs <- function(labels, pca, md_pc, cut_lim=0.01, ablines=T, dims=c(1,2)) {
  lvls <- unique(labels)
  colors <- grDevices::rainbow(length(lvls))
  col_vector <- colors[match(labels, lvls)]
  pcs <- md_pc$pcs

  lims <- list()
  for (i in pcs) {
    lims[[i]] <- c(quantile(pca$x[,pcs[[i]]], cut_lim),
                   quantile(pca$x[,pcs[[i]]], 1-cut_lim))
  }

  x <- pcs[[dims[[1]]]]
  y <- pcs[[dims[[2]]]]

  plot(pca$x[,x], pca$x[,y], xlim=lims[[x]], y=lims[[y]], col = col_vector)
  # if (ablines) {
  #   abline(v = md_pc$medians[md_pc[[1]]])
  # }

#  plot(pca$x[match(label,lvls[[]])])



  plot(train.pca$x[,pcs[[1]]],train.pca$x[,pcs[[2]]], xlim=xlim,ylim=ylim, col=col_vector)
  abline(v=medians[pcs[[1]]][[1]][[1]], col=colors[[1]])
  abline(v=medians[pcs[[1]]][[1]][[2]], col=colors[[2]])
  abline(h=medians[pcs[[2]]][[1]][[1]], col=colors[[1]])
  abline(h=medians[pcs[[2]]][[1]][[2]], col=colors[[2]])



}


