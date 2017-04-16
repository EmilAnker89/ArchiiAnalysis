#' condense
#' @description find n top PCs and k highest loading features for those PCs
#'
#' @param pca \code{prcomp} output from stats::prcomp (PCA)
#' @param n \code{integer} number of PCs to extract
#' @param k \code{integer} number of features per PC
#'
#' @return list
#' @export
#'
condense <- function(pca, n, k) {
  out <- list()
  for (i in seq_along(1:n)) {
    pc <- pca$rotation[,i]
    pc_abs <- sort(abs(pc), decreasing=T)[1:k]
    out[[paste0("pc",i)]] <- pc[match(names(pc_abs), names(pc))]
  }
  out
}
