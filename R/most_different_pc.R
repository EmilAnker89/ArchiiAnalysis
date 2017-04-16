#' most_different_pc
#' @description returns the sorted indices of first n PCs with the highest median differences across groups.
#'
#' @param pca \code{prcomp} output from stats::prcomp
#' @param n \code{integer} number of PCs to extract.
#'
#' @return \code{list}
#' @export
#'

most_different_pc <- function(pca, label_vec, n) {
  if (length(n)>1 || n<1 | n%%1!=0) stop("n must be a positive integer")
  lvls <- unique(label_vec)
  medians <- apply(pca$x[,1:n], 2, function(x) {
    m <- lapply(lvls, function(y) {
      subset <- match(label_vec,y)==1
      median(x[subset], na.rm=T)
    })
    names(m) <- lvls
    m[["max-diff"]] <-  max(unlist(m))-min(unlist(m))
    m
  })
  pcs <- lapply(medians, "[[", "max-diff") %>% unlist %>% sort(.,decreasing=T)
  pcs <- match(names(pcs),names(medians))
  return(list(medians = medians, pcs = pcs))
}


