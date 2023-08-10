#' Remove observations of high frequency, unlikely to be outliers
#'
#' @param disc_pts_cat Discarded points.
#' @param outscoredfcells Contributions to discrete scores.
#' @param thresh_1lvl Threshold values.
#'
#' @return Likely itemsets of unit length.
#'
rm_likely_1lvl <- function(disc_pts_cat, outscoredfcells, thresh_1lvl){
  likely_1lvl_items <- c()
  for (pts in intersect(disc_pts_cat, which(apply(outscoredfcells, FUN=function(c)sum(c!=0)==1, 1)))){
    # Non-zero column index
    col_inx <- which(outscoredfcells[pts,]!=0)
    thresh_val <- thresh_1lvl[which(thresh_1lvl[,1]==col_inx), 2]
    if (1/outscoredfcells[pts,col_inx]>0.90*thresh_val){
      likely_1lvl_items <- c(likely_1lvl_items, pts)
    }
  }
  return(likely_1lvl_items)
}
