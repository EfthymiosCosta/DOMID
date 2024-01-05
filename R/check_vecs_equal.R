#' Check if 2 vectors are the same
#'
#' @param vec1 Vector 1
#' @param vec2 Vector 2
#'
#' @return Sum of common elements of vec1 and vec2
#'
#' @noRd

check_vecs_equal <- function(vec1, vec2){
  sum(vec1==vec2)
}
