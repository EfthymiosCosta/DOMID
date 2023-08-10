#' Method of consecutive angles.
#'
#' @description This function implements the method of consecutive angles for determining
#' the point where a large drop occurs in a more efficient way than the kneedle algorithm.
#' The function takes 2 input arguments vec and range which corresponding to the vector
#' of overvations and their corresponding x values. The method requires setting a value
#' for drop_tol, that is the tolerated level of drop in consecutive vec values for which
#' a drop is not considered significant. The default value is 3 assuming integer values in vec
#' but this can be modified. If the method of consecutive angles returns
#' a very large value in the range vector that does not seem to be plausible,
#' the parameter range_tol can restrict the search, so that if the index of the value where
#' a drop is found exceeds range_tol, the kneedle algorithm is used and the elbow point is
#' returned instead.
#'
#' @param vec Vector of values - the values should be decreasing and the vector should include at least 4 values.
#' @param range Range vector - this corresponds to the x values corresponding to the vec values. Values should be unique and increasing, while the vector needs to be of length equal to vec.
#' @param drop_tol Tolerated level of drop in consecutive vec values for which
#' a drop is not considered significant. The default value is 3 assuming integer values in vec
#' but this can be modified.
#' @param range_tol Parameter used to restrict the search, so that if the index of the value where
#' a drop is found exceeds range_tol, the kneedle algorithm is used and the elbow point is
#' returned instead. Set this to the maximum value in range if you don't want such a restriction.
#'
#' @return Value of range beyond which there is no longer a significant drop in consecutive vec values.
#' @export
#'
#' @examples consec_angles(vec = sort(rnorm(10), decreasing = TRUE), range = c(1:10), drop_tol = 0.7, range_tol = 8)
consec_angles <- function(vec, range, drop_tol = 3, range_tol){
  ### INPUT CHECKS ###
  stopifnot("The 2 parameters must be vectors of the same length." = length(vec)==length(range))
  if (length(vec) < 4 | length(range) < 4){
    stop("Input arguments should be vectors of length at least equal to 4.")
  }
  if (!is.numeric(vec) | !is.numeric(range)){
    stop("Input arguments should be vectors of class 'numeric'.")
  }
  if (any(diff(vec) > 0)){
    stop("Vector of values should consist of decreasing values.")
  }
  stopifnot("Range vector should consist of strictly increasing unique values." = !is.unsorted(range, strictly = TRUE))
  stopifnot("Argument drop_tol should be a positive number." = is.numeric(drop_tol))
  stopifnot("Argument drop_tol should be a positive number." = (length(drop_tol) == 1))
  stopifnot("Argument drop_tol should be a positive number." = (drop_tol > 0))
  stopifnot("Argument range_tol should be a positive integer." = is.numeric(range_tol))
  stopifnot("Argument range_tol should be a positive integer." = (range_tol %% 1 == 0))
  if (range_tol < 1 | range_tol > length(range)){
    stop("Argument range_tol should be a positive integer from 1 up to the length of range.")
  }
  ### END OF CHECKS ###
  # Check if range includes negative values and shift them towards 0
  shift <- 0
  if (length(range < 0) > 1){
    shift <- min(range)
    range <- range - shift
  }
  # Angles with horizontal axis
  angle_h <- c()
  for (i in 2:(length(range))){
    angle_h <- c(angle_h, atan(abs(vec[i]-vec[i-1])/(range[i]-range[i-1])))
  }
  # Spot 2 consecutive equal angles & no large drops
  for (i in 1:(length(angle_h)-1)){
    if ((angle_h[i] == angle_h[i+1]) & (vec[i]-vec[i+1] < drop_tol)){
      break
    }
  }
  ifelse(i >= range_tol,
         Lambda_i <- kneedle::kneedle(x = range, y = vec,
                                      decreasing = TRUE, concave = FALSE)[1] + shift,
         Lambda_i <- range[i] + shift)
  return(Lambda_i)
}
