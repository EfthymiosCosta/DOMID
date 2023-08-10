#' Calculate angle of elbow point in a decreasing convex function.
#'
#' @description
#' This function uses the kneedle algorithm for finding the knee of a decreasing convex function.
#' It then links the knee point to the lowest and highest points of the curve and calculates the
#' angle between these 2 line segements. If the 2 input arguments are very short vectors
#' (e.g. including just 5 elements) and no elbow is present, the angle value returned will be NA.
#'
#' @param vec Vector of values - the values should be decreasing and the vector should include at least 4 values.
#' @param range Range vector - this corresponds to the x values corresponding to the vec values. Values should be unique and increasing, while the vector needs to be of length equal to vec.
#'
#' @return A numeric value of the elbow angle in degrees.
#' @export
#'
#' @examples elbow_angle(vec = sort(rnorm(10), decreasing = TRUE), range = c(1:10))
#' @examples elbow_angle(vec = sort(rgamma(100, 1, 4), decreasing = TRUE), range = sort(rexp(100, 3)))
#' @examples elbow_angle(vec = sort(rpois(20, 8.4), decreasing = TRUE), range = seq(0, 90, length.out = 20))
#'
elbow_angle <- function(vec, range){
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
  ### END OF CHECKS ###
  # Check if range includes negative values and shift them towards 0
  if (length(range < 0) > 1){
    range <- range - min(range)
  }
  # First make range on same scale as vec
  range_scaled <- range * max(vec)
  # Find knee point
  elbow <- kneedle::kneedle(x = range_scaled, y = vec,
                            decreasing = TRUE, concave = FALSE)
  elbow_inx <- match(elbow[1], range_scaled)
  # Compute elbow angle
  angle <- atan(abs((elbow[1]-range_scaled[1])/(vec[elbow_inx]-vec[1]))) +
    atan(abs((elbow[1]-range_scaled[length(range_scaled)])/(vec[elbow_inx]-vec[length(range_scaled)])))
  angle <- angle*180/pi # Angle in degrees
  return(angle)
}
