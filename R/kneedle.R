#' Kneedle algorithm
#'
#' @param x Vector 1
#' @param y Vector 2
#' @param s s (default is 1)
#' @param decreasing Logical; is y decreasing?
#' @param concave Logical; is y = f(x) concave?
#'
#' @return Knee/Elbow point
#'
#' @noRd
kneedle <- function(x, y, s = 1, decreasing, concave){
  data <- matrix(unlist(list(x, y)), ncol = 2)
  data <- data[order(data[,1], decreasing = FALSE), ]
  maxy <- max(y)
  miny <- min(y)
  maxx <- max(x)
  minx <- min(x)
  data[ ,1] <- (data[, 1]- min(data[, 1]))/(max(data[ ,1])- min(data[, 1]))
  data[ ,2] <- (data[, 2]- min(data[, 2]))/(max(data[ ,2])- min(data[, 2]))
  if(concave && !decreasing) {
    differ <- abs(c(data[ ,2] - data[ ,1]))
  } else if(concave && decreasing) {
    differ <- abs(c(data[ ,2] - (1 - data[ ,1])))
  } else if(!concave && !decreasing) {
    differ <- abs(c(data[ ,2] - data[ ,1]))
  } else if(!concave && decreasing) {
    differ <- abs(c(data[ ,2] - (1 - data[ ,1])))
  }
  peak.indices <- quantmod::findPeaks(differ) - 1
  data <- cbind(data, differ)

  diffx <- diff(data[, 1])
  T.lm.x.s <- s * mean(diffx)
  knee <- NULL

  for (i in 1:length(peak.indices)){
    T <- data[peak.indices[i] ,3] - (T.lm.x.s)
    y.value <- data[peak.indices[i] ,3]
    for (j in (peak.indices[i]):if(i+1 < length(peak.indices)) peak.indices[i+1] else length(differ)){
      if (differ[j] < T) {
         knee <- peak.indices[i]
        break
      }
    }
    if(!is.null(knee)){
      break
    }
  }
  # Returns the x,y coordinate values
  x <- ((maxx - minx) * (data[knee, 1])) + minx
  y <- ((maxy - miny) * (data[knee, 2])) + miny
  return(c(as.numeric(x),as.numeric(y)))
}
