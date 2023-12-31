% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rm_likely_1lvl.R
\name{rm_likely_1lvl}
\alias{rm_likely_1lvl}
\title{Remove observations of high frequency, unlikely to be outliers}
\usage{
rm_likely_1lvl(disc_pts_cat, outscoredfcells, thresh_1lvl)
}
\arguments{
\item{disc_pts_cat}{Discarded points.}

\item{outscoredfcells}{Contributions to discrete scores.}

\item{thresh_1lvl}{Threshold values.}
}
\value{
Likely itemsets of unit length.
}
\description{
Remove observations of high frequency, unlikely to be outliers
}
