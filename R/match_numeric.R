#' Match numeric
#'
#'
match_numeric <- function(x, table){
  are.equal <- function(x, y) isTRUE(all.equal(x, y))
  match.one <- function(x, table)
    match(TRUE, vapply(table, are.equal, logical(1L), x = x))
  vapply(x, match.one, integer(1L), table)
}
