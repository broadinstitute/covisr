#' Subgenome matching between contigs in A and D
#' @export
remove_subg <- function (contig_vec) {
  contig_vec <- gsub('_[AD]', '', contig_vec)
  contig_vec <- gsub('chr', '', contig_vec)
  return(as.factor(as.numeric(contig_vec)))
}


#' @title reverse coordinates
#' @description reverse coordinates to the reverse strand, for ease of
#' comparing two subgenomes with different strand order
#' @param vec input coordinate vector
#' @return a reverse vector of coordinates
#' @examples
#' vec <- c(1, 3, 6, 10)
#' rvec <- reverse_coord(vec)
#' rvec
#' [1] 1 5 8 10
#' @export
reverse_coord <- function(vec) {
  n_elem <- length(vec)
  dist <- vec[n_elem] - vec[(n_elem-1):1]
  reverse_coord <- c(vec[1], vec[1] + dist)
  return(reverse_coord)
}


#' get mid position of a position vector
#' @param pos_vec position vector
mid_pos <- function(pos_vec) {
  return(mean(c(min(pos_vec), max(pos_vec))))
}

.is.cov <- function(obj) {
  if (class(obj) != 'cov' ){
    stop("Input should be an coverage object")
  }
  return(TRUE)
}
