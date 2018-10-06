#' Create a color vector dataframe
#' @export
create_color_vector <- function(color_csv, subg1, subg2) {
  color <- read.csv(
    color_csv, sep='\t', col.names=c('chr', 'start', 'end', 'R', 'G', 'B'),
    header=F
  )
  cols <- vector(length=28)
  color$chr = c(subg1, subg2)
  for (i in 1:dim(color)[1]) {
    cols[color$chr[i]] <- rgb(color$R[i], color$G[i], color$B[i])
  }
  return(cols)
}
