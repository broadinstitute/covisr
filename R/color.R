#' Create a color vector dataframe
#' @export
legacy_colors <- function(cov, color_csv=NULL) {
  if (!is.null(color_csv)) {
    color <- read.csv(
      color_csv, sep='\t', col.names=c('chr', 'start', 'end', 'R', 'G', 'B'),
      header=F
    )    
  } else {
    color = data.frame(
      'chr'=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
      'start'=c(1, 461, 787, 1208, 1564, 1865, 2152),
      'end'=c(461, 787, 1208, 1564, 1865, 2152, 2421),
      'R'=c(0.933333, 0, 0.462745, 0.933333, 0.333333, 0, 0.933333),
      'G'=c(0, 0, 0.933333, 0.070588, 0.101961, 0.803922, 0.25098),
      'B'=c(0, 1, 0, 0.537255, 0.545098, 0, 0)
    )
  }
  cols <- vector(length=28)
  names(cols) <- c(cov[[cov$sub_suffix[1]]], cov[[cov$sub_suffix[2]]])
  for (i in 1:28) {
    if (i <= 14) {
      cols[chrs[i]] <- rgb(color$R[i], color$G[i], color$B[i])
    } else{
      cols[chrs[i]] <- rgb(color$R[i-14], color$G[i-14], color$B[i-14])
    }
  }
  return(cols)
}


get_colors<- function(n, seed=0){
  set.seed(seed)
  colors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  return(sample(colors, n))
}

