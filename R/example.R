#' An example coverage dataframe
cov_example <- function() {
  df <- data.frame(
    "chr" =     c('chr1_A', 'chr1_A', 'chr2_A', 'chr2_A', 
                  'chr1_D', 'chr1_D', 'chr2_D', 'chr2_D'),
    "start0" =  c(   0, 1000,    0, 1000,    0, 1000,   0, 1000),
    "end0" =    c(1000, 2000, 1000, 2000, 1000, 2000,1000, 2000),
    "id" =      c('chr1_A_0', 'chr1_A_1', 'chr2_A_0', 'chr2_A_1', 
                  'chr1_D_0', 'chr1_D_1', 'chr2_D_0', 'chr2_D_1'),
    "sample1" = c(1.46, 0.62, 0.54, 0.56, 0.67, 0.55, 0.53, 0.48) ,
    "sample2" = c(2.35, 1.0,  0.84, 0.85, 1.01, 0.84, 0.97, 0.82),
    "sample3" = c(2.68, 1.45, 1.28, 1.46, 1.45, 1.31, 1.35, 1.2)
  )
  reverse_contigs='chr2_D'
  suba=c('chr1_A', 'chr2_A')
  subb=c('chr2_D', 'chr1_D')
  return(cov.init(cov_df=df, reverse_contigs = reverse_contigs, 
                  suba = suba, subb = subb))
}
