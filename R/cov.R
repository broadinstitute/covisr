#' Creat new coverage object
#' @description Create an Coverage object, with preprocessing includes
#' a) clean up background signal
#' b) inverse positions of contigs on the reverse strand
#' c) reorder subgenome contigs for ease of comparsion between subgenomes
#' @param cov_tsv coverage tsv file
#' @param cutoff coverage cutoff for background noise
#' @param suba contig orders for subgenome A
#' @param subb contig orders for subgenome B, matching that of subgenome A
#' @param first_sample_idx first sample index (1-based index) in the input tsv,
#' default 5
#' @return coverage object
#' @export
cov.init <- function (cov_tsv, cutoff=0, prefix='output',
                      reverse_contigs=NULL, suba=NULL, subb=NULL,
                      first_sample_idx=5) {
  cov = list()
  cov$df <- read.csv(cov_tsv, sep='\t')
  cov$cutoff <- cutoff
  cov$prefix <- prefix
  cov$reverse_contigs <- reverse_contigs
  cov$suba <- suba
  cov$subb <- subb
  cov$n_samples <- dim(cov$df)[2] - first_sample_idx + 1
  cov$first_sample_idx <- first_sample_idx
  class(cov) <- 'cov'   # create class
  return(cov)
}


cov.preprocess <- function(cov) {
  .is.cov(cov)
  # preprocessing
  cov <- cov.add_sample_idx(cov)
  cov <- cov.add_bin_idx(cov)
  if(!is.null(cov$cutoff)) cov <- cov.filter_background(cov)
  if(!is.null(cov$reverse_contigs)) cov <- cov.reverse_idx(cov)
}


# Inverse bin index of contigs to the complement strand
#' @export
cov.reverse_idx <- function(cov) {
  .is.cov(cov)
  if(is.null(cov$reverse_contigs)) stop("reverse_contigs cannot be null.")
  contigs <- cov.get_contigs(cov)
  cov$df$reversed <- cov$df$chr %in% cov$reverse_contigs
  for (i in cov$reverse_contigs) {
    idx = which(cov$df$chr == i)
    cov$df$pos[idx] = rev(cov$df$pos[idx])
  }
  return(cov)
}


#' Remove background noise
#' @param cov object
#' @return covobj
#' @export
cov.filter_background <- function (cov){
  .is.cov(cov)
  if (!all(c('sample_idx', 'cutoff') %in% names(cov))) {
    stop('sample_idx or cutoff not available in coverage list')
  }
  cov$df[,cov$sample_idx] = apply(
    cov$df[,cov$sample_idx], c(1,2), function(x) if(x< cov$cutoff) 0 else x)
  return(cov)
}


#' Get sample index
#' @param cov coverage list
#' @return coverage obj
#' @export
cov.add_sample_idx <- function(cov) {
  .is.cov(cov)
  cov$n_sample = dim(cov$df)[2] - cov$first_sample_idx + 1
  cov$sample_idx = cov$first_sample_idx:cov$n_samples
  return(cov)
}


#' Get unique contigs from the coverage object
#' @param cov coverage object
#' @return contig vector
#' @export
cov.get_contigs <- function(cov) {
  .is.cov(cov)
  return(unique(cov$df$chr))
}


#' add bin index for coverage object
#' @description Bin index here is used as a proxy for physical location of each
#' bin. This assumes bins of the genome have identical size.
#' @param cov coverage object
#' @param reorder optional, whether to reorder the bin index by matching order
#' order of subgenomes. This option is useful when contrasting homologous
#' contigs
#' @return coverage object
cov.add_bin_idx <- function(cov, reorder=T) {
  .is.cov(cov)
  if (reorder) cov <- cov.reorder_contigs(cov)
  cov$df$bidx = 1:dim(cov$df)[1]
  return(cov)
}


#' Reorder position by defined chromosome orders in both subgenomes
#' @param cov coverage object
#' @param contig_order order of contigs
#' @return updated data.frame
#' @export
cov.reorder_contigs <- function (cov) {
  .is.cov(cov)
  if(is.null(cov$suba) || is.null(cov$subb)){
    stop('Both suba and subb should be available in the coverage object!')
  }
  contig_order <- c(cov$suba, cov$subb)
  # sort data.frame according to given contig order
  cov$df = cov$df[order(match(cov$df$chr, contig_order)), ]
  return(cov)
}
