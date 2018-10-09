#' Creat new coverage object
#' @description Create an Coverage object, with preprocessing includes
#' a) clean up background signal
#' b) inverse positions of contigs on the reverse strand
#' c) reorder subgenome contigs for ease of comparsion between subgenomes
#' @param cov_tsv coverage tsv file, either this paramter of cov_tsv must be 
#' specified, if both specified, cov_tsv will be used. 
#' @param cov_df coverage dataframe, either this paramter of cov_tsv must be 
#' specified
#' @param cutoff coverage cutoff for background noise
#' @param suba contig orders for subgenome A
#' @param subb contig orders for subgenome B, matching that of subgenome A
#' @param sub_suffix subgenome suffix
#' @param first_sample_idx first sample index (1-based index) in the input tsv,
#' default 5
#' @return coverage object
#' @export
cov.init <- function (cov_tsv=NULL, cov_df=NULL, cutoff=0, prefix='output',
                      reverse_contigs=NULL, suba=NULL, subb=NULL, 
                      sub_suffix=NULL, first_sample_idx=5, preprocess=T) {
  cov = list()
  if (is.null(cov_tsv) & is.null(cov_df)) {
    stop('Either cov_tsv or cov_df should be provided.')
  } else if (!is.null(cov_tsv)) {
    if (!is.null(cov_tsv) & !is.null(cov_df)) {
      warning(paste("Both cov_tsv and cov_df is specified, cov_tsv will be",
                    "used as input", sep=' '))
    }
    cov$df <- read.csv(cov_tsv, sep='\t')
  } else {
    cov$df <- cov_df
  }
  cov$cutoff <- cutoff
  cov$prefix <- prefix
  cov$reverse_contigs <- reverse_contigs
  cov$n_samples <- dim(cov$df)[2] - first_sample_idx + 1
  cov$first_sample_idx <- first_sample_idx
  cov$sub_suffix <- sub_suffix
  if(!is.null(sub_suffix)) {
    cov[[sub_suffix[1]]] <- suba
    cov[[sub_suffix[2]]] <- subb
  }
  class(cov) <- 'cov'   # create class
  if (preprocess) {
    cov <- cov.preprocess(cov)
  }
  return(cov)
}


cov.preprocess <- function(cov) {
  .is.cov(cov)
  # preprocessing
  cov <- cov.add_sample_idx(cov)
  cov <- cov.add_bin_idx(cov)
  if(!is.null(cov$cutoff)) cov <- cov.filter_background(cov)
  if(!is.null(cov$reverse_contigs)) cov <- cov.reverse_idx(cov)
  return(cov)
}


# Inverse bin index of contigs to the complement strand
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
cov.add_sample_idx <- function(cov) {
  .is.cov(cov)
  cov$sample_idx = cov$first_sample_idx:(cov$n_samples+cov$first_sample_idx-1)
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
  if (reorder) {
    if (cov.has_subgenome(cov)) {
      cov <- cov.reorder_contigs(cov)
    } else{
      stop('Suba and subb should be both present for reorder')
    }
  } 
  cov$df$bidx = 1:dim(cov$df)[1]
  return(cov)
}


cov.has_subgenome <- function (cov) {
  return(all(!is.null(cov$sub_suffix), !is.null(cov[sub_suffix[1]]),
             !is.null(cov$sub_suffix[2])))
}


#' Reorder position by defined chromosome orders in both subgenomes
#' @param cov coverage object
#' @param contig_order order of contigs
#' @return updated data.frame
cov.reorder_contigs <- function (cov) {
  .is.cov(cov)
  if(!cov.has_subgenome(cov)){
    stop('Both suba and subb should be available in the coverage object!')
  }
  contig_order <- c(cov[[cov$sub_suffix[1]]], cov[[cov$sub_suffix[2]]])
  # sort data.frame according to given contig order
  cov$df = cov$df[order(match(cov$df$chr, contig_order)), ]
  return(cov)
}
