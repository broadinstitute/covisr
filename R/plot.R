#' Overlay coverage on top of each other in one plot
#' @param cov coverage df
#' @param prefix output figure prefix
#' @param cols color vector for each contig
#' @export
cov.lineplot <- function (cov) {
  .is.cov(cov)
  sample_names = names(cov$df)[cov$sample_idx]
  sample_colors = get_colors(cov$n_samples)

  # generate line plots
  pdf(paste(prefix, '_lineplot.pdf', sep=''), width = 20, height = 8)
  par(mfrow=c(2,1)) # create two panels
  # plot for each subgenome
  for (subg in cov$sub_suffix) {
    has_chr_label = F  # whether plot has chr label on x axis
    subcovdf = cov$df[which(grepl(subg, cov$df$chr)), ]

    plot(1, ylim=c(0,5), cex = 0, xaxt='n', xlab=NA, ylab='cov', 
         xlim=c(min(subcovdf$bidx), max(subcovdf$bidx)), 
         main=paste('subgenome', subg, sep=''))
    # plot each sample
    for (sample_idx in 1:length(sample_names)) {
      lines(subcovdf$bidx, subcovdf[[sample_names[sample_idx]]], 
            col=sample_colors[sample_idx])
      # add figure legends
      if (!has_chr_label) {
        for (chr in cov[[subg]]) {
          mtext(chr, side=1, at=mid_pos(subcovdf$bidx[subcovdf$chr==chr]), 
                cex=0.8)
          abline(v=max(subcovdf$bidx[subcovdf$chr==chr]))
        }
        has_chr_label = T
      }
    }
  }
  dev.off()
}

#' To do: cov.heatmap
