## Figure density
figDens <- function(seqx, means, sds = rep(1, length(means)), pal = 1:length(means),
  xlab = "Values taken by the biotracer", ylab = "Density", xlim = c(-5, 5), ylim = c(0, .5), ...) {
  plot0(xlim, ylim)
  for (i in 1:length(means)) lines(seqx, dnorm(seqx, means[i], sds[i]), col = pal[i], ...)
  mtext(1, text = xlab, cex = 1.48)
  mtext(2, text = ylab, cex = 1.48)
}
