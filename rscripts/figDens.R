## Figure density
figDens <- function(seqx, means, sds = rep(1, length(means)), pal = 1:length(means),
  xlab = "Bio-tracer value", ylab = "Density", xlim = c(-5.1, 5.1), ylim = c(0, .4), ...) {
  plot0(xlim, ylim)
  box(bty = "l")
  for (i in seq_along(means)) lines(seqx, dnorm(seqx, means[i], sds[i]), col = pal[i], ...)
  mtext(1, text = xlab, cex = 1.8, line = 1.1)
  mtext(2, text = ylab, cex = 1.8, line = 1.1)
}

##Figure twoDims
figTwoDims <- function(col1, col2) {

  plot0(c(-5, 5), c(-5, 5))
  dfO1 <- data.frame(
    x = rnorm(30, -1, 1),
    y = rnorm(30, 0, 2)
  )
  dfO2 <- data.frame(
    x = rnorm(30, 0, .2),
    y = rnorm(30, 0, 2)
  )
  ########
  points(dfO1$x, dfO1$y, col = col2, pch = 19, cex = 1.5)
  points(dfO2$x, dfO2$y, col = col1, pch = 19, cex = 1.2)
  #######
  box(bty ="l")
  mtext(1, text = "Biotracer 1", line = 1.2, cex = 1.4)
  mtext(2, text = "Biotracer 2", line = 1.2, cex = 1.4)
}




##---Figure one Dim
figOneDim <- function(col1, col2) {

  plot0(c(-5,5), c(-1, 1))
  ##
  text(c(-1, 1), rep(.8, 2), labels = c("Origin 1", "Origin 2"), col = c(col1, col2), cex = 1.6)
  ########
  text(0, .35, labels = "Biotracer 1", cex = 1.1)
  points(rnorm(30, 1, 1), rep(.2, 30), col = col2, pch = 19, cex = 1.5)
  points(rnorm(30, -1, 1), rep(.2, 30), col = col1, pch = 19, cex = 1.2)
  ##
  lines(c(-4.6, 4.6), rep(0, 2))
  text(c(-4.5, 4.5), rep(-.07, 2), labels = c("min", "max"), cex = 1.2)
  text(0, -.1, labels = "Values", cex = 1.4)
  ########
  text(0, -.5, labels = "Biotracer 2", cex = 1.1)
  points(rnorm(30, 0, .2), rep(-.65, 30), col = col2, pch = 19, cex = 1.5)
  points(rnorm(30, 0, 2), rep(-.65, 30), col = col1, pch = 19, cex = 1.2)
  ##
  lines(c(-4.6, 4.6), rep(-.85, 2))
  text(c(-4.5, 4.5), rep(-.92, 2), labels = c("min", "max"), cex = 1.2)
  text(0, -.9, labels = "Values", cex = 1.4)

}
