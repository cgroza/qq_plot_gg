library(ggplot2)
library(tibble)
library(dplyr)

qq_plot_gg <- function(x , y, quantiles = 100, quantiles_fill = T) {
  # use stats::qqplot to compute the bins
  qq <- as_tibble(qqplot(x, y, asp = 1, plot.it = F))

  # disable this feature if negative
  if(quantiles > 0) {
    # we need to take every nth element for the given number of quantiles
    # (roughly)
    nth_quantile <- max(floor(nrow(qq) / quantiles), 1)

    ## take every nth_quantile row from the qqplot output
    if (nth_quantile > 1) {
      qq <- filter(qq, row_number() %% nth_quantile == 1)
    }
  }

  # compute quantiles
  fi <- (1:nrow(qq) - 0.5) / nrow(qq)
  qq <- qq %>% add_column(Quantile = fi)

  # limits for the qq-plot
  xylim <- range( c(qq$x, qq$y) )
  p <- ggplot(qq, aes( x= x, y = y)) +
    geom_abline( intercept=0, slope=1) +
    coord_fixed(ratio = 1, xlim = xylim, ylim = xylim)
  # should we color points by their quantile?
  if(quantiles_fill) {
    p + geom_point(aes(color = Quantile))
  }
  else {
    p + geom_point()
  }
}
