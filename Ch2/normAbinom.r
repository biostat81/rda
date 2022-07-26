normAbinom <- function() { # normAbinom.r
  x <- seq(5, 50, by=5)
  p.binom <-  pbinom(x, 50, 0.5)
  p.norm <- pnorm(x, 25, sqrt(12.5))
  diff <- p.binom-p.norm
  list(p.binom = round(p.binom,4), p.norm = round(p.norm,4), diff = round(diff,4))
 } # end function

normAbinom()
