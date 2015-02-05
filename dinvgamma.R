dinvgamma = function (x, shape, scale = 1, log=FALSE) 
{
  if (shape <= 0 | scale <= 0) {
    stop("Shape or scale parameter negative in dinvgamma().\n")
  }
  alpha <- shape
  beta <- scale
  log.density <- alpha * log(beta) - lgamma(alpha) - (alpha + 
                                                        1) * log(x) - (beta/x)
  return(ifelse(log, log.density, exp(log.density)))
}
