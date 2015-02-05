dinvgamma = function (x, shape, scale = 1, log=FALSE) 
{
  if (shape <= 0 | scale <= 0) {
    stop("Shape or scale parameter negative in dinvgamma().\n")
  }
  alpha <- shape
  beta <- scale
  density <- alpha * log(beta) - lgamma(alpha) - (alpha + 1) * log(x) - (beta/x)
  if (!log) density = exp(density)
  return(density)
}
