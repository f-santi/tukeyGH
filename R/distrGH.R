
pgh <- function(x, a, b, g, h)
{
  if (h < 0 || b <= 0)
    try(if(h<0 || b <0) stop("b and h must be positive"))
  if (h>0 & b >0)
  {    
    source("pgh_scalar.R")
    f <- mapply(pgh_scalar, x, a, b, g, h)
  }
  if (h==0 & b >0)
  {
    source("pg.R")
    f <- mapply(pg_scalar, x, a, b, g)
  }
  return(f)
}
