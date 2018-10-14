GetSplines <- function(
  x.i,
  x0 = NULL,
  I = 2.5,
  degree = 3
) {
  if (is.null(x0)) {
    x0 <- max(x.i)-0.5*I
  }
  # get knots, given that one knot needs to be in year0
  knots <- seq(x0-1000*I, x0+1000*I, I)
  while (min(x.i) < knots[1]) knots <- c(seq(knots[1]-1000*I, knots[1]-I,I), knots)
  while (max(x.i) > knots[length(knots)]) knots <- c(knots, seq(knots[length(knots)]+I,
                                                                knots[length(knots)]+1000*I, I))
  Btemp.ik <- bs(x.i, knots = knots[-c(1, length(knots))],  degree = degree,
                 Boundary.knots = knots[c(1, length(knots))])
  indicesofcolswithoutzeroes <- which(apply(Btemp.ik, 2, sum) > 0)
  # only remove columns with zeroes at start and end
  startnonzerocol <- indicesofcolswithoutzeroes[1]
  endnonzerocol <- indicesofcolswithoutzeroes[length(indicesofcolswithoutzeroes)]
  B.ik <- Btemp.ik[,startnonzerocol:endnonzerocol]
  colnames(B.ik) <- paste0("spline", seq(1, dim(B.ik)[2]))
  knots.k <- knots[startnonzerocol:endnonzerocol]
  names(knots.k) <- paste0("spline", seq(1, dim(B.ik)[2]))
  ##value<< List of B-splines containing:
  return(list(B.ik = B.ik, ##<< Matrix, each row is one observation, each column is one B-spline.
              knots.k = knots.k ##<< Vector of knots.
  ))
}