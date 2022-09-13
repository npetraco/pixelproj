#' Wrapper to quick fit a stack of registered images
#'
#' Wrapper to quick fit a stack of registered images. If this goes wrong, fit the stack step by
#' step to see where it fails.
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
mle.fit.stack <- function(img.list, num.sims, num.iters=1, grad.tol=1e-3){

  # Load stack and simulate more patterns
  loc.stk.sims <- simulate.pix.pattern(img.list, nsims=num.sims)  # Add seed to save??

  # Flatten patterns into sample matrix
  loc.samps           <- image.list2vec.mat(c(img.list, loc.stk.sims), white.pix.val = 1, black.pix.val = 2)
  stk.nr              <- nrow(img.list[[1]][,,1])
  stk.nc              <- ncol(img.list[[1]][,,1])
  colnames(loc.samps) <- 1:(stk.nr*stk.nc)

  # Generate the requisite lattice graph
  loc.graph <- make.lattice(num.rows = stk.nr, num.cols = stk.nc, cross.linksQ = F, node.names.vec = 1:(stk.nr*stk.nc) )

  loc.fit <- fit_mle_params(loc.graph, loc.samps,
                            parameterization.typ = "flexible",
                            opt.method           = "L-BFGS-B",
                            #opt.method           = "CG",
                            inference.method     = infer.junction,
                            state.nmes           = c("white","black"),
                            num.iter             = num.iters,
                            mag.grad.tol         = grad.tol,
                            plotQ                = T)

  fit.info <- list(loc.samps, loc.fit)
  names(fit.info) <- c("samples","fit")

  return(fit.info)

}
