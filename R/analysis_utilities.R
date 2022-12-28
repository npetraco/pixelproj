#' Analyze probs of feathing patters on each feathering pattern set
#' XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
match.analysis <- function(an.idx, prob.vec, an.fp.info.mat, an.lbl.names, min.prob=0.001, printQ=F, plotQ=F){

  prob.vec.loc <- as.numeric(prob.vec)

  # Best match:
  pr.best.match  <- max(prob.vec.loc)                    # Probability for the best match. May not be the true ID
  best.match.idx <- which(prob.vec.loc == pr.best.match) # There can be more than one....

  # True ID:
  true.lbl   <- as.numeric(an.fp.info.mat[an.idx, 3]) # Numeric label and index for the true ID
  true.ID    <- an.lbl.names[ true.lbl ]              # string for the true ID
  pr.true.ID <- prob.vec.loc[true.lbl]                # probability for the true ID

  # Examine all probs greater than the specified minimum
  check.idxs <- which(prob.vec.loc >= min.prob)
  # Lump true match into check.idxs in case it didn't make it because it was below min.prob
  check.idxs <- sort(unique(c(check.idxs, true.lbl)))

  # Label the true match (M) and the rest non-match (NM)
  tr.idx.chk           <- which(an.lbl.names[check.idxs] == true.ID) # Pull out true match or label it
  ID.indcs             <- rep("NM", length(check.idxs))
  ID.indcs[tr.idx.chk] <- "M****"

  # Logical indicator if Pr(X|K) is best match. Ideally, best match is the highest prob (by far), and is unique
  best.matchQ <- as.numeric(prob.vec.loc[check.idxs]) == pr.best.match

  # Compute LRs for each prob above the specified minimum
  LRs <- array(NA, length(check.idxs))
  for(i in 1:length(check.idxs)) {

    numerator   <- as.numeric(prob.vec.loc[check.idxs[i]])                                         # Pr(X|K)
    denominator <- sum( as.numeric(prob.vec.loc[-check.idxs[i]]) * 1/ (length(an.lbl.names) - 1) ) # Pr(X)
    LR          <- numerator / denominator                                                     # Pr(X|K)/Pr(X)
    LRs[i]      <- LR

  }

  # Throw warning if true match is not the top match??
  # Throw warning if multiple probs are above min.prob??

  match.info <- data.frame(
    check.idxs,
    an.lbl.names[check.idxs],
    as.numeric(prob.vec.loc[check.idxs]),
    as.numeric(LRs),
    best.matchQ,
    ID.indcs
  )
  colnames(match.info) <- c("lbl.idx", "ID-K", "Pr(X|K)", "LR", "best.matchQ", "indicator")

  # Also return M**** vector with best.matchQ==trueID, number of best match and number >= min.prob
  mtvec <- data.frame(match.info[tr.idx.chk, ], as.logical(pr.true.ID == pr.best.match), length(best.match.idx), nrow(match.info))
  colnames(mtvec) <- c("lbl.idx", "ID-K", "Pr(X|K)", "LR", "best.matchQ", "indicator", "correctIDQ", "num.best", "num.abv.min.prob")
  #print(mtvec)
  #print(class(mtvec))

  # Plot Pr(X|K) for all K
  if(plotQ == T) {
    plot(1:length(an.lbl.names), prob.vec.loc, typ="h", xlab="K", ylab="probability", main="Pr(X|K)")
    points(true.lbl, pr.true.ID, pch=4) # Mark the correct ID with an x
  }

  if(printQ == T){
    print(paste0("FP#:                                    ", an.idx ))
    print(paste0("K:                                      ", mtvec[1] ))
    #print(paste0("indicator:                              ", mtvec[6] )) # Should always just be M**** indicating this K is the true match
    print(paste0("true ID of K:                           ", mtvec[2] ))
    print(paste0("best match? [Pr(X|K)    == max(prob)]:  ", mtvec[5] ))
    print(paste0("correct ID? (best match == correct ID): ", mtvec[7] ))
    print(paste0("#K best match:                          ", mtvec[8] ))
    print(paste0("min.prob:                               ", min.prob ))
    print(paste0("#K geq min.prob:                        ", mtvec[9] ))
    print(paste0("Pr(X|K):         ", mtvec[3] ))
    print(paste0("LR:              ", mtvec[4] ))
  }

  all.match.info <- list(
    mtvec,
    match.info
  )
  names(all.match.info) <- c("Q.summary.vec", "match.info")

  return(all.match.info)
}


#' Check a Q feathering pattern against a K
#' Check a Q feathering pattern against a K. Handy for checking miss-IDed fps
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
check.analysis <- function(a.Q.img.num, a.K.set.num, K.set.fnames, an.fp.info.mat, fp.file.names, reg.fill.typ = "255", path.to.fp.images, path.to.fp.fits, printQ=F, plotQ=F, plot.level=0){

  # Build path to K.set to check against:
  K.pth.chk      <- paste0(path.to.fp.images, K.set.fnames[a.K.set.num])

  # Load the K.set and take thier mean as the correlation reference:
  K.stk.chk      <- thresh.img.obj(image_read(K.pth.chk), type = "individual.mean")
  K.mean.img.chk <- mean.stack.img(K.stk.chk) # This will be the reference for registration

  print(paste0("K Image stack: ", a.K.set.num, ", filename: ", K.set.fnames[a.K.set.num]))
  if(plotQ==T & plot.level >=2) {

    # Plot mean of K.set in two ways, side by side
    par(mfrow=c(1,1)) # In case it didn't reset
    par(mfrow=c(1,2))
    plot(K.mean.img.chk)
    plot.stack(list(K.mean.img.chk), num.cycles = 1, delay = 0.1, type = "image.matrix", main.title = paste0("K (mean): ", K.set.fnames[a.K.set.num]) )
    par(mfrow=c(1,1))

    invisible(readline(prompt="Press [enter] to continue"))
  }

  # Load the Q to compare
  Q.img.info.vec <- an.fp.info.mat[a.Q.img.num, ]
  print(paste0("Q FP lbl:       ", Q.img.info.vec[3], ", name:     ", Q.img.info.vec[2], ", FP image# = ", Q.img.info.vec[1]))

  Q.pth <- paste0(path.to.fp.images, fp.file.names[ an.fp.info.mat[a.Q.img.num, 3] ])
  Q.stk <- thresh.img.obj(image_read(Q.pth), type = "individual.mean")
  Q.img <- Q.stk[[ an.fp.info.mat[a.Q.img.num, 6] ]] # Select the right rep number

  if(plotQ==T & plot.level >=2) {

    # Plot mean of K.set in two ways, side by side
    par(mfrow=c(1,1)) # In case it didn't reset
    par(mfrow=c(1,2))
    plot(Q.img)
    plot.stack(list(Q.img), num.cycles = 1, delay = 0.1, type = "image.matrix", main.title = paste0("Q: ", Q.img.info.vec[2]))
    par(mfrow=c(1,1))

    invisible(readline(prompt="Press [enter] to continue"))
  }

  # Register Q to K
  par(mfrow=c(1,1)) # In case it didn't reset
  Q.img.mat.reg <- register.template(
    template.img  = Q.img,          # The specific Q in the stack
    reference.img = K.mean.img.chk, # Take mean image of stack as the reference to align to
    fill.typ      = reg.fill.typ,   # Fill for missing parts: "devils.advocate" (assume missing parts are the same as the reference), "opposite" (assume missing parts the opposite of the reference), "random" (random values for missing parts), "0" (missing parts all black), "255" (missing parts all white)
    printQ        = printQ,
    plotQ         = plotQ)

  if(plotQ==T & plot.level >=1) {

    # Plot mean of K.set in two ways, side by side
    par(mfrow=c(1,1)) # In case it didn't reset
    par(mfrow=c(1,2))

    # Q before reg side-by-side with K:
    plot.stack(list(Q.img), num.cycles = 1, delay = 0.1, type = "image.matrix", main.title = paste0("Q before reg: ", Q.img.info.vec[2]))
    plot.stack(list(K.mean.img.chk), num.cycles = 1, delay = 0.1, type = "image.matrix", main.title = paste0("K (mean): ", K.set.fnames[a.K.set.num]) )

    invisible(readline(prompt="Press [enter] to continue"))

    # Q after reg side-by-side with K:
    plot(Q.img.mat.reg[nrow(Q.img.mat.reg):1,], key=NULL, main=paste0("Q after reg: ", Q.img.info.vec[2]) )
    plot(K.mean.img.chk[nrow(K.mean.img.chk):1,,1], key=NULL, main=paste0("K (mean): ", K.set.fnames[a.K.set.num]))
    par(mfrow=c(1,1))
  }

  if(plotQ==T & plot.level == 0) {

    # Plot mean of K.set in two ways, side by side
    par(mfrow=c(1,1)) # In case it didn't reset
    par(mfrow=c(2,2))

    # Q before reg side-by-side with K:
    plot.stack(list(Q.img), num.cycles = 1, delay = 0.1, type = "image.matrix", main.title = paste0("Q before reg: ", Q.img.info.vec[2]))
    plot.stack(list(K.mean.img.chk), num.cycles = 1, delay = 0.1, type = "image.matrix", main.title = paste0("K (mean): ", K.set.fnames[a.K.set.num]) )

    #invisible(readline(prompt="Press [enter] to continue"))

    # Q after reg side-by-side with K:
    plot(Q.img.mat.reg[nrow(Q.img.mat.reg):1,], key=NULL, main=paste0("Q after reg: ", Q.img.info.vec[2]) )
    plot(K.mean.img.chk[nrow(K.mean.img.chk):1,,1], key=NULL, main=paste0("K (mean): ", K.set.fnames[a.K.set.num]))
    par(mfrow=c(1,1))
  }

  load(file = paste0(path.to.fp.fits, K.set.fnames[a.K.set.num], "_fit_info.RData") ) # Load parameters and logZ for the K
  plr.vec <- prob.pattern(pattern.mat=Q.img.mat.reg, fit.obj$fit, logZ)

  print(plr.vec)

  check.info.list <- list(
    Q.pth,
    K.pth.chk,
    Q.stk,
    Q.img,
    Q.img.mat.reg,
    K.stk.chk,
    K.mean.img.chk,
    plr.vec
  )
  names(check.info.list) <- c(
    "Q.pth",
    "K.pth",
    "Q.stk",
    "Q.img",
    "Q.img.mat.reg",
    "K.stk",
    "K.mean.img.chk",
    "calcs"
  )

  return(check.info.list)

}
