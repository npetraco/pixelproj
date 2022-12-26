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

  # Move this below at some point
  if(plotQ == T) {
    plot(1:length(an.lbl.names), prob.vec.loc, typ="h", xlab="K", ylab="probability", main="Pr(X|K)")
  }

  # Best match:
  pr.best.match  <- max(prob.vec.loc)                    # Probability for the best match. May not be the true ID
  best.match.idx <- which(prob.vec.loc == pr.best.match) # There can be more than one....

  # True ID:
  true.lbl   <- as.numeric(an.fp.info.mat[an.idx, 3]) # Numeric label and index for the true ID
  true.ID    <- an.lbl.names[ true.lbl ]              # string for the true ID
  pr.true.ID <- prob.vec.loc[true.lbl]                    # probability for the true ID

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
    as.factor(ID.indcs)
  )
  colnames(match.info) <- c("lbl.idx", "ID-K", "Pr(X|K)", "LR", "best.matchQ", "indicator")

  # Also return M**** vector with best.matchQ==trueID, number of best match and number >= min.prob
  mtvec <- data.frame(match.info[tr.idx.chk, ], as.logical(pr.true.ID == pr.best.match), length(best.match.idx), ncol(match.info))
  colnames(mtvec) <- c("lbl.idx", "ID-K", "Pr(X|K)", "LR", "best.matchQ", "indicator", "correctIDQ", "num.best", "num.all")
  #print(mtvec)
  #print(class(mtvec))

  all.match.info <- list(
    mtvec,
    match.info
  )
  names(all.match.info) <- c("Q.summary.vec", "match.info")

  return(all.match.info)
}
