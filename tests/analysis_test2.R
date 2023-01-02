ridx <- low.LR.idxs[392]
fp.lbl.info[ridx, ]
mi2   <- match.analysis(
  an.idx = ridx,
  prob.vec = probs.mat[ridx, ],
  an.fp.info.mat = fp.lbl.info,
  min.prob = 0.001,
  an.lbl.names = fp.stack.names,
  printQ = T,
  plotQ = T)
mi2$match.info
mi2$Q.summary.vec


mi   <- match.analysis_SAFE(
  an.idx = ridx,
  prob.vec = probs.mat[ridx, ],
  an.fp.info.mat = fp.lbl.info,
  min.prob = 0.001,
  an.lbl.names = fp.stack.names,
  printQ = T,
  plotQ = F)
mi$match.info
