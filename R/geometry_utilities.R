#' Extract some geometric parameters of a thresholded feathering pattern
#' XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
geom_analysis <- function(thresh.img.obj, black.pixel.code=0, plotQ=F, plot.img.type="image.matrix"){

  # NOTE: function assumes image has been thresholded to binary

  if(class(thresh.img.obj)[1] == "Rcpp_Image"){
    img.mat <- thresh.img.obj[,,1]    # If a thresholded Rvision image object was sent in
  } else {
    img.mat <- thresh.img.obj         # If a thresholded image matrix was sent in
  }

  img.mat.logical <- (img.mat == black.pixel.code)

  nr.loc       <- nrow(img.mat)
  nc.loc       <- ncol(img.mat)
  num.blk.pix  <- sum(img.mat.logical) # num black pixels
  num.pix      <- nr.loc * nc.loc                  # num pixels
  perc.blk.pix <- num.blk.pix / num.pix *100       # %black pixels

  # Complure area of black convex hull
  blk.idxs <- which(img.mat == 0, arr.ind = T) # matrix indices of black pixels
  ch <- chull(blk.idxs)                        # convex hull around black pixels

  convex.hull.area       <- splancs::areapl(blk.idxs[ch,])
  shape.complexity.index <- ( 1 - convex.hull.area/num.blk.pix )

  if(plotQ == T) {

    convex.hull.mat <- img.mat
    for(i in 1:length(ch)) {

      row.num <- ch[i]
      r.idx   <- blk.idxs[row.num,1]
      c.idx   <- blk.idxs[row.num,2]

      convex.hull.mat[r.idx, c.idx] <- 128 # Mark out vertices of convex hull with mid-range pixel color (assuming 8-bit pixel depth)

      par(mfrow=c(1,1)) # In case it didn't reset
      par(mfrow=c(1,2))
      plot(convex.hull.mat[nr.loc:1,], key=NULL, main="" )
      plot(blk.idxs[ch,2], blk.idxs[ch,1], type="b", xlab="Column", ylab="Row", xlim=c(1,nc.loc), ylim=c(1,nr.loc))
      par(mfrow=c(1,1))
    }

  }

  geom.info.vec           <- t(as.matrix(c(nr.loc, nc.loc, num.blk.pix, num.pix, perc.blk.pix, shape.complexity.index)))
  colnames(geom.info.vec) <- c("num.rows", "num.cols", "num.black.pixels", "area", "perc.black.pixels", "shape.complexity.index")

  return(geom.info.vec)

}
