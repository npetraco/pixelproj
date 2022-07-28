#' Utility function for XXXXXX
#'
#' XXXX
#'
#' The function depends on the Rvision library which must be installed
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
chop.and.fill <- function(tmpl.in.ref.padded, reference.img, fill.typ, h.pad.leng, w.pad.leng){

  # Now take off the border:
  tmpl.in.ref <- tmpl.in.ref.padded[(h.pad.leng+1):(h.pad.leng+nrow(reference.img)),
                                    (w.pad.leng+1):(w.pad.leng+ncol(reference.img))]

  # Now fill any NAs with users choice
  tmpl.in.ref.filled  <- tmpl.in.ref
  tmpl.in.ref.na.idxs <- which(is.na(tmpl.in.ref) == T, arr.ind = T)

  if(fill.typ=="opposite"){
    # Fill NAs with the opposite of what is in the reference
    # This is like saying the NAs were not part of the pattern so they definitely
    # shouldn't be considered a match to the reference
    for(i in 1:nrow(tmpl.in.ref.na.idxs)){
      val <- reference.img[tmpl.in.ref.na.idxs[i,1],tmpl.in.ref.na.idxs[i,2],1]
      if(val == 0) {
        tmpl.in.ref.filled[tmpl.in.ref.na.idxs[i,1],tmpl.in.ref.na.idxs[i,2]] <- 255
      } else if(val == 255) {
        tmpl.in.ref.filled[tmpl.in.ref.na.idxs[i,1],tmpl.in.ref.na.idxs[i,2]] <- 0
      } else {
        print(tmpl.in.ref.na.idxs[i,])
        print(reference.img[tmpl.in.ref.na.idxs[i,1],tmpl.in.ref.na.idxs[i,2],1])
        stop("Weird pixel value encountered!")
      }
    }

  } else if(fill.typ=="devils.advocate") {

    for(i in 1:nrow(tmpl.in.ref.na.idxs)){
      # Fill NAs with the same pixel value as in the reference
      # This is like a "worst case scenario"
      val <- reference.img[tmpl.in.ref.na.idxs[i,1],tmpl.in.ref.na.idxs[i,2],1]

      tmpl.in.ref.filled[tmpl.in.ref.na.idxs[i,1],tmpl.in.ref.na.idxs[i,2]] <- val
    }

  } else if(fill.typ=="random") {

    for(i in 1:nrow(tmpl.in.ref.na.idxs)){
      # Fill NAs with one of the two pixel values at random
      val <- sample(c(0,255), size = 1)

      tmpl.in.ref.filled[tmpl.in.ref.na.idxs[i,1],tmpl.in.ref.na.idxs[i,2]] <- val
    }

  } else if(fill.typ=="0") {

    for(i in 1:nrow(tmpl.in.ref.na.idxs)){
      # Fill NAs with 0
      val <- 0

      tmpl.in.ref.filled[tmpl.in.ref.na.idxs[i,1],tmpl.in.ref.na.idxs[i,2]] <- val
    }

  } else if(fill.typ=="1") {

    for(i in 1:nrow(tmpl.in.ref.na.idxs)){
      # Fill NAs with 1
      val <- 1

      tmpl.in.ref.filled[tmpl.in.ref.na.idxs[i,1],tmpl.in.ref.na.idxs[i,2]] <- val
    }

  } else if(fill.typ=="255") {

    for(i in 1:nrow(tmpl.in.ref.na.idxs)){
      # Fill NAs with 255
      val <- 255

      tmpl.in.ref.filled[tmpl.in.ref.na.idxs[i,1],tmpl.in.ref.na.idxs[i,2]] <- val
    }

  } else {
    tmpl.in.ref.filled <- tmpl.in.ref.filled
  }

  return(tmpl.in.ref.filled)

}
