#' Register (best align) a template to a reference.
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
register.template <- function(template.img, reference.img, h.min.overlap=1, w.min.overlap=1, method="SQDIFF", fill.typ="opposite", printQ=F, plotQ=F){

  if(!(method %in% c("CCOEFF", "CCOEFF_NORMED", "CCORR", "CCORR_NORMED", "SQDIFF", "SQDIFF_NORMED"))){
    stop("method must be = CCOEFF, CCOEFF_NORMED, CCORR, CCORR_NORMED, SQDIFF, SQDIFF_NORMED")
  }

  if(!(fill.typ %in% c("NA", "opposite", "random", "devils.advocate", "0", "1", "255"))){
    stop("fill.typ must be NA, random, opposite, devils.advocate, 0, 1, 255")
  }

  # Make a padded ref wrt the tmp which will be shifting around over the ref:
  w <- ncol(template.img)
  h <- nrow(template.img)

  # Padding lengths
  h.pad.leng <- h - h.min.overlap
  w.pad.leng <- w - w.min.overlap

  # Pad the reference so that the template can slide over it
  padded.reference.img <- border(reference.img,
                                 top    = h.pad.leng,
                                 bottom = h.pad.leng,
                                 left   = w.pad.leng,
                                 right  = w.pad.leng,
                                 border_color = "white",
                                 border_type = "constant",
                                 target = "new")

  match.info <- matchTemplate(image = padded.reference.img,
                              template = template.img,
                              target = "new",
                              method = method)

  mm <- minMaxLoc(match.info)
  if(printQ==T){
    print(mm)
  }
  # If the method is SQDIFF or SQDIFF_NORMED, take minimum
  if(method=="SQDIFF" | method=="SQDIFF_NORMED"){
    min.loc <- mm[1,c(2,3)]
    x <- min.loc[1]        # Idx across this many
    y <- min.loc[2]        # Idx down this many
  } else {
    max.loc <- mm[2,c(2,3)]
    x <- max.loc[1]        # Idx across this many
    y <- max.loc[2]        # Idx down this many
  }

  # Embed the template in an array the same size as the padded reference
  # Any NAs should be from padding here on out
  tmpl.in.ref <- array(NA, dim(padded.reference.img)[c(1,2)])

  # Embed template into padded array which is the same size as the reference:
  tmpl.in.ref[y:(y+h-1), x:(x+w-1)] <- template.img[,,1]

  # Plot overlay of template aligned to reference if requested:
  if(plotQ == T){

    # This will be the plot canvas: an array the same size as the padded reference
    # Any NAs should be from padding here on out and will appear in white
    tmpl.in.ref.plot <- array(NA, dim(padded.reference.img)[c(1,2)])

    # Extract and re-scale the  reference's pixels
    ref.plot <- reference.img[,,1]
    ref.plot[which(ref.plot==255, arr.ind = T)] <- 2

    # Embed re-scaled ref in the tmpl.in.ref:
    tmpl.in.ref.plot[(h.pad.leng+1):(h.pad.leng+nrow(ref.plot)),
                     (w.pad.leng+1):(w.pad.leng+ncol(ref.plot))] <- ref.plot

    # Extract and re-scale the  template's pixels
    tmpl.plot                                     <- template.img[,,1]
    tmpl.plot[which(tmpl.plot==0, arr.ind = T)]   <- 4
    tmpl.plot[which(tmpl.plot==255, arr.ind = T)] <- 6

    # Overlay re-scaled template onto padded rescaled reference and see what we get:
    tmpl.in.ref.plot[y:(y+h-1), x:(x+w-1)] <- tmpl.plot[,]

    # Plot:
    plot(tmpl.in.ref.plot[nrow(tmpl.in.ref.plot):1,], col=c("blue","red","green","orange"), breaks = c(0,1,3,5,7),
         key=NULL)
    key.info <- cbind(c("blue","red","green","orange"), c("0   reference", "255 reference", "0   template", "255 template"))
    colnames(key.info) <- c("color", "pixel")
    print(key.info)

  }

  # Now take off the border:
  tmpl.in.ref <- tmpl.in.ref[(h.pad.leng+1):(h.pad.leng+nrow(reference.img)),
                             (w.pad.leng+1):(w.pad.leng+ncol(reference.img))]

  # Now fill any NAs with users choice. If there are no NAs, then overlap between the
  # reference and the template were perfect. In that case, we skip the filling step.
  #
  # Fill types:
  #"NA", "opposite", "random", "devils.advocate", "0", "1", "255"

  tmpl.in.ref.filled  <- tmpl.in.ref
  tmpl.in.ref.na.idxs <- which(is.na(tmpl.in.ref) == T, arr.ind = T)
  # print(tmpl.in.ref.na.idxs)           # **** Sometimes there are no NAs???? ISSUE HERE
  # print(dim(tmpl.in.ref.na.idxs))
  # print(length(tmpl.in.ref.na.idxs))
  # print(nrow(tmpl.in.ref.na.idxs))

  # Check to see if there are NAs added:
  if(nrow(tmpl.in.ref.na.idxs) != 0) {

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

  }

  return(tmpl.in.ref.filled)

}

