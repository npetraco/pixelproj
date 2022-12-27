#' Overlay template on reference and systematically shift over it
#'
#' Overlay template on reference and systematically shift over it, plotting the result
#'
#' The function depends on the Rvision library which must be installed
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
plot.overlay <- function(template.img, reference.img, h.min.overlap=1, w.min.overlap=1, x.max, y.max, ref.lo=0, ref.hi=2, templ.lo=4, templ.hi=6, delay=0.1) {

  # Make a padded ref wrt the tmp which will be shifting around over the ref:
  w <- ncol(template.img)
  h <- nrow(template.img)

  # Padding lengths
  h.pad.leng <- h - h.min.overlap
  w.pad.leng <- w - w.min.overlap

  # Pad the reference so that the template can slide over it
  pad.rows <- nrow(reference.img) + 2 * h.pad.leng
  pad.cols <- ncol(reference.img) + 2 * w.pad.leng

  # Extract and re-scale the reference's pixels
  ref.plot <- reference.img[,,1]
  ref.plot[which(ref.plot==0, arr.ind = T)]   <- ref.lo
  ref.plot[which(ref.plot==255, arr.ind = T)] <- ref.hi

  # Extract and re-scale the  template's pixels
  tmpl.plot                                     <- template.img[,,1]
  tmpl.plot[which(tmpl.plot==0, arr.ind = T)]   <- templ.lo
  tmpl.plot[which(tmpl.plot==255, arr.ind = T)] <- templ.hi

  key.info <- cbind(c("blue","red","green","orange"), c("0   reference", "255 reference", "0   template", "255 template"))
  colnames(key.info) <- c("color", "pixel")
  print(key.info)


  # LOOP HERE
  for(x in 1:(w.pad.leng+ncol(ref.plot))) {
    for(y in 1:(h.pad.leng+nrow(ref.plot))) {
      # This will be the plot canvas: an array the same size as the padded reference
      # Any NAs should be from padding here on out and will appear in white
      tmpl.in.ref.plot <- array(NA, c(pad.rows, pad.cols))

      # Embed re-scaled ref in the tmpl.in.ref:
      tmpl.in.ref.plot[(h.pad.leng+1):(h.pad.leng+nrow(ref.plot)),
                       (w.pad.leng+1):(w.pad.leng+ncol(ref.plot))] <- ref.plot

      # Overlay re-scaled template onto padded rescaled reference and see what we get:
      print(paste("pad=(", pad.cols, pad.rows, ") ","x=", x, " y=",y),sep="")
      #print(paste("x=", x, " y=",y, " x == x.max?", x == x.max, " y == y.max?", y == y.max,
      #            " (x == x.max) & (y == y.max)=", (x == x.max) & (y == y.max)),sep="")

      tmpl.in.ref.plot[y:(y+h-1), x:(x+w-1)] <- tmpl.plot[,]

      # Plot:
      #plot(tmpl.in.ref.plot, col=c("blue","red","green","orange"), breaks = c(0,1,3,5,7), key=NULL)
      plot(tmpl.in.ref.plot[nrow(tmpl.in.ref.plot):1,], col=c("blue","red","green","orange"), breaks = c(0,1,3,5,7), key=NULL)

      if((x == x.max) & (y == y.max)){
        break()
      }

      Sys.sleep(delay)
    }

    if((x == x.max) & (y == y.max)){
      break()
    }

  }

}


#' Plot an image stack
#'
#' Plot an image stack by plotting one image after the next
#'
#' The function depends on the Rvision library which must be installed
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
plot.stack <- function(img.list, num.cycles=1, type="Rvision", main.title = NULL, delay=0.5) {

  num.imgs <- length(img.list)
  for(k in 1:num.cycles) {

    print(paste0("Cycle: ", k))

    for(i in 1:num.imgs){

      if(type == "Rvision") {

        if(is.null(main.title)){
          print(paste0("    Image: ",i))
        } else {
          print(main.title)
        }
        plot(img.list[[i]])

      } else if(type == "matrix") {

        if(is.null(main.title)){
          mtitl.loc <- paste0("    Image: ",i)
        } else {
          mtitl.loc <- main.title
        }
        plot(img.list[[i]][,,1], key=NULL, main=mtitl.loc)

      } else if(type == "image.matrix") {

        if(is.null(main.title)){
          mtitl.loc <- paste0("    Image: ",i)
        } else {
          mtitl.loc <- main.title
        }
        nri <- nrow(img.list[[i]][,,1])
        plot(img.list[[i]][nri:1,,1], key=NULL, main=mtitl.loc)

      } else {
        stop("Choose type = Rvision, matrix, image.matrix")
      }

      Sys.sleep(delay)

    }

  }

}

