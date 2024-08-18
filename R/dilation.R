#' Compute a morphological dilation of a signal
#' 
#' A dilation is a moving local maximum over a window of specific fixed width
#' specified by \code{span}. This dilation is computed first in one direction and then
#' in the other.
#' 
#' A dilation is a method often used in mathematical morphology and image
#' analysis (Soille 1999). This function is for vectors not matrices or images
#' though applying it to rows and columns of a matrix will give the
#' corresponding results.
#' 
#' An erosion of a vector or image can also be computed easily from this by
#' computing the dilation of -1 times the vector and transforming back.
#' 
#' We recommend using a dilation to form a \code{penalty} for use in
#' \code{VPdtw}.
#' 
#' @param y Signal (as a numeric vector).
#' @param span An integer specifying the width of the moving window.
#' @return \item{res}{Dilation of y with width specified by \code{span}}
#' @author David Clifford
#' @references Soille, P. Morphological Image Analysis: Principles and
#' Applications; Springer: New York, 1999.
#' @keywords methods
#' @examples
#' 
#' ## Example 1 - dilation of a signal
#' data(reference)
#' dref <- dilation(reference, 150)
#' plot(reference, log = "y", type = "l")
#' lines(dref, col = 2)
#' 
#' ## Example 2 - dilation of an image
#' BIN <- (volcano > 177)
#' dBIN <- t(apply(BIN, 1, dilation, span = 5))
#' dBIN <- apply(dBIN, 2, dilation, span = 5)
#' oldpar <- par(no.readonly = TRUE)
#' par(mfrow=c(2, 2))
#' image(volcano)
#' image(BIN)
#' image(dBIN)
#' par(oldpar)
#' 
#' @export dilation

dilation <- function(y, span){
  y <- na.omit(y)
  nmes <- names(y)
  res <- .C("dilation",
            y = as.double(y),
            nym = as.integer(length(y)),
            sp = as.integer(span),
            ey = double(length(y)),
            PACKAGE = "VPdtw")$ey
  res <- rev(.C("dilation",
                y = as.double(rev(res)),
                nym = as.integer(length(res)),
                sp = as.integer(span),
                ey = double(length(res)),
                PACKAGE = "VPdtw")$ey)
  names(res) <- nmes
  res
}

