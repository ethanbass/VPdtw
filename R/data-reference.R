#' GC-MS Chromatogram
#' 
#' GC-MS chromatogram from a wine sample
#' 
#' This together with the \code{query} data are used in the VPdtw examples. The
#' alignment of these two signals is usually carried out on the log scale.
#' Plotting of this signal is best done also on the log scale (see example
#' below).
#' 
#' @name reference
#' @aliases reference query
#' @docType data
#' @format A numeric vector of length 10018
#' @references Amalia Z. Berna, Stephen Trowell, David Clifford, Wies Cynkar,
#' Daniel Cozzolino, Geographical origin of Sauvignon Blanc wines predicted by
#' mass spectrometry and metal oxide based electronic nose, Analytica Chimica
#' Acta, Volume 648, Issue 2, 26 August 2009, Pages 146-152, ISSN 0003-2670,
#' DOI: 10.1016/j.aca.2009.06.056.  Keywords: Sauvignon Blanc; Electronic nose;
#' Gas chromatography-mass spectrometry; Prediction
#' @source Amalia Berna, Stephen Trowell, CSIRO Food Futures Flagship
#' @keywords datasets
#' @examples
#' 
#' 
#' data(reference)
#' data(query)
#' plot(reference, log="y", type="l", main = "Gas Chromatogram", 
#'   ylab = "log(intensity)", lwd = 2, col = 1)
#' lines(query, col = 2)
#' 
#' 
NULL
