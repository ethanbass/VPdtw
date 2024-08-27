#' Variable Penalty Dynamic Time Warping function
#' 
#' Use variable penalty dynamic time warping to align one (or many) query
#' signal(s) to a master signal. Penalties are incurred whenever a non-diagonal
#' move is taken.
#' 
#' Performs variable penalty dynamic time warping of \code{query} to
#' \code{reference}. Sakoe Chiba dtw used with width \code{maxshift}.
#' 
#' The basic operation aligns a \code{query} vector to a \code{reference}
#' vector.
#' 
#' If \code{reference} is not specified and \code{query} is a matrix then the
#' \code{reference} is created based on the value of \code{Reference.type}. The
#' four choices are \code{random}, \code{median}, \code{mean} and
#' \code{trimmed}. These choose a column of \code{query} at random as a
#' reference, or the piecewise median, mean or trimmed mean (with 
#' \code{trim = 0.1}) with missing values removed.
#' 
#' If \code{query} is a matrix and \code{penalty} is a vector then the same
#' penalty is used to align each column of \code{query} to the
#' \code{reference}. Different alignment paths are chosen for each column of
#' the \code{query} matrix.
#' 
#' If \code{query} is a vector and \code{penalty} is a matrix then the
#' \code{query} is aligned to the \code{reference} several times, using each
#' column of the \code{penalty} matrix in turn as the penalty for the
#' alignment.
#' 
#' If \code{query} and \code{penalty} are matrices then nothing happens. If you
#' wish to align many query vectors and test many penalty vectors at the same
#' time then do the appropriate looping (over queries, or penalties) outside of
#' \code{VPdtw}.
#' 
#' @importFrom stats median na.omit
#' @param reference Reference signal, NULL or a vector, see details.
#' @param query Query signal, a vector or matrix, see details
#' @param penalty Penalty term, a vector of same length as reference (not
#' checked) or a matrix. See details. Default is 0 repeated to the length of
#' reference
#' @param maxshift Maximum allowable shift, an integer
#' @param Reference.type Choices for \code{reference} if NULL is input
#' @return \item{xVals}{For plotting everything to correct index}
#' \item{reference}{reference vector used by VPdtw expanded by NAs for
#' plotting.}
#' \item{query}{query passed to VPdtw}
#' \item{penalty}{penalty passed to VPdtw}
#' \item{warpedQuery}{result of alignment, same class as query}
#' \item{shift}{shifts required to achieve alignment}
#' \item{summary}{Summary information about the alignment. Used for 
#' \code{print.VPdtw}}
#' \item{information}{Information about the alignment. Used for 
#' \code{print.VPdtw}}
#' @useDynLib VPdtw
#' @author David Clifford, Glenn Stone
#' @seealso Also check out the \code{dtw} package by Toni Giorgino which
#' covers many variations of dynamic time warping.
#' @references Alignment Using Variable Penalty Dynamic Time Warping by
#' Clifford, D; Stone, G; Montoliu, I; et al. Analytical Chemistry Volume: 81
#' Issue: 3 Pages: 1000-1007 Published: 2009
#' @keywords methods
#' @examples
#' 
#'   ## Citation
#'   citation("VPdtw")
#' 
#'   ## Basic Examples of zero-penalty DTW
#' 
#'   ## Example of exact fit in the middle
#'   query <- c(1,5,4,3,9,8,5,2,6,5,4)
#'   reference <- c(rnorm(5), query, rnorm(5))
#'   lambda <- rep(0, length(reference))
#'   maxshift <- 11
#'   res <- VPdtw(reference, query, lambda, maxshift)
#'   plot(res)
#'   res
#' 
#'   ## Example of exact fit on one side
#'   reference <- c(1,5,4,3,9,8,5,2,6,5,4)
#'   query <- c(rnorm(5), reference)
#'   reference <- c(reference, rnorm(5))
#'   lambda <- rep(0, length(reference))
#'   maxshift <- 6
#'   res <- VPdtw(reference, query, lambda, maxshift)
#'   plot(res)
#'   res
#' 
#'   ## Example of exact fit on the other side
#'   reference <- c(1,5,4,3,9,8,5,2,6,5,4)
#'   query <- c(reference, rnorm(5))
#'   reference <- c(rnorm(5), reference)
#'   lambda <- rep(0, length(reference))
#'   maxshift <- 6
#'   res <- VPdtw(reference, query, lambda, maxshift)
#'   plot(res)
#'   res
#' 
#'   ## Example of exact fit except where one query gets dropped and its all on one side
#'   reference <- c(1,5,4,3,9,8,5,2,6,5,4)
#'   query <- c(reference[1:5], 20, reference[6:11])
#'   reference <- c(rnorm(5), reference)
#'   query <- c(query, rnorm(5))
#'   lambda <- rep(0, length(reference))
#'   maxshift <- 6
#'   res <- VPdtw(reference, query, lambda, maxshift)
#'   plot(res)
#'   res
#'   
#' ## Examples that use penalty term. Examples with long signals
#' 
#' data(reference)
#' data(query)
#' ## Do alignment on log scale
#' reference <- log(reference)
#' query <- log(query)
#' 
#' ## VPdtw
#' result <- VPdtw(reference=reference[1:2500], query = query[1:2500],
#'                 penalty = dilation(reference[1:2500], 150)/4, maxshift=150)
#' plot(result)
#' result
#' 
#' ## Zero penalty DTW
#' result2 <- VPdtw(reference = reference[1:2500], query = query[1:2500],
#'                  penalty = rep(0, length(reference)), maxshift = 150)
#' plot(result2)
#' 
#' ## Try both penalties at the same time
#' penalty <- dilation(reference, 350)/5
#' penalty <- cbind(penalty, rep(0, length(penalty)))
#' 
#' result <- VPdtw(reference, query, penalty = penalty, maxshift = 350)
#' plot(result, "After")
#' plot(result, "Shift")
#' result
#' 
#' ## All three plots at once
#' plot(result)
#' 
#' 
#' @export VPdtw

VPdtw <- function(reference, query, penalty = 0, maxshift = 50,
                  Reference.type = c("random", "median", "mean", "trimmed")) {

  ## We assume Sakoe Chiba DTW to allow for faster computation times
  if(!is.numeric(maxshift))
    stop("Please specify maxshift as an integer value")

  ## Figure out what kind of alignment we are doing -

  ## a reference vector to a query vector?
  ## no reference and a matrix of query vectors?
  ## something else = no implementation here
  
  if(is.null(reference) & !is.matrix(query))
    stop("Please specify a reference when passing a non-matrix query")
  
  if(is.null(reference) & is.matrix(query)) {

    ## If no reference is specified, we choose one randomly, using
    ## median, mean or trimmed mean of the query matrix depending on
    ## the value of type as specified by the user.
    
    type <- match.arg(Reference.type, c("random", "median", "mean", "trimmed"))
    ss <- sample(seq_len(ncol(query)), 1)
    reference <- switch(type,
                        random = query[,ss],
                        median = apply(query, 1, median, na.rm = TRUE),
                        mean = apply(query, 1, mean, na.rm = TRUE),
                        trimmed = apply(query, 1, mean, na.rm = TRUE, trim = 0.1))
    reference <- na.omit(reference)

    ## if penalty is a number, then this means a constant penalty vector, create that vector
    if(length(penalty) ==1 )
      penalty <- rep(penalty, length(reference))

    ## Check penalty vector length
    if(length(penalty) < length(reference)) {
      stop("Penalty vector should be at least of length ", 
           length(reference), " but it has length ", length(penalty))
    }

    ## information used in summary at end - what kind of reference do
    ## we have - in this block of code it IS null, include info on
    ## what kind was generated.
    
    information <- paste("Reference is NULL.")
    information <- paste(information, switch(type,
                                            random = paste("Query column #", ss,
                                                           "is chosen at random."),
                                            median = "Query median used.",
                                            mean = "Query mean used.",
                                            trimmed = "Query trimmed mean used."))
    information <- paste(information, "\n")
  } else {
    information <- paste("Reference supplied by user.\n")
  }

  ## This function DoAlignment is a wrapped that takes a vector query,
  ## vector reference and vector penalty and (eventually) does a Sakoe
  ## Chiba DTW with width maxshift. This function calls
  ## signalMatchABand which calls the C++ code itself. This can be
  ## "applied" to matrices of queries. Another version DoAlignmentP is
  ## for apply to matrix of penalties.
  
  DoAlignment <- function(query, reference, penalty, maxshift) {
    ## Drop NAs
    reference <- na.omit(reference)
    query <- na.omit(query)
    result <- signalMatchABand(reference = reference,
                               query = query,
                               lambda = penalty,
                               maxshift = maxshift)
    result ## matrix four columns 
  }

  ## Ok we are ready to do our alignment.

  ## Scenario 1: all vectors:
  if(is.vector(reference) & is.vector(query) & is.vector(penalty)) {
    information <- "Reference is supplied by the user.\n"
    information <- paste(information, "Query vector is of length ",
                         length(query), ".\n", sep = "")

    information <- paste(information,
                         "Single Penalty vector supplied by user.\n", sep = "")
    information <- paste(information, "Max allowed shift is ",
                         maxshift, ".\n", sep = "")
    reference <- na.omit(reference)

    if(length(penalty) == 1)
      penalty <- rep(penalty, length(reference))
    
    result <- DoAlignment(query, reference, penalty, maxshift)

    ## format output a little better:
    
    output <- vector("list", 6)
    names(output) <- c("xVals", "reference", "query",
                       "penalty", "warpedQuery", "shift")
    
    output$xVals <- result[, "xVals"]
    
    output$reference <- result[, "reference"]

    output$query <- query
    output$penalty <- penalty

    output$warpedQuery <- result[, "warped query"]

    output$shift <- result[, "shift"]
    
    class(output) <- "VPdtw"

    ## Summary Statistics: 

    cost1 <- function(x) {
      ret <- c(sum(abs(x$warpedQuery - x$reference), na.rm = TRUE) +
               sum(x$penalty[x$xVals[which(diff(x$shift) == 1) + 1]],
                   na.rm = TRUE) +
               2*sum(x$penalty[x$xVals[which(diff(x$shift) == -1) + 1]],
                     na.rm = TRUE),
               sum(!is.na(x$warpedQuery * x$reference), na.rm = TRUE),
               max(abs(x$shift), na.rm = TRUE),
               sum(diff(x$shift) == 0, na.rm = TRUE) + 1,
               sum(diff(x$shift) == 1, na.rm = TRUE),
               sum(diff(x$shift) == -1, na.rm = TRUE))
      names(ret) <- c("Cost", "Overlap", "Max Obs. Shift",
                      "# Diag Moves", "# Expanded", "# Dropped")
      ret
    }
    output$summary <- cost1(output)
              
    output$information <- information
    
    return(invisible(output))
  }

  ## Scenario 2: Matrix of queries, penalty vector
  
  if(is.matrix(query) & is.vector(penalty)) {
    information <- paste(information, "Query matrix is made up of ",
                         ncol(query), " samples of length ", nrow(query), 
                         ".\n", sep = "")
    information <- paste(information, "Single Penalty vector supplied by user.\n", 
                         sep = "")
    information <- paste(information, "Max allowed shift is ", maxshift, ".\n", 
                         sep = "")
    reference <- na.omit(reference)
    ## Align all of query vectors to reference using the same penalty
    queryL <- as.list(as.data.frame(query)) ## to ensure I get a list in the next round, can't apply to matrix
    result <- lapply(queryL, DoAlignment, reference = reference,
                     penalty = penalty, maxshift = maxshift) ## produces a list

    ## result is now a list of length ncol(penalty) each part is a matrix

    xlim <- NULL
    for(ii in seq_len(length(result)))
      xlim <- c(xlim, range(result[[ii]][,1]))
    xlim <- range(xlim)

    xVals <- seq(xlim[1], xlim[2], by = 1)

    output <- vector("list", 6)
    names(output) <- c("xVals", "reference", "query",
                       "penalty", "warpedQuery", "shift")
    
    output$xVals <- xVals
    
    str <- which(xVals == 1)
    end <- which(xVals == length(reference))
    output$reference <- rep(NA, length(xVals))
    output$reference[seq(str, end, by = 1)] <- reference

    output$query <- query
    output$penalty <- penalty
    
    output$warpedQuery <- matrix(NA, length(xVals), ncol(query))
    colnames(output$warpedQuery) <- paste("warped query", seq_len(ncol(query)))

    output$shift <- matrix(NA, length(xVals), ncol(query))
    colnames(output$shift) <- paste("shift", seq_len(ncol(query)))
    
    for(ii in seq_len(ncol(query))) {
      colName <- paste("warped query", ii)
      str <- which(xVals == result[[ii]][1,1])
      end <- which(xVals == result[[ii]][nrow(result[[ii]]), 1])
      output$warpedQuery[seq(str, end, by = 1), colName] <- result[[ii]][,3]
      colName <- paste("shift",ii)
      output$shift[seq(str, end, by = 1), colName] <- result[[ii]][,4]
    }

    class(output) <- "VPdtw"

    ## Summary Statistics for each query separately
    
    cost2 <- function(x,ii) {
      ret <- c(sum(abs(x$warpedQuery[,ii] - x$reference),
                   na.rm = TRUE) +
               sum(x$penalty[x$xVals[which(diff(x$shift[,ii]) == 1) + 1]],
                   na.rm = TRUE) +
               2*sum(x$penalty[x$xVals[which(diff(x$shift[,ii]) == -1) + 1]],
                     na.rm = TRUE),
               sum(!is.na(x$warpedQuery[,ii] * x$reference), na.rm = TRUE),
               max(abs(x$shift[,ii]), na.rm = TRUE),
               sum(diff(x$shift[,ii]) == 0, na.rm = TRUE) + 1,
               sum(diff(x$shift[,ii]) == 1, na.rm = TRUE),
                 sum(diff(x$shift[,ii]) == -1, na.rm = TRUE))
      names(ret) <- c("Cost", "Overlap", "Max Obs Shift", "# Diag Moves",
                      "# Expanded", "# Dropped")
      ret
    }
    
    output$summary <- NULL
    for(ii in seq_len(ncol(output$warpedQuery))){
      output$summary <- rbind(output$summary, cost2(output,ii))
    }
    rownames(output$summary) <- paste("Query #",
                                      seq_len(ncol(output$warpedQuery)),
                                      ":", sep = "")
    output$information <- information
        
    return(invisible(output))
  }

  ## For doing alignment for many different penalties
  DoAlignmentP <- function(penalty, query, reference, maxshift) {
    ## Drop NAs
    reference <- na.omit(reference)
    query <- na.omit(query)
    result <- signalMatchABand(reference = reference,
                               query = query,
                               lambda = penalty,
                               maxshift = maxshift)
    result
  }

  ## Scenario 3: vector query and penalty matrix
  if(is.vector(query) & is.matrix(penalty)){

    information <- paste(information, "Query vector of length ",
                         length(query), ".\n", sep = "")
    
    information <- paste(information, "Penalty matrix made up of ",
                         ncol(penalty), " penalties supplied by user.\n",
                         sep = "")

    information <- paste(information, "Max allowed shift is ", maxshift, 
                         ".\n", sep = "")
    reference <- na.omit(reference)
    ## Align query to reference using each of the penalties separately
    penaltyL <- as.list(as.data.frame(penalty)) ## to ensure I get a list in the next round, can't apply to matrix
    result <- lapply(penaltyL, DoAlignmentP, reference = reference,
                     query = query, maxshift = maxshift) ## produces a list
    ## result is now a list of length ncol(penalty) each part is a matrix

    xlim <- NULL
    for(ii in seq_len(length(result)))
      xlim <- c(xlim, range(result[[ii]][,1]))
    xlim <- range(xlim)

    xVals <- seq(xlim[1], xlim[2], by=1)

    output <- vector("list", 6)
    names(output) <- c("xVals", "reference", "query",
                       "penalty", "warpedQuery", "shift")
    
    output$xVals <- xVals
    
    str <- which(xVals == 1)
    end <- which(xVals == length(reference))
    output$reference <- rep(NA, length(xVals))
    output$reference[seq(str, end, by = 1)] <- reference

    output$query <- query
    output$penalty <- penalty
    
    output$warpedQuery <- matrix(NA, length(xVals), ncol(penalty))
    colnames(output$warpedQuery) <- paste("warped query penalty", seq_len(ncol(penalty)))

    output$shift <- matrix(NA, length(xVals), ncol(penalty))
    colnames(output$shift) <- paste("shift penalty", seq_len(ncol(penalty)))
    
    for(ii in seq_len(ncol(penalty))) {
      colName <- paste("warped query penalty", ii)
      str <- which(xVals == result[[ii]][1,1])
      end <- which(xVals == result[[ii]][nrow(result[[ii]]), 1])
      output$warpedQuery[seq(str, end, by=1), colName] <- result[[ii]][,3]
      colName <- paste("shift penalty", ii)
      output$shift[seq(str, end, by = 1), colName] <- result[[ii]][,4]
    }
    class(output) <- "VPdtw"

    ## Summary Statistics for each query separately
    
    cost3 <- function(x,ii) {
      ret <- c(sum(abs(x$warpedQuery[,ii] - x$reference), na.rm = TRUE) +
               sum(x$penalty[,ii][x$xVals[which(diff(x$shift[,ii]) == 1) + 1]],
                   na.rm = TRUE) +
               2*sum(x$penalty[,ii][x$xVals[which(diff(x$shift[,ii]) == -1) + 1]],
                     na.rm = TRUE),
               sum(!is.na(x$warpedQuery[,ii] * x$reference), na.rm = TRUE),
               max(abs(x$shift[,ii]), na.rm = TRUE),
               sum(diff(x$shift[,ii]) == 0, na.rm = TRUE) + 1,
               sum(diff(x$shift[,ii]) == 1, na.rm = TRUE),
               sum(diff(x$shift[,ii]) == -1, na.rm = TRUE))
      names(ret) <- c("Cost", "Overlap", "Max Obs Shift", 
                      "# Diag Moves", "# Expanded","# Dropped")
      ret
    }
    
    output$summary <- NULL
    for(ii in seq_len(ncol(output$warpedQuery))){
      output$summary <- rbind(output$summary, cost3(output, ii))
    } 
    rownames(output$summary) <- paste("Penalty #", 
                                      seq_len(ncol(output$warpedQuery)), ":", 
                                      sep = "")
    output$information <- information
    
    return(invisible(output))
  }

  if(is.matrix(query) & is.matrix(penalty)) {
    stop("Multiple queries and multiple penalties not yet implemented.
         Please create loops and call VPdtw as needed.")
  }

  ## finished
}

#' Print VPdtw
#' @param x A VPdtw object generated by \code{VPdtw}.
#' @param ... Additional argument.
#' @return Numeric vector from the \code{summary} slot in the \code{VPdtw}
#' object specified by \code{x}. 
#' @examples
#' query <- c(1,5,4,3,9,8,5,2,6,5,4)
#' reference <- c(rnorm(5), query, rnorm(5))
#' lambda <- rep(0, length(reference))
#' maxshift <- 11
#' res <- VPdtw(reference, query, lambda, maxshift)
#' print(res)
#' 
#' @export

print.VPdtw <- function(x,...) {
  cat(x$information)
  cat("\n")
  print(signif(x$summary, 5))
}
