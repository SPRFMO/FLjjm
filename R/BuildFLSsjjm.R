#' Build Multiple FLStock Objects from a jjm.output Object
#'
#' This function creates a collection of FLStock objects from a jjm.output object. 
#' It is particularly useful for multi-stock assessments.
#'
#' @param out A jjm.output object, typically the output of a stock assessment model.
#'
#' @return An FLStocks object containing multiple FLStock objects.
#'
#' @seealso \code{\link[FLCore]{FLStock}}, \code{\link[FLCore]{FLStocks}}
#'
#' @export
buildFLSsjjm <- function(out) {
  
  # Validate input
  if (!inherits(out, "jjm.output")) {
    stop("The 'out' parameter should be a 'jjm.output' object.")
  }

  # Number of stocks
  nstks <- out[[1]]$info$output$nStock
  
  if (nstks <= 0) {
    stop("The 'out' object does not contain any stock information.")
  }

  # Build FLStock objects for each stock
  stks <- FLStocks(lapply(seq_len(nstks), function(idx) {
    buildFLSjjm(out, stock=idx)
  }))
  
  # Assigning names (Optional)
  # If specific names are known for the stocks, they can be assigned here.
  # Example: names(stks) <- c("Stock1", "Stock2", ...)

  return(stks)
}


# buildFLSsjjm : FLStocks {{{
# buildFLSsjjm <- function(out) {
#   nstks <- out[[1]]$info$output$nStock
#   stks <- FLStocks(lapply(seq(nstks), buildFLSjjm, out=out))
#   names(stks) <- c("Southern", "North")
#   return(stks)
# }
# }}}

