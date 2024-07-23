#' Choice functions
#'
#' Implements several learning functions commonly used in models of the IGT
#'
#' @param V Vector of deck valuations.
#' @param par A named list of parameters. See details.
#' @param trial Current trial number. Only needed for FUN = vartemp.
#' @param FUN The choice function to be used. See details.
#' @return A vector of choice probabilities.
#' @details All choice functions require a single parameter c.
#' Allowable choice functions are
#' \itemize{
#'  \item{"fixedtemp": }{Temperature is equal to 3^c - 1.}
#'  \item{"vartemp": }{Temperature is equal to (trial/10)^c.}
#' }
#' @export

igtChoice <- function(V, par, trial = NULL, FUN = NULL) {
    if (is.null(FUN))
        stop('Must specify choice function')
    if (!FUN %in% c('fixedtemp', 'vartemp')){
        stop('Unrecognized choice function')
    }

    d <- switch(FUN,
                fixedtemp = choicefun_fixedtemp(V, par),
                vartemp = choicefun_vartemp(V, trial, par))
    return(d)
}
