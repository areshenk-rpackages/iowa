#' Utility functions
#'
#' Implements several utility functions commonly used in models of the IGT
#'
#' @param x,g,l For utility functions which depend only on the net outcome, x is a scale/vector
#' of net outcomes. For utilities which depend on both the observed gain and loss (e.g. the EV utility),
#' g and l are scalars/vectors indicating the gain and loss.
#' @param par A named list of parameters. See details.
#' @param FUN The utility function to be used. See details.
#' @details Allowable utility functions are
#' \itemize{
#'  \item{"PV": }{The prospect utility function. Depends only on the net outcome x,
#'  and requires a shape parameter A and a loss aversion parameter l}
#'  \item{"EV": }{The expectancy valence utility function. Depends on both the observed gain and loss (g,l),
#'  and requires a loss aversion parameter w.}
#' }
#' @export

igtUtility <- function(x = NULL, g = NULL, l = NULL, par, FUN = NULL) {
    if (is.null(FUN))
        stop('Must specify utility function')
    if (!FUN %in% c('PV', 'EV')){
        stop('Unrecognized utility')
    }

    d <- switch(FUN,
                PV = utilityfun_pv(x, par),
                EV = utilityfun_ev(g, l, par))
    return(d)
}
