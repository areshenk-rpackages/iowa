#' Learning functions
#'
#' Implements several learning functions commonly used in models of the IGT
#'
#' @param u A scalar. The observed utility.
#' @param V A vector of current deck valuations.
#' @param choice An integer indicating the current deck choice.
#' @param par A named list of parameters. See details.
#' @param FUN The learning function to be used. See details.
#' @details All learning functions require a single learning rate parameter r.
#' Allowable learning functions are
#' \itemize{
#'  \item{"PV": }{The learning function used in the PVL model.}
#'  \item{"EV": }{The learning function used in the EV model.}
#'  \item{"Delta": }{The delta learning rule.}
#' }
#' @export

igtLearning <- function(u, V, choice, par, FUN = NULL) {
    if (is.null(FUN))
        stop('Must specify learning function')
    if (!FUN %in% c('PV', 'EV')){
        stop('Unrecognized learning function')
    }

    d <- switch(FUN,
                PV = learning_pv(u, V, choice, par),
                EV = learning_ev(u, V, choice, par),
                Delta = learning_delta(u, V, choice, par))
    return(d)
}
