#' Fit model to IGT data
#'
#' Fit model to IGT data either by MLE/MAP, or full posterior sampling
#'
#' @param win Vector of wins. If only net outcomes are observed, they should be provided here.
#' @param loss Vector of losses. May be ommited if only net outcomes are observed.
#' @param choice Integer vector of deck choices
#' @param numDecks Integer. Number of decks
#' @param utility Name of utility function
#' @param updating Name of updating function
#' @param temperature Name of temperature function
#' @param pars A named list of parameters. See details.
#' @param scale Scaling factor for observed outcomes. Typically, outcomes are divided
#' by 100, corresponding to scale = .01
#' @param reg Positive regularization parameter. A Beta(reg,reg) prior is placed
#' over the range of each parameter, with larger values of reg giving a tighter
#' concentration around the midpoint of the interval. Default value of 1 gives
#' a uniform prior.
#' @param method Either "optimization" for MAP (MLE when reg = 1), or "sampling"
#' for full posterior sampling via Stan's No-U-Turn-Sampler (NUTS).
#' @param returnStanfit Whether to return the full stanfit object.
#' @param ... Optional arguments provided to rstan::sampling()
#' @details Argument "pars" is a named list with fields:
#' \itemize{
#'  \item{"utility": }{A named list of parameter values. Names must correspond to
#'  the parameters of the utility function specified by argument "utility".
#'  Each parameter must be a vector of length 2, giving lower and upper bounds.}
#'  \item{"updating": }{See above.}
#'  \item{"temperature": }{See above.}
#' }
#' Note that any or all parameters may be omitted, in which case default bounds
#' will be selected based on conventions in the literature. If any of "utility",
#' "updating", or "temperature" are missing, default bounds will be selected for
#' all parameters.
#'
#' The user may pass optional arguments to rstan::sampling(). In the absence of
#' such arguments, the function uses the defaults, which at the time of writing
#' specify four chains of 2000 samples for full posterior sampling.
#'
#' @return Nothing. Generates an error message in the event of incorrect input.
#' @importFrom rstan sampling optimizing extract
#' @export

fitIGT <- function(choice, win, loss = NULL, numDecks,
                   utility, updating, temperature,
                   pars, scale = .01, reg = 1, method = 'optimizing',
                   returnStanfit = F, ...) {

    # Create Stan data object
    stanData <- createStanDataForFitting(win, loss, choice, numDecks, utility, updating,
                                         temperature, pars, reg, scale)

    # Call Stan
    if (method == 'optimizing') {
        fit <- optimizing(stanmodels$fit_igt, data = stanData, ...)
    } else if (method == 'sampling') {
        fit <- sampling(stanmodels$fit_igt, data = stanData, ...)
    } else {
        stop('Unrecognized method')
    }

    return(fit)
}
