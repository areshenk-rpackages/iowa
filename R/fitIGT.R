#' Fit model to IGT data
#'
#' Fit model to IGT data either by MLE/MAP, or full posterior sampling
#'
#' @param wins Vector of wins. If only net outcomes are observed, they should be provided here.
#' @param losses Vector of losses. May be omitted if only net outcomes are observed.
#' @param choices Integer vector of deck choices
#' @param numDecks Integer. Number of decks
#' @param utility Name of utility function
#' @param updating Name of updating function
#' @param temperature Name of temperature function
#' @param pars A named list of parameters. See details.
#' @param scale Scaling factor for observed outcomes. Typically, outcomes are
#' divided by 100, corresponding to scale = .01
#' @param reg Positive regularization parameter. A Beta(reg,reg) prior is placed
#' over the range of each parameter, with larger values of reg giving a tighter
#' concentration around the midpoint of the interval. Default value of 1 gives
#' a uniform prior.
#' @param method Either "optimize" for MAP (MLE when reg = 1), or "sample"
#' for full posterior sampling via Stan's No-U-Turn-Sampler (NUTS).
#' @param returnStanobj Whether to return the full cmdstan object.
#' @param ... Optional arguments provided to cmdstan::optimize() or cmdstan::sample()
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
#' The user may pass optional arguments to rstan::sampling() or rstan::optimizing().
#' In the absence of such arguments, the function uses the defaults.
#'
#' @return A list containing either estimated, or full posterior samples for all parameters.
#' @importFrom posterior as_draws_df
#' @export

fitIGT <- function(choices, wins, losses = NULL, numDecks,
                   utility, updating, temperature,
                   pars = NULL, scale = .01, reg = 1, method = 'optimize',
                   returnStanobj = F, ...) {

    pars <- verifyFittingBounds(utility, updating, temperature, pars)

    # Create Stan data object
    stanData <- createStanDataForFitting(wins, losses, choices, numDecks, utility, updating,
                                         temperature, pars, reg, scale)
    parLabs <- c(paste0('Utility_', names(modelDetails$utility[[utility]]$pars)),
                 paste0('Updating_', names(modelDetails$updating[[updating]]$pars)),
                 paste0('Temperature_', names(modelDetails$temperature[[temperature]]$pars)))

    # Call Stan
    model <- stan_package_model(name = "fit_igt", package = "iowa")

    if (method == 'optimize') {

        fit <- model$optimize(data = stanData, ...)
        est <- fit$mle()
        parIdx <- which(!grepl('raw_', names(est)))
        out <- fit$o
        p <- est[parIdx]
        names(p) <- parLabs
        ret <- list('Parameters' = p, 'logPosterior' = fit$lp(), 'return_code' = fit$return_codes())
        if (returnStanobj)
            ret$stanobj <- fit

    } else if (method == 'sample') {
        fit <- model$sample(data = stanData, ...)

        # Posterior dataframe
        s <- fit$draws()
        s <- as_draws_df(s)
        parIdx <- which(!grepl('raw_', names(s)) & grepl('params', names(s)))
        p <- s[,parIdx]
        names(p) <- parLabs

        # Diagnostics
        s <- fit$summary()
        parIdx <- which(!grepl('raw_', s$variable) & grepl('params', s$variable))
        s <- s[parIdx,]
        s$variable <- parLabs
        ret <- list('Samples' = p, 'Summary' = s)
        if (returnStanobj)
            ret$stanobj <- fit
    } else {
        stop('Unrecognized method')
    }

    return(ret)
}
