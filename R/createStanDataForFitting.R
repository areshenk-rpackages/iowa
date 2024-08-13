#' Create list of data for Stan fitting
#'
#' @param wins Vector or wins
#' @param losses Vector of losses
#' @param choices Integer vector of deck choices
#' @param numDecks Integer. Number of decks
#' @param utility Name of utility function
#' @param updating Name of updating function
#' @param temperature Name of temperature function
#' @param pars A named list of parameters. See details.
#' @param scale Scaling factor for observed outcomes
#' @param reg Positive regularization parameter. A Beta(reg,reg) prior is placed
#' over the range of each parameter, with larger values of reg giving a tighter
#' concentration around the midpoint of the interval. Default value of 1 gives
#' a uniform prior.
#' @details Argument "pars" is a named list with fields:
#' \itemize{
#'  \item{"utility": }{A named list of parameter values. Names must correspond to
#'  the parameters of the utility function specified by argument "utility".
#'  Each parameter must be a vector of length 2, giving lower and upper bounds.}
#'  \item{"updating": }{See above.}
#'  \item{"temperature": }{See above.}
#' }
#' @return A list of data for stan

createStanDataForFitting <- function(wins, losses = NULL, choices, numDecks, utility, updating,
                                     temperature, pars = NULL, reg = 1, scale = .01){

    if (reg < 0)
        stop('Prior regularization must be positive')
    else if (reg < 1)
        warning("Prior regularization can technically be less than 1, but you probably don't want this.")

    if (is.null(losses))
        losses <- 0 * wins

    if (length(unique((c(length(wins), length(losses), length(choices))))) != 1)
        stop('wins, losses, and choices must be vectors of identical length')

    # Setup stan data
    stanData <- list(NUM_TRIALS = length(wins),
                     NUM_DECKS = numDecks,

                     UTILITY_FUNCTION     = modelDetails$utility[[utility]]$index,
                     UPDATING_FUNCTION    = modelDetails$updating[[updating]]$index,
                     TEMPERATURE_FUNCTION = modelDetails$temperature[[temperature]]$index,

                     NUM_UTILITY_PARAMETERS     = length(pars$utility),
                     NUM_UPDATING_PARAMETERS    = length(pars$updating),
                     NUM_TEMPERATURE_PARAMETERS = length(pars$temperature),

                     UTILITY_LOWER_BOUND = as.array(sapply(pars$utility, function(i) i[1])),
                     UTILITY_UPPER_BOUND = as.array(sapply(pars$utility, function(i) i[2])),

                     UPDATING_LOWER_BOUND = as.array(sapply(pars$updating, function(i) i[1])),
                     UPDATING_UPPER_BOUND = as.array(sapply(pars$updating, function(i) i[2])),

                     TEMPERATURE_LOWER_BOUND = as.array(sapply(pars$temperature, function(i) i[1])),
                     TEMPERATURE_UPPER_BOUND = as.array(sapply(pars$temperature, function(i) i[2])),


                     wins  = scale * wins,
                     losses = scale * losses,
                     choices = choices,
                     reg = reg)

    return(stanData)
}
