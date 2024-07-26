#' Create list of data for Stan fitting
#'
#' @param win Vector or wins
#' @param loss Vector of losses
#' @param choice Integer vector of deck choices
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

createStanDataForFitting <- function(win, loss = NULL, choice, numDecks, utility, updating,
                                     temperature, pars = NULL, reg = 1, scale = .01){

    if (reg < 0)
        stop('Prior regularization must be positive')
    else if (reg < 1)
        warning("Prior regularization can technically be less than 1, but you probably don't want this.")

    if (is.null(loss))
        loss <- 0 * win

    if (length(unique((c(length(win), length(loss), length(choice))))) != 1)
        stop('win, loss, and choice must be vectors of identical length')

    pars <- verifyFittingBounds(pars)

    # Setup stan data
    stanData <- list(NUM_TRIALS = length(win),
                     NUM_DECKS = numDecks,

                     UTILITY_FUNCTION     = which(utility == names(modelDetails$utility)),
                     UPDATING_FUNCTION    = which(updating == names(modelDetails$updating)),
                     TEMPERATURE_FUNCTION = which(temperature == names(modelDetails$temperature)),

                     NUM_UTILITY_PARAMETERS     = length(pars$utility),
                     NUM_UPDATING_PARAMETERS    = length(pars$updating),
                     NUM_TEMPERATURE_PARAMETERS = length(pars$temperature),

                     UTILITY_LOWER_BOUND = sapply(pars$utility, function(i) i[1]),
                     UTILITY_UPPER_BOUND = sapply(pars$utility, function(i) i[2]),

                     UPDATING_LOWER_BOUND = sapply(pars$updating, function(i) i[1]),
                     UPDATING_UPPER_BOUND = sapply(pars$updating, function(i) i[2]),

                     TEMPERATURE_LOWER_BOUND = sapply(pars$temperature, function(i) i[1]),
                     TEMPERATURE_UPPER_BOUND = sapply(pars$temperature, function(i) i[2]),


                     win  = scale * deck$win,
                     loss = scale * deck$loss,
                     choice = choice,
                     reg = reg)

    return(stanData)
}
