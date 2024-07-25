#' Create list of data for Stan simulation
#'
#' @param decks Names list with entries "win" and "loss"
#' @param utility Name of utility function
#' @param updating Name of updating function
#' @param temperature Name of temperature function
#' @param pars A named list of parameters. See details.
#' @details Argument "pars" is a named list with fields:
#' \itemize{
#'  \item{"utility": }{A named list of parameter values. Names must correspond to
#'  the parameters of the utility function specified by argument "utility".}
#'  \item{"updating": }{See above.}
#'  \item{"temperature": }{See above.}
#' }
#' @return A list of data for stan

createStanDataForFitting <- function(decks, utility, updating,
                                     temperature, pars = NULL, scale){

    if (any(dim(deck$win) != dim(deck$loss)))
        stop('Deck win and loss must have the same dimensions.')

    # Get IGT parameters
    numTrials <- nrow(deck$win)
    numDecks  <- ncol(deck$win)

    # Setup stan data
    stanData <- list(NUM_TRIALS = numTrials, NUM_DECKS = numDecks,
                      UTILITY_FUNCTION     = which(utility == names(modelComponentDetails$utility)),
                      UPDATING_FUNCTION    = which(updating == names(modelComponentDetails$updating)),
                      TEMPERATURE_FUNCTION = which(temperature == names(modelComponentDetails$temperature)),
                      NUM_UTILITY_PARAMETERS     = length(pars$utility),
                      NUM_UPDATING_PARAMETERS    = length(pars$updating),
                      NUM_TEMPERATURE_PARAMETERS = length(pars$temperature),
                      win  = scale * deck$win,
                      loss = scale * deck$loss)

    return(stanData)
}
