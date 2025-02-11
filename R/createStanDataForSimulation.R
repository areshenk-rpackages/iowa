#' Create list of data for Stan simulation
#'
#' @param decks Names list with entries "wins" and "losses"
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

createStanDataForSimulation <- function(deck, utility, updating,
                                        temperature, pars, scale){

    if (any(dim(deck$wins) != dim(deck$losses)))
        stop('Deck wins and losses must have the same dimensions.')

    # Get IGT parameters
    numTrials <- nrow(deck$wins)
    numDecks  <- ncol(deck$wins)

    # Setup stan data
    stanData <- list(NUM_TRIALS = numTrials, NUM_DECKS = numDecks,
                     UTILITY_FUNCTION     = modelDetails$utility[[utility]]$index,
                     UPDATING_FUNCTION    = modelDetails$updating[[updating]]$index,
                     TEMPERATURE_FUNCTION = modelDetails$temperature[[temperature]]$index,
                     NUM_UTILITY_PARAMETERS     = length(modelDetails$utility[[utility]]$pars),
                     NUM_UPDATING_PARAMETERS    = length(modelDetails$updating[[updating]]$pars),
                     NUM_TEMPERATURE_PARAMETERS = length(modelDetails$temperature[[temperature]]$pars),
                     wins  = scale * deck$wins, losses = scale * deck$losses)

    return(stanData)
}
