#' Create list of data for Stan likelihood computation
#'
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

createStanDataForLikelihood <- function(wins, losses, choices, numDecks, utility, 
                                        updating, temperature, scale){
    
    # Setup stan data
    stanData <- list(NUM_TRIALS = length(wins), NUM_DECKS = numDecks,
                     UTILITY_FUNCTION     = modelDetails$utility[[utility]]$index,
                     UPDATING_FUNCTION    = modelDetails$updating[[updating]]$index,
                     TEMPERATURE_FUNCTION = modelDetails$temperature[[temperature]]$index,
                     NUM_UTILITY_PARAMETERS     = length(modelDetails$utility[[utility]]$pars),
                     NUM_UPDATING_PARAMETERS    = length(modelDetails$updating[[updating]]$pars),
                     NUM_TEMPERATURE_PARAMETERS = length(modelDetails$temperature[[temperature]]$pars),
                     wins  = scale * wins, losses = scale * losses, choices = choices)
    
    return(stanData)
}
