#' Simulate performance of IGT model
#'
#' Internal function. Verify that simulation parameters are correct, and correspond to implemented models.
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
#' @return Nothing. Generates an error message in the event of incorrect input.
#' @export

simulateIGT <- function(deck, n, utility, updating,
                        temperature, pars, scale = .01,
                        fullOutput = FALSE) {

    if (is.character(deck)) {
        deckPath <- system.file(paste0('extdata/decks/deck_', deck, '.rds'), package = "iowa")
        deck <- readRDS(deckPath)
    }

    # Output
    if (fullOutput){
        returnPars <- c('choice', 'wins', 'losses', 'V', 'P', 'U')
    } else {
        returnPars <- c('choice', 'wins', 'losses')
    }

    # Verify pars
    verifySimulationParameters(utility, updating, temperature, pars)

    # Create Stan data object
    stanData <- createStanDataForSimulation(object, utility, updating, temperature, pars, scale)

    # Set parameter values
    paramValues <- setStanParamsForSimulation(utility, updating, temperature, pars)

    # Call Stan simulation
    fit <- sampling(stanmodels$simulate_igt, data = stanData,
                    algorithm = 'Fixed_param', pars = returnPars,
                    init = paramValues, chains = 1, warmup = 0,
                    iter = n)

    simData             <- extract(fit)[returnPars]
    simData$wins        <- simData$wins / scale
    simData$losses      <- simData$losses / scale
    simData$utility     <- utility
    simData$updating    <- updating
    simData$temperature <- temperature
    simData$pars        <- pars
    return(simData)
}
