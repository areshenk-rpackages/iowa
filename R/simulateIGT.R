#' Simulate performance of an IGT model
#'
#' Simulate IGT performance using a specified model.
#'
#' @param deck Either the name of an included deck or a named list
#' with matrices entries "win" and "loss".
#' @param n Number of subjects to simulate
#' @param updating Name of updating function
#' @param temperature Name of temperature function
#' @param pars A named list of parameters. See details.
#' @param scale How to scale outcomes before passing to the model. Typically, outcomes
#' are divided by 100 (i.e. scale = .01).
#' @param fullOutput Whether to output internal state variables (i.e. deck valuations)
#' @details Argument "pars" is a named list with fields:
#' \itemize{
#'  \item{"utility": }{A named list of parameter values. Names must correspond to
#'  the parameters of the utility function specified by argument "utility".}
#'  \item{"updating": }{See above.}
#'  \item{"temperature": }{See above.}
#' }
#'
#' @seealso [importDeck()] for available deck structures, and more information
#' about the structure of a deck object
#' @return Nothing. Generates an error message in the event of incorrect input.
#' @export

simulateIGT <- function(deck = 'bechara', n, utility, updating,
                        temperature, pars, scale = .01,
                        fullOutput = FALSE) {

    if (is.character(deck))
        deck <- importDeck(deck)

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
