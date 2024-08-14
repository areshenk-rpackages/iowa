#' Simulate performance of an IGT model
#'
#' Simulate IGT performance using a specified model.
#'
#' @param deck Either the name of an included deck or a named list
#' with matrices entries "wins" and "losses".
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
#' @return A named list containing simulated IGT data.
#' @export

simulateIGT <- function(deck = 'bechara', n, utility, updating,
                        temperature, pars, scale = .01) {

    if (is.character(deck))
        deck <- importDeck(deck)

    # Verify pars
    verifyStanParamsForSimulation(utility, updating, temperature, pars)

    # Create Stan data object
    stanData <- createStanDataForSimulation(deck, utility, updating, temperature, pars, scale)

    # Set parameter values
    paramValues <- setStanParamsForSimulation(utility, updating, temperature, pars)

    # Call Stan simulation
    model <- stan_package_model(name = "simulate_igt", package = "iowa")
    fit <- model$generate_quantities(fitted_params = paramValues, data = stanData)
    draws <- fit$draws()
    
    ret <- formatSimulationOutput(draws, paramValues, deck, scale)

    return(ret)
}
