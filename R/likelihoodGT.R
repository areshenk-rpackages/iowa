#' Compute likelihood of an IGT model
#'
#' Likelihood calculation for a specified model.
#'
#' @param wins Vector of wins
#' @param losses Vector of losses
#' @param choice Vector of deck choices
#' @param numDecks Number of decks
#' @param updating Name of updating function
#' @param temperature Name of temperature function
#' @param pars A named list of parameters. See details.
#' @param scale How to scale outcomes before passing to the model. Typically, outcomes
#' are divided by 100 (i.e. scale = .01).
#' @details Argument "pars" is a named list with fields:
#' \itemize{
#'  \item{"utility": }{A named list of parameter values. Names must correspond to
#'  the parameters of the utility function specified by argument "utility".}
#'  \item{"updating": }{See above.}
#'  \item{"temperature": }{See above.}
#' }
#'
#' @return A named list containing simulated IGT data.
#' @export

likelihoodIGT <- function(wins, losses, choices, numDecks, utility, updating,
                        temperature, pars, scale = .01) {

    # Verify pars
    verifyStanParamsForSimulation(utility, updating, temperature, pars)

    # Create Stan data object
    stanData <- createStanDataForLikelihood(wins, losses, choices, numDecks, utility, 
                                            updating, temperature, scale)

    # Set parameter values
    paramValues <- setStanParamsForSimulation(utility, updating, temperature, pars)

    # Call Stan simulation
    model <- stan_package_model(name = "likelihood_igt", package = "iowa")
    fit <- model$generate_quantities(fitted_params = paramValues, data = stanData)
    draws <- fit$draws()
    ret <- formatLikelihoodOutput(draws, paramValues)
    
    return(ret)
}
