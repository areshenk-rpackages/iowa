#' Verify provided simulation parameters
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

verifySimulationParameters <- function(utility, updating, temperature, pars){

    # Verify model functions
    if (!utility %in% names(modelDetails$utility)){
        stop('Unrecognized utility function')
    } else if (!updating %in% names(modelDetails$updating)){
        stop('Unrecognized updating function')
    } else if (!temperature %in% names(modelDetails$temperature)){
        stop('Unrecognized temperature function')
    }

    # Get number of parameters, and check that the correct number has been provided
    nparam_utility <- length(modelDetails$utility[[utility]])
    if (nparam_utility != length(pars$utility))
        stop('Utility has incorrect number of parameters')

    nparam_updating <- length(modelDetails$updating[[updating]])
    if (nparam_updating != length(pars$updating))
        stop('Updating has incorrect number of parameters')

    nparam_temperature <- length(modelDetails$temperature[[temperature]])
    if (nparam_temperature != length(pars$temperature))
        stop('Temperature has incorrect number of parameters')

    # Get parameter names and check that names are correct
    names_utility     <- names(modelDetails$utility[[utility]])
    if (!setequal(names(pars$utility), names_utility))
        stop('Incorrect utility parameter names')

    names_updating    <- names(modelDetails$updating[[updating]])
    if (!setequal(names(pars$updating), names_updating))
        stop('Incorrect updating parameter names')

    names_temperature <- names(modelDetails$temperature[[temperature]])
    if (!setequal(names(pars$temperature), names_temperature))
        stop('Incorrect temperature parameter names')

    # Check that all arguments are present, and that all are scalars
    parUnlist <- unlist(pars, recursive = T)
    if (any(sapply(parUnlist, function(i) length(i) > 1 | !is.numeric(i))))
        stop('Parameter values must be a single scalar')

    # Check that parameters respect bounds
    utilityBoundViolation <- sapply(names_utility, function(n) {
        p <- pars$utility[[n]]
        p < modelDetails$utility[[utility]][[n]]$bounds[1] |
            p > modelDetails$utility[[utility]][[n]]$bounds[2]
    })
    if (any(utilityBoundViolation))
        stop('One or more utility parameters exceeds a required bound')

    updatingBoundViolation <- sapply(names_updating, function(n) {
        p <- pars$updating[[n]]
        p < modelDetails$updating[[updating]][[n]]$bounds[1] |
            p > modelDetails$updating[[updating]][[n]]$bounds[2]
    })
    if (any(updatingBoundViolation))
        stop('One or more updating parameters exceeds a required bound')

    temperatureBoundViolation <- sapply(names_temperature, function(n) {
        p <- pars$temperature[[n]]
        p < modelDetails$temperature[[temperature]][[n]]$bounds[1] |
            p > modelDetails$temperature[[temperature]][[n]]$bounds[2]
    })
    if (any(temperatureBoundViolation))
        stop('One or more temperature parameters exceeds a required bound')

}
