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

    # Get number of parameters
    nparam_utility <- length(modelDetails$utility[[utility]])
    nparam_updating <- modelDetails$updating[[updating]]$num_params
    nparam_temperature <- modelDetails$temperature[[temperature]]$num_params

    # Get parameter names
    names_utility     <- modelDetails$utility[[utility]]$par_names
    names_updating    <- modelDetails$updating[[updating]]$par_names
    names_temperature <- modelDetails$temperature[[temperature]]$par_names

    # Get bounds
    lbound_utility <- modelDetails$utility[[utility]]$lower
    ubound_utility <- modelDetails$utility[[utility]]$upper

    lbound_updating <- modelDetails$updating[[updating]]$lower
    ubound_updating <- modelDetails$updating[[updating]]$upper

    lbound_temperature <- modelDetails$temperature[[temperature]]$lower
    ubound_temperature <- modelDetails$temperature[[temperature]]$upper

    # Verify number of parameters
    if (length(pars$utility) != nparam_utility){
        stop(paste(utility, 'utility function requires', nparam_utility, 'parameter(s).'))
    } else if (length(pars$updating) != nparam_updating){
        stop(paste(updating, 'updating function requires', nparam_updating, 'parameter(s).'))
    } else if (length(pars$temperature) != nparam_temperature){
        stop(paste(temperature, 'temperature function requires', nparam_temperature, 'parameter(s).'))
    }

    # Verify parameter names
    if (any(!names(pars$utility) %in% names_utility)){
        stop(paste(utility, 'utility function requires parameter(s):', names_utility))
    } else if (any(!names(pars$updating) %in% names_updating)){
        stop(paste(updating, 'updating function requires parameter(s):', names_updating))
    } else if (any(!names(pars$temperature) %in% names_temperature)){
        stop(paste(temperature, 'temperature function requires parameter(s):', names_temperature))
    }

    # Verify lower bounds
    utility_bounds <- unlist(pars$utility[names_utility]) < lbound_utility
    utility_bounds[is.na(utility_bounds)] <- FALSE
    if (any(utility_bounds)){
        stop(paste('One or more utility parameters lies below the lower bound.'))
    }

    updating_bounds <- unlist(pars$updating[names_updating]) < lbound_updating
    updating_bounds[is.na(updating_bounds)] <- FALSE
    if (any(updating_bounds)){
        stop(paste('One or more updating parameters lies below the lower bound.'))
    }

    temperature_bounds <- unlist(pars$temperature[names_temperature]) < lbound_temperature
    temperature_bounds[is.na(temperature_bounds)] <- FALSE
    if (any(temperature_bounds)){
        stop(paste('One or more temperature parameters lies below the lower bound.'))
    }

    # Verify upper bounds
    utility_bounds <- unlist(pars$utility[names_utility]) > ubound_utility
    utility_bounds[is.na(utility_bounds)] <- FALSE
    if (any(utility_bounds)){
        stop(paste('One or more utility parameters lies above the upper bound.'))
    }

    updating_bounds <- unlist(pars$updating[names_updating]) > ubound_updating
    updating_bounds[is.na(updating_bounds)] <- FALSE
    if (any(updating_bounds)){
        stop(paste('One or more updating parameters lies above the upper bound.'))
    }

    temperature_bounds <- unlist(pars$temperature[names_temperature]) > ubound_temperature
    temperature_bounds[is.na(temperature_bounds)] <- FALSE
    if (any(temperature_bounds)){
        stop(paste('One or more temperature parameters lies above the upper bound.'))
    }
}
