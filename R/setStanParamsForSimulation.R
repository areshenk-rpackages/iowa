#' Create parameter object for Stan
#'
#' Internal function.
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
#' @importFrom posterior as_draws_array
#' @return A draws_array object for stan

setStanParamsForSimulation <- function(utility, updating, temperature, pars){

    names_utility     <- names(modelDetails$utility[[utility]]$pars)
    names_updating    <- names(modelDetails$updating[[updating]]$pars)
    names_temperature <- names(modelDetails$temperature[[temperature]]$pars)

    # Create parameter names for stan program
    parnames <- c(paste0('utility_params[',     1:length(names_utility), ']'),
                  paste0('updating_params[',    1:length(names_updating), ']'),
                  paste0('temperature_params[', 1:length(names_temperature), ']'))

    # Make sure parameters are provided in correct order
    pars <- list('utility' = pars$utility[names_utility],
                 'updating' = pars$updating[names_updating],
                 'temperature' = pars$temperature[names_temperature])
    parlists <- unlist(pars, recursive = F)
    names(parlists) <- parnames
    numpars <- sapply(parlists, length)

    if (length(unique(numpars)) != 1)
        stop('Parameter vectors must have equal length')

    arr <- as_draws_array(parlists)

    return(arr)

}
