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
#' @return A list for Stan.

setStanParamsForSimulation <- function(utility, updating, temperature, pars){

    names_utility     <- names(modelDetails$utility[[utility]])
    names_updating    <- names(modelDetails$updating[[updating]])
    names_temperature <- names(modelDetails$temperature[[temperature]])

    list(list(
         utility_params = array(unlist(pars$utility[names_utility])),
         updating_params = array(unlist(pars$updating[names_updating])),
         temperature_params = array(unlist(pars$temperature[names_temperature]))))

}
