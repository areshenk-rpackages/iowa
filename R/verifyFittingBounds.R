#' Verify provided fitting bounds
#'
#' Internal function. Verify that fitting parameters are correct, and correspond to implemented models.
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
#' @return Generates an error message in the event of incorrect input. Otherwise,
#' returns a copy of pars, possibly with missing bounds set to their defaults.

verifyFittingBounds <- function(utility, updating, temperature, pars = NULL){

    if (is.null(pars))
        pars <- list()

    # Verify model functions
    if (!utility %in% names(modelDetails$utility)){
        stop('Unrecognized utility function')
    } else if (!updating %in% names(modelDetails$updating)){
        stop('Unrecognized updating function')
    } else if (!temperature %in% names(modelDetails$temperature)){
        stop('Unrecognized temperature function')
    }

    # If component is unspecified, set it to the default
    if (!'utility' %in% names(pars))
        pars$utility <- lapply(modelDetails$utility[[utility]]$pars,
                               function(i) i$default_bounds)
    if (!'updating' %in% names(pars))
        pars$updating <- lapply(modelDetails$updating[[updating]]$pars,
                               function(i) i$default_bounds)
    if (!'temperature' %in% names(pars))
        pars$temperature <- lapply(modelDetails$temperature[[temperature]]$pars,
                               function(i) i$default_bounds)

    # If any individual parameter is unspecified, set it to the default
    # if (any(!names(modelDetails$utility[[utility]]$pars) %in% names(pars$utility))) {
    #     missing_par <- names(modelDetails$utility[[utility]]$pars)[which(!names(modelDetails$utility[[utility]]$pars) %in% names(pars$utility))]
    #     pars$utility[[missing_par]] <- modelDetails$utility[[utility]]$pars[[missing_par]]$default_bounds
    # }
    # if (any(!names(modelDetails$updating[[updating]]$pars) %in% names(pars$updating))) {
    #     missing_par <- names(modelDetails$updating[[updating]])[which(!names(modelDetails$updating[[updating]]) %in% names(pars$updating))]
    #     pars$updating[[missing_par]] <- modelDetails$updating[[updating]][[missing_par]]$default_bounds
    # }
    # if (any(!names(modelDetails$temperature[[temperature]]$pars) %in% names(pars$temperature))) {
    #     missing_par <- names(modelDetails$temperature[[temperature]])[which(!names(modelDetails$temperature[[temperature]]) %in% names(pars$temperature))]
    #     pars$temperature[[missing_par]] <- modelDetails$temperature[[temperature]][[missing_par]]$default_bounds
    # }

    # Get number of parameters, and check that the correct number has been provided
    nparam_utility <- length(modelDetails$utility[[utility]]$pars)
    if (nparam_utility != length(pars$utility))
        stop('Utility has incorrect number of parameters')

    nparam_updating <- length(modelDetails$updating[[updating]]$pars)
    if (nparam_updating != length(pars$updating))
        stop('Updating has incorrect number of parameters')

    nparam_temperature <- length(modelDetails$temperature[[temperature]]$pars)
    if (nparam_temperature != length(pars$temperature))
        stop('Temperature has incorrect number of parameters')

    # Get parameter names and check that names are correct
    names_utility     <- names(modelDetails$utility[[utility]]$pars)
    if (!setequal(names(pars$utility), names_utility))
        stop('Incorrect utility parameter names')

    names_updating    <- names(modelDetails$updating[[updating]]$pars)
    if (!setequal(names(pars$updating), names_updating))
        stop('Incorrect updating parameter names')

    names_temperature <- names(modelDetails$temperature[[temperature]]$pars)
    if (!setequal(names(pars$temperature), names_temperature))
        stop('Incorrect temperature parameter names')

    # Check that bounds are acceptable
    utilityBoundViolation <- sapply(names_utility, function(n) {
        p <- pars$utility[[n]]
        if (length(p) != 2)
            stop('All utility parameters must have precisely two bounds')
        else if (p[1] >= p[2])
            stop('Utility bounds must be in accending order')
        else if ( (p[1] < modelDetails$utility[[utility]]$pars[[n]]$bounds[1] |
                   p[1] > modelDetails$utility[[utility]]$pars[[n]]$bounds[2]) |
                  (p[2] < modelDetails$utility[[utility]]$pars[[n]]$bounds[1] |
                   p[2] > modelDetails$utility[[utility]]$pars[[n]]$bounds[2]))
            stop('One or more utility bounds exceeds allowed range')
    })

    updatingBoundViolation <- sapply(names_updating, function(n) {
        p <- pars$updating[[n]]
        if (length(p) != 2)
            stop('All updating parameters must have precisely two bounds')
        else if (p[1] >= p[2])
            stop('Updating bounds must be in accending order')
        else if ( (p[1] < modelDetails$updating[[updating]]$pars[[n]]$bounds[1] |
                   p[1] > modelDetails$updating[[updating]]$pars[[n]]$bounds[2]) |
                  (p[2] < modelDetails$updating[[updating]]$pars[[n]]$bounds[1] |
                   p[2] > modelDetails$updating[[updating]]$pars[[n]]$bounds[2]))
            stop('One or more updating bounds exceeds allowed range')
    })

    temperatureBoundViolation <- sapply(names_temperature, function(n) {
        p <- pars$temperature[[n]]
        if (length(p) != 2)
            stop('All temperature parameters must have precisely two bounds')
        else if (p[1] >= p[2])
            stop('Temperature bounds must be in accending order')
        else if ( (p[1] < modelDetails$temperature[[temperature]]$pars[[n]]$bounds[1] |
                   p[1] > modelDetails$temperature[[temperature]]$pars[[n]]$bounds[2]) |
                  (p[2] < modelDetails$temperature[[temperature]]$pars[[n]]$bounds[1] |
                   p[2] > modelDetails$temperature[[temperature]]$pars[[n]]$bounds[2]))
            stop('One or more temperature bounds exceeds allowed range')
    })

    # Place parameters in standard order
    pars$utility     <- pars$utility[names(modelDetails$utility[[utility]]$pars)]
    pars$updating    <- pars$updating[names(modelDetails$updating[[updating]]$pars)]
    pars$temperature <- pars$temperature[names(modelDetails$temperature[[temperature]]$pars)]

    return(pars)

}
