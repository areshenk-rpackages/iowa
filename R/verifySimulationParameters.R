verifySimulationParameters <- function(utility, updating, temperature, pars){

    # Verify model functions
    if (!utility %in% names(modelComponentDetails$utility)){
        stop('Unrecognized utility function')
    } else if (!updating %in% names(modelComponentDetails$updating)){
        stop('Unrecognized updating function')
    } else if (!temperature %in% names(modelComponentDetails$temperature)){
        stop('Unrecognized temperature function')
    }

    # Verify number of parameters
    if (length(pars$utility) !=
        modelComponentDetails$utility[[utility]]$num_params){
        stop(paste(utility, 'utility function requires',
                   modelComponentDetails$utility[[utility]]$num_params,
                   'parameter(s).'))
    } else if (length(pars$updating) !=
               modelComponentDetails$updating[[updating]]$num_params){
        stop(paste(updating, 'updating function requires',
                   modelComponentDetails$updating[[updating]]$num_params,
                   'parameter(s).'))
    } else if (length(pars$temperature) !=
               modelComponentDetails$temperature[[temperature]]$num_params){
        stop(paste(temperature, 'temperature function requires',
                   modelComponentDetails$temperature[[temperature]]$num_params,
                   'parameter(s).'))
    }

    # Verify parameter names
    if (any(!names(pars$utility) %in%
        modelComponentDetails$utility[[utility]]$par_names)){
        stop(paste(utility, 'utility function requires parameters:',
                   modelComponentDetails$utility[[utility]]$par_names))
    } else if (any(!names(pars$updating) %in%
                   modelComponentDetails$updating[[updating]]$par_names)){
        stop(paste(updating, 'updating function requires parameters:',
                   modelComponentDetails$updating[[updating]]$par_names))
    } else if (any(!names(pars$temperature) %in%
                   modelComponentDetails$temperature[[temperature]]$par_names)){
        stop(paste(temperature, 'temperature function requires parameters:',
                   modelComponentDetails$temperature[[temperature]]$par_names))
    }

    # Verify lower bounds
    utility_bounds <-
        unlist(pars$utility[modelComponentDetails$utility[[utility]]$par_names]) <
        modelComponentDetails$utility[[utility]]$lower
    utility_bounds[is.na(utility_bounds)] <- FALSE
    if (any(utility_bounds)){
        stop(paste('One of more utility parameters lies below the lower bound.'))
    }

    updating_bounds <-
        unlist(pars$updating[modelComponentDetails$updating[[updating]]$par_names]) <
        modelComponentDetails$updating[[updating]]$lower
    updating_bounds[is.na(updating_bounds)] <- FALSE
    if (any(updating_bounds)){
        stop(paste('One of more updating parameters lies below the lower bound.'))
    }

    temperature_bounds <-
        unlist(pars$temperature[modelComponentDetails$temperature[[temperature]]$par_names]) <
        modelComponentDetails$temperature[[temperature]]$lower
    temperature_bounds[is.na(temperature_bounds)] <- FALSE
    if (any(temperature_bounds)){
        stop(paste('One of more temperature parameters lies below the lower bound.'))
    }

    # Verify upper bounds
    utility_bounds <- unlist(pars$utility[modelComponentDetails$utility[[utility]]$par_names]) >
        modelComponentDetails$utility[[utility]]$upper
    utility_bounds[is.na(utility_bounds)] <- FALSE
    if (any(utility_bounds)){
        stop(paste('One of more utility parameters lies above the upper bound.'))
    }

    updating_bounds <- unlist(pars$updating[modelComponentDetails$updating[[updating]]$par_names]) >
        modelComponentDetails$updating[[updating]]$upper
    updating_bounds[is.na(updating_bounds)] <- FALSE
    if (any(updating_bounds)){
        stop(paste('One of more updating parameters lies above the upper bound.'))
    }

    temperature_bounds <- unlist(pars$temperature[modelComponentDetails$temperature[[temperature]]$par_names]) >
        modelComponentDetails$temperature[[temperature]]$upper
    temperature_bounds[is.na(temperature_bounds)] <- FALSE
    if (any(temperature_bounds)){
        stop(paste('One of more temperature parameters lies above the upper bound.'))
    }
}
