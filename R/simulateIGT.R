simulateIGT <- function(object, n, utility, updating,
                        temperature, pars, scale = .01,
                        full_output = FALSE) {

    # Output
    if (full_output){
        return_pars <- c('choice', 'wins', 'losses', 'V', 'P', 'U')
    } else {
        return_pars <- c('choice', 'wins', 'losses')
    }

    # Verify pars
    verifySimulationParameters(utility, updating, temperature, pars)

    # Create Stan data object
    stan_data <- createStanDataForSimulation(object, utility, updating,
                                             temperature, pars, scale)

    # Set parameter values
    param_values <- setStanParamsForSimulation(utility, updating,
                                               temperature, pars)

    # Call Stan simulation
    fit <- sampling(stanmodels$simulate_igt, data = stan_data,
                    algorithm = 'Fixed_param', pars = return_pars,
                    init = param_values, chains = 1, warmup = 0,
                    iter = n)

    sim_data             <- extract(fit)[return_pars]
    sim_data$wins        <- sim_data$wins / scale
    sim_data$losses      <- sim_data$losses / scale
    sim_data$utility     <- utility
    sim_data$updating    <- updating
    sim_data$temperature <- temperature
    sim_data$pars        <- pars
    return(sim_data)
}
