#' Organize likelihood generated quantities
#'
#' @param draws A draws_array object returned by a simulate_igt model
#' @param paramValues A draws_array object returned by createStanParamsForSimulation()
#' @return A named list containing the log-likelihood for the provided model parameters
#' @importFrom posterior as_draws_df

formatLikelihoodOutput <- function(draws, paramValues) {
    
    # Format simulated data
    d    <- as_draws_df(draws)
    
    # Format parameter values
    paramdf <- as_draws_df(paramValues)
    paramdf <- paramdf[,grepl('params', names(paramdf))]
    paramdf$logLik <- d[,grepl('lhd', names(d))]
    
    return(paramdf)
}
