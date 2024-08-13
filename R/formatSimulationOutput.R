#' Organize simulation generated quantities
#'
#' @param draws A draws_array object returned by a simulate_igt model
#' @param paramValues A draws_array object returned by createStanParamsForSimulation()
#' @param deck A deck object
#' @param scale The scale factor used for simulation
#' @return A named list containing simulated IGT data.
#' @importFrom posterior as_draws_df

formatSimulationOutput <- function(draws, paramValues, deck, scale) {
    
    # Format simulated data
    d    <- as_draws_df(draws)

    varnames <- c('choice', 'win', 'loss', 'V', 'P', 'U')
    varpattern <- c('choice[', 'win', 'loss', 'V[', 'P', 'U')
    
    idx <- lapply(varpattern, grepl, x = names(d), fixed = T)
    names(idx) <- varnames
    
    vars    <- lapply(idx, function(i) d[,i])
    ntrials <- ncol(vars$choice)
    nchains <- nrow(vars$choice)
    ndecks  <- ncol(vars$V) / ntrials
    
    V      <- array(t(as.matrix(vars$V)), dim = c(ntrials, ndecks, nchains))
    P      <- array(t(as.matrix(vars$P)), dim = c(ntrials, ndecks, nchains))
    wins   <- t(as.matrix(vars$win))
    losses <- t(as.matrix(vars$loss))
    choices <- t(as.matrix(vars$choice))
    U      <- t(as.matrix(vars$U))
    
    simData <- list('choices' = choices, 'wins' = wins, 'losses' = losses,
                'V' = V, 'P' = P, 'U' = U)
    
    # Format parameter values
    paramdf <- as_draws_df(paramValues)
    paramdf <- paramdf[,grepl('params', names(paramdf))]
    
    ret <- list('deck' = deck, 'params' = paramdf, 'simData' = simData)
    
    return(ret)
}







