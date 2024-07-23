createStanDataForSimulation <- function(igtObject, utility, updating,
                                        temperature, pars, scale){

    # Get IGT parameters
    n_trials <- igtObject$numTrials()
    n_decks  <- igtObject$numDecks()
    decks    <- igtObject$getDecks()

    # Setup stan data
    stan_data <- list(NUM_TRIALS = n_trials,
                      NUM_DECKS = n_decks,

                      UTILITY_FUNCTION =
                          which(utility == names(modelComponentDetails$utility)),
                      UPDATING_FUNCTION =
                          which(updating == names(modelComponentDetails$updating)),
                      TEMPERATURE_FUNCTION =
                          which(temperature == names(modelComponentDetails$temperature)),
                      NUM_UTILITY_PARAMETERS = length(pars$utility),
                      NUM_UPDATING_PARAMETERS = length(pars$updating),
                      NUM_TEMPERATURE_PARAMETERS = length(pars$temperature),

                      win = scale * deck$win,
                      loss = scale * deck$loss)

    return(stan_data)
}
