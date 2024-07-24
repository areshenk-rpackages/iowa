functions {
#include /include/utilityFunctions.stan
#include /include/updatingFunctions.stan
#include /include/temperatureFunctions.stan

#include /include/utilityWrapper.stan
#include /include/updatingWrapper.stan
#include /include/temperatureWrapper.stan
}

data {
  int<lower=0> NUM_TRIALS;
  int<lower=0> NUM_DECKS;

  int UTILITY_FUNCTION;
  int UPDATING_FUNCTION;
  int TEMPERATURE_FUNCTION;

  int<lower=0> NUM_UTILITY_PARAMETERS;
  int<lower=0> NUM_UPDATING_PARAMETERS;
  int<lower=0> NUM_TEMPERATURE_PARAMETERS;

  real win[NUM_TRIALS, NUM_DECKS];
  real loss[NUM_TRIALS, NUM_DECKS];
}

parameters {
  real utility_params[NUM_UTILITY_PARAMETERS];
  real updating_params[NUM_UPDATING_PARAMETERS];
  real temperature_params[NUM_TEMPERATURE_PARAMETERS];
}

model {
}

generated quantities {
    vector[NUM_DECKS]  V[NUM_TRIALS];
    simplex[NUM_DECKS] P[NUM_TRIALS];
    vector[NUM_TRIALS] wins;
    vector[NUM_TRIALS] losses;
    vector[NUM_TRIALS] U;
    vector[NUM_DECKS] n_choices;
    real theta;
    vector[NUM_DECKS] V_dummy;
    int card[NUM_DECKS];
    int choice[NUM_TRIALS];
    for (d in 1:NUM_DECKS){
        V_dummy[d] = 0;
        card[d] = 0;
    }

    // Play IGT
    for (t in 1:NUM_TRIALS){

        // Deck values for each trial
        V[t] = V_dummy;

        // Compute temperature
        theta = temperature(t, temperature_params, TEMPERATURE_FUNCTION);

        // Draw card
        P[t] = softmax(theta * V[t]);
        choice[t] = categorical_rng(P[t]);
        card[choice[t]] = card[choice[t]] + 1;

        // Wins and losses
        wins[t]  = win[card[choice[t]], choice[t]];
        losses[t] = loss[card[choice[t]], choice[t]];

        // Compute utility
        U[t] = utility(wins[t], losses[t],
                       utility_params, UTILITY_FUNCTION);

        // Update deck values
        V_dummy = updating(V_dummy, U[t], choice[t],
                           updating_params, UPDATING_FUNCTION);
    }
}
