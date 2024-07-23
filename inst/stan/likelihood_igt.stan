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

  real utility_params[NUM_UTILITY_PARAMETERS];
  real updating_params[NUM_UPDATING_PARAMETERS];
  real temperature_params[NUM_TEMPERATURE_PARAMETERS];

  vector[NUM_TRIALS] win;
  vector[NUM_TRIALS] loss;
  int choice[NUM_TRIALS];
}

model {
}

generated quantities {
    vector[NUM_DECKS]  V;
    simplex[NUM_DECKS] P;
    vector[NUM_TRIALS] probs;
    real U;
    real lhd;
    real theta;
    for (d in 1:NUM_DECKS){
        V[d] = 0;
    }
    probs[1] = .25;

    // Play IGT
    for (t in 2:NUM_TRIALS){

        // Compute utility of previous outcome
        U = utility(win[t-1], loss[t-1], utility_params, UTILITY_FUNCTION);

        // Update deck valuations
        V = updating(V, U, choice[t-1], updating_params, UPDATING_FUNCTION);

        // Compute temperature
        theta = temperature(t, temperature_params, TEMPERATURE_FUNCTION);

        // Compute probability of current selection
        P = softmax(theta * V);
        probs[t] = P[choice[t]];

    }

    lhd = sum(log(probs));
}
