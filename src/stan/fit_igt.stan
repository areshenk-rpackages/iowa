functions {
    
    //// UTILITY ////
    // EU utility function (used in EV model)
    real utility_EU(real win, real loss, array[] real par) {
        return (1-par[1]) * win - par[1] * loss;
    }
    
    // Prospect utility function
    real utility_PU(real win, real loss, array[] real par) {
        real net = win - loss;
        if (net >= 0){
            return pow(net, par[1]);
        } else {
            return -par[2] * pow(abs(net), par[1]);
        }
    }
    
    // Prospect utility 2 function (used by Dai in PVL2 model)
    real utility_PU2(real win, real loss, array[] real par) {
        return pow(win, par[1]) - par[2] * pow(abs(loss), par[1]);
    }
        
    // Utility wrapper
    real utility(real win, real loss, array[] real par, int ind){
        if (ind == 1){
            return utility_EU(win, loss, par);
        } else if (ind == 2){
            return utility_PU(win, loss, par);
        } else if (ind == 3){
            return utility_PU2(win, loss, par);
        } else {
            return 0;
        }
    }
    
    //// UPDATING ////
    // Decay reinforcement learning rule
    vector updating_DRL(vector V, real u, int sel, array[] real par) {
        vector[num_elements(V)] V_new = par[1] * V;
        V_new[sel] = V_new[sel] + u;
        return V_new;
    }
    
    // Delta learning rule
    vector updating_DEL(vector V, real u, int sel, array[] real par) {
        vector[num_elements(V)] V_new = V;
        V_new[sel] = V_new[sel] + par[1] * (u - V_new[sel]);
        return V_new;
    }
    
    // Mixed learning rule
    vector updating_ML(vector V, real u, int sel, array[] real par) {
        vector[num_elements(V)] V_new = (1 - par[1]) * V;
        V_new[sel] = par[2] * (u - (1-par[1]) * V[sel]);
        return V_new;
    }
    
    // Updating wrapper
    vector updating(vector V, real u, int sel, array[] real par, int ind){
        if (ind == 1){
            return updating_DRL(V, u, sel, par);
        } else if (ind == 2){
            return updating_DEL(V, u, sel, par);
        } else if (ind == 3){
            return updating_ML(V, u, sel, par);
        } else {
            return V;
        }
    }
    
    //// TEMPERATURE ////
    // Trial-dependent choice
    real temperature_TDC(int t, array[] real par) {
        return pow(t/10.0, par[1]);
    }
    
    // Trial-independent choice
    real temperature_TIC(int t, array[] real par) {
        return pow(3.0, par[1]) - 1;
    }
    
    // Temperature wrapper
    real temperature(int t, array[] real par, int ind){
    
        if (ind == 1){
            return temperature_TDC(t, par);
        } else if (ind == 2){
            return temperature_TIC(t, par);
        } else {
            return 1;
        }
    }

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

  array[NUM_UTILITY_PARAMETERS] real UTILITY_LOWER_BOUND;
  array[NUM_UPDATING_PARAMETERS] real UPDATING_LOWER_BOUND;
  array[NUM_TEMPERATURE_PARAMETERS] real TEMPERATURE_LOWER_BOUND;

  array[NUM_UTILITY_PARAMETERS] real UTILITY_UPPER_BOUND;
  array[NUM_UPDATING_PARAMETERS] real UPDATING_UPPER_BOUND;
  array[NUM_TEMPERATURE_PARAMETERS] real TEMPERATURE_UPPER_BOUND;
  
  array[NUM_TRIALS] real wins;
  array[NUM_TRIALS] real losses;
  array[NUM_TRIALS] int choices;

  real<lower=1> reg;
}

parameters {
  array[NUM_UTILITY_PARAMETERS] real<lower=0, upper=1> raw_utility_params;
  array[NUM_UPDATING_PARAMETERS] real<lower=0, upper=1> raw_updating_params;
  array[NUM_TEMPERATURE_PARAMETERS] real<lower=0, upper=1> raw_temperature_params;
}

transformed parameters {
  array[NUM_UTILITY_PARAMETERS] real utility_params;
  array[NUM_UPDATING_PARAMETERS] real updating_params;
  array[NUM_TEMPERATURE_PARAMETERS] real temperature_params;

  for (i in 1:NUM_UTILITY_PARAMETERS){
      utility_params[i] = (UTILITY_UPPER_BOUND[i] - UTILITY_LOWER_BOUND[i]) *
                          raw_utility_params[i] + UTILITY_LOWER_BOUND[i];
  }

  for (i in 1:NUM_UPDATING_PARAMETERS){
      updating_params[i] = (UPDATING_UPPER_BOUND[i] - UPDATING_LOWER_BOUND[i]) *
                          raw_updating_params[i] + UPDATING_LOWER_BOUND[i];
  }

  for (i in 1:NUM_TEMPERATURE_PARAMETERS){
      temperature_params[i] = (TEMPERATURE_UPPER_BOUND[i] -
                               TEMPERATURE_LOWER_BOUND[i]) *
                          raw_temperature_params[i] + TEMPERATURE_LOWER_BOUND[i];
  }

}

model {

    // Dummy variables
    vector[NUM_DECKS]  V;
    real theta;
    real U;
    for (d in 1:4){
        V[d] = 0;
    }

    // Likelihood
    for (t in 1:NUM_TRIALS){

        // Compute temperature
        theta = temperature(t, temperature_params, TEMPERATURE_FUNCTION);

        // Draw card
        choices[t] ~ categorical_logit(theta * V);

        // Compute utility
        U = utility(wins[t], losses[t], utility_params, UTILITY_FUNCTION);

        // Update deck values
        V = updating(V, U, choices[t], updating_params, UPDATING_FUNCTION);
    }

    // Priors
    raw_utility_params ~ beta(reg, reg);
    raw_updating_params ~ beta(reg, reg);
    raw_temperature_params ~ beta(reg, reg);
}
