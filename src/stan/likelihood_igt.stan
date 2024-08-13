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

    array[NUM_TRIALS] real wins;
    array[NUM_TRIALS] real losses;
    array[NUM_TRIALS] int choices;
}

parameters {
  array[NUM_UTILITY_PARAMETERS] real utility_params;
  array[NUM_UPDATING_PARAMETERS] real updating_params;
  array[NUM_TEMPERATURE_PARAMETERS] real temperature_params;
}

generated quantities {
    vector[NUM_DECKS]  V;
    vector[NUM_DECKS] P;
    vector[NUM_TRIALS] probs;
    real U;
    real lhd;
    real theta;
    for (d in 1:NUM_DECKS){
        V[d] = 0;
    }
    probs[1] = 1.0 / NUM_DECKS;

    // Play IGT
    for (t in 2:NUM_TRIALS){

        // Compute utility of previous outcome
        U = utility(wins[t-1], losses[t-1], utility_params, UTILITY_FUNCTION);

        // Update deck valuations
        V = updating(V, U, choices[t-1], updating_params, UPDATING_FUNCTION);

        // Compute temperature
        theta = temperature(t, temperature_params, TEMPERATURE_FUNCTION);

        // Compute probability of current selection
        P = softmax(theta * V);
        probs[t] = P[choices[t]];

    }

    lhd = sum(log(probs));
}


