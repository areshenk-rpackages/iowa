// Decay reinforcement learning rule
vector updating_DRL(vector V, real u, int sel, real[] par) {
    vector[num_elements(V)] V_new = par[1] * V;
    V_new[sel] = V_new[sel] + u;
    return V_new;
}

// Delta learning rule
vector updating_DEL(vector V, real u, int sel, real[] par) {
    vector[num_elements(V)] V_new = V;
    V_new[sel] = V_new[sel] + par[1] * (u - V_new[sel]);
    return V_new;
}

// Mixed learning rule
vector updating_ML(vector V, real u, int sel, real[] par) {
    vector[num_elements(V)] V_new = (1 - par[1]) * V;
    V_new[sel] = par[2] * (u - (1-par[1]) * V[sel]);
    return V_new;
}
