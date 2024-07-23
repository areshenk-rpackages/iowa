// EU utility function (used in EV model)
real utility_EU(real win, real loss, real[] par) {
    return (1-par[1]) * win - par[1] * loss;
}

// Prospect utility function
real utility_PU(real win, real loss, real[] par) {
    real net = win - loss;
    if (net >= 0){
        return pow(net, par[1]);
    } else {
        return -par[2] * pow(fabs(net), par[1]);
    }
}

// Prospect utility 2 function (used by Dai in PVL2 model)
real utility_PU2(real win, real loss, real[] par) {
    return pow(win, par[1]) - par[2] * pow(fabs(loss), par[1]);
}
