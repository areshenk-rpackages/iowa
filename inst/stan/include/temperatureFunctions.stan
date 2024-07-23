// Trial-dependent choice
real temperature_TDC(int t, real[] par) {
    return pow(t/10.0, par[1]);
}

// Trial-independent choice
real temperature_TIC(int t, real[] par) {
    return pow(3.0, par[1]) - 1;
}
