real temperature(int t, real[] par, int ind){

    if (ind == 1){
        return temperature_TDC(t, par);
    } else if (ind == 2){
        return temperature_TIC(t, par);
    } else {
        return 1;
    }
}
