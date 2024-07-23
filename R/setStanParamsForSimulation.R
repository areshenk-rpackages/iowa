setStanParamsForSimulation <- function(utility, updating, temperature, pars){

    list(list(
         utility_params =
             array(unlist(pars$utility[modelComponentDetails$utility[[utility]]$par_names])),
         updating_params =
             array(unlist(pars$updating[modelComponentDetails$updating[[updating]]$par_names])),
         temperature_params =
             array(unlist(pars$temperature[modelComponentDetails$temperature[[temperature]]$par_names]))))

}
