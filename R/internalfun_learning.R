learning_ev <- function(u, V, choice, par) {
    r <- par$r
    V[choice] <- (1-r)*V[choice] + r * u
    return(V)
}

learning_pv <- function(u, V, choice, par) {
    r <- par$r
    V <- (1-r)*V
    V[choice] <- r * u
    return(V)
}

learning_delta <- function(u, V, choice, par) {
    r <- par$r
    V[choice] <- V[choice] + r*(u - V[choice])
    return(V)
}
