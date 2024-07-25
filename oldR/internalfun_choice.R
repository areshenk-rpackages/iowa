choicefun_fixedtemp <- function(V, par) {
    theta <- 3^par$c - 1
    V <- theta * V
    V <- V - mean(V)
    return(exp(V)/sum(exp(V)))
}

choicefun_vartemp <- function(V, trial, par) {
    theta <- (trial/10)^par$c
    V <- theta * V
    V <- V - mean(V)
    return(exp(V)/sum(exp(V)))
}
