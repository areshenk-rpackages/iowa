utilityfun_pv <- function(x, par) {
    A <- par$A
    l <- par$l
    ifelse(x >= 0, x^A, -l*abs(x)^A)
}

utilityfun_ev <- function(g, l, par) {
    w <- par$w
    (1-w)*g + w*l
}
