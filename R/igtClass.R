IGT <- R6Class("IGT", list(

    # Slots
    win          = NA,
    loss         = NA,
    cards        = NA,
    n.trials     = NA,
    n.decks      = NA,
    scale.factor = NA,

    initialize = function(win = NA, loss = NA, cards = NA,
                          n.trials = NA, n.decks = NA, scale.factor = NA) {

        init <- IGTInitialize(win = NA, loss = NA, cards = NA,
                              n.trials = NA, n.decks = NA, scale.factor = NA)

        self$win   <- init$win
        self$loss  <- init$loss
        self$cards <- init$cards
        self$n.trials <- init$n.trials
        self$n.decks  <- init$n.decks
        self$scale.factor <- init$scale.factor
    },

    numTrials = function() {
        return(self$n.trials)
    },

    numDecks = function() {
        return(self$n.decks)
    },

    getDecks = function() {
        decks <- list(win = self$win, loss = self$loss)
        return(decks)
    }

))
