#' Import IGT deck structure
#'
#' Import one of the standard IGT deck structures included in the iowa package
#'
#' @param deck The name of an included deck (see details)
#' @details iowa includes several standard IGT deck structures, in addition to the ability
#' for the user to construct their own. All decks are named lists with fields:
#'
#' \itemize{
#'   \item win: A numTrial x numDeck matrix of gains
#'   \item loss: A numTrial x numDeck matrix of losses
#' }
#'
#' Recall that, in the classic administration of the IGT, subjects experience
#' both a gain and a loss when selecting a card. In cases where the subject sees
#' only a net outcome (e.g. as in the prospect utility function), the net gain is
#' computed internal as win - loss, and so the user can simply place the net outcome
#' in the "win" slot, and specify the "loss" slot to be a matrix of zeros.
#' Included decks are identified by a character string:
#' \itemize{
#'   \item bechara: The standard deck structure used by Bechara et al. (1994).
#'   Note that the authors used decks of 40 cards, while the decks provided here
#'   contains 100 cards, consisting of 2.5 "copies" of each deck.
#'   \item becharavar: A variant of the Bechara deck structure in which the loss of
#'   deck C is fixed at 50, rather than varying between (25, 50, 75).
#'   \item soochow: The alternative deck structure used by Chiu et al. (2008) in
#'   which the win/loss frequency is symmetric across pairs of good and bad decks.
#' }
#' @return A named list with fields "win" and "loss".
#' @export

importDeck <- function(deck) {
    if (!is.character(deck))
        stop('deck must be a character string')

    deckPath <- system.file(paste0('extdata/decks/deck_', deck, '.rds'), package = "iowa")
    deck <- readRDS(deckPath)
    return(deck)
}
