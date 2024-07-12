#' Propose new gamma
#'
#' @param data A list of data
#' @param state A list of the current state of the variables
#' @param likelihood A function that calculates the likelihood
#' @param prior A function that calculates the prior
#' @param proposal A function that proposes a new state
#'
#' @return A list of the new state of the variables
#' @export
propose_gamma <- function(iter, data, trace, likelihood, prior = NULL, proposal = NULL) {
  state <- trace$state

  for (j in sample(1:ncol(data$Z))) {
    state.new <- state
    state.new$gamma[j] <- 1 - state.new$gamma[j]
    log.acc.ratio <- likelihood(data, state.new) - likelihood(data, state)
    if (log(runif(1)) < log.acc.ratio) {
      state$gamma[j] <- state.new$gamma[j]
    }
    trace$trace$GAMMA[iter,j] <- state$gamma[j]
  }
  trace$state <- state

  return(trace)
}
