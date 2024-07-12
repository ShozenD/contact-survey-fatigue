propose_sigma <- function(iter, data, trace, likelihood, prior = NULL, proposal = NULL) {
  state <- trace$state
  state.new <- state

  state.new$sigma <- proposal(state$sigma)
  log.acc.ratio <- lpmf(data, state.new) - lpmf(data, state)
  log.acc.ratio <- log.acc.ratio + prior(state.new$sigma) - prior(state$sigma)

  if (log(runif(1)) < log.acc.ratio) {
    state <- state.new
    trace$accept$sigma <- trace$accept$sigma + 1
  }
  trace$state <- state
  trace$trace$SIGMA[iter] <- state$sigma

  return(trace)
}
