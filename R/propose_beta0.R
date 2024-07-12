propose_beta0 <- function(iter, data, trace, likelihood, prior = NULL, proposal = NULL) {
  state <- trace$state
  state.new <- state

  state.new$b0 <- proposal(state$b0)
  log.acc.ratio <- lpmf(data, state.new) - lpmf(data, state)
  log.acc.ratio <- log.acc.ratio + prior(state.new$b0) - prior(state$b0)

  if (log(runif(1)) < log.acc.ratio) {
    state <- state.new
    trace$accept$b0 <- trace$accept$b0 + 1
  }
  trace$state <- state
  trace$trace$BETA0[iter] <- state$b0

  return(trace)
}
