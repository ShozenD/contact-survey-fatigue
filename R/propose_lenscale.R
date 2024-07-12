propose_lenscale <- function(iter, data, trace, likelihood, prior = NULL, proposal = NULL) {
  state <- trace$state
  state.new <- state

  state.new$lenscale <- proposal(state$lenscale)
  log.acc.ratio <- lpmf(data, state.new) - lpmf(data, state) + prior(state.new$lenscale) - prior(state$lenscale)
  if (log(runif(1)) < log.acc.ratio) {
    state <- state.new
    trace$accept$lenscale <- trace$accept$lenscale + 1
  }
  trace$state <- state
  trace$trace$LENSCALE[iter] <- state$lenscale

  return(trace)
}
