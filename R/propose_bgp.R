propose_bgp <- function(iter, data, trace, likelihood, prior = NULL, proposal = NULL) {
  state <- trace$state
  state.new <- state

  state.new$bgp <- proposal(state$bgp)
  log.acc.ratio <- lpmf(data, state.new) - lpmf(data, state) + prior(state.new$bgp) - prior(state$bgp)
  if (log(runif(1)) < log.acc.ratio) {
    state <- state.new
    trace$accept$bgp <- trace$accept$bgp + 1
  }
  trace$state <- state
  trace$trace$BGP[iter,] <- state$bgp

  return(trace)
}
