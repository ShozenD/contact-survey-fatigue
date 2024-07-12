propose_beta <- function(iter, data, trace, likelihood, prior = NULL, proposal = NULL) {
  state <- trace$state

  for (j in 1:ncol(data$Z)) {
    state.new <- state

    state.new$beta[j] <- proposal(state$beta[j])
    log.acc.ratio <- lpmf(data, state.new) - lpmf(data, state)
    log.acc.ratio <- log.acc.ratio + prior(state.new$beta[j]) - prior(state$beta[j])
    if (log(runif(1)) < log.acc.ratio) {
      state$beta[j] <- state.new$beta[j]
      trace$accept$beta[j] <- trace$accept$beta[j] + 1
    }
  }

  trace$state <- state
  trace$trace$BETA[iter,] <- state$beta

  return(trace)
}
