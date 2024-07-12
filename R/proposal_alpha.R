propose_alpha <- function(iter, data, trace, likelihood, prior = NULL, proposal = NULL) {
  state <- trace$state
  state.new <- state

  state.new$alpha <- proposal(state$alpha)
  log.acc.ratio <- likelihood(data, state.new) - likelihood(data, state)
  log.acc.ratio <- log.acc.ratio + prior(state.new$alpha) - prior(state$alpha)
  if (log(runif(1)) < log.acc.ratio) {
    state$alpha <- state.new$alpha
    trace$accept$alpha <- trace$accept$alpha + 1
  }
  trace$state <- state
  trace$trace$ALPHA[iter,] <- state$alpha

  #for (j in 1:ncol(data$X)) {
  #  state.new <- state
#
  #  state.new$alpha[j] <- proposal(state$alpha[j])
  #  log.acc.ratio <- lpmf(data, state.new) - lpmf(data, state)
  #  log.acc.ratio <- log.acc.ratio + prior(state.new$alpha[j]) - prior(state$alpha[j])
  #  if (log(runif(1)) < log.acc.ratio) {
  #    state$alpha[j] <- state.new$alpha[j]
  #    trace$accept$alpha[j] <- trace$accept$alpha[j] + 1
  #  }
  #}

  return(trace)
}
