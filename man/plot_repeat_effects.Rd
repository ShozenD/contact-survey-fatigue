% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_repeat_effects.R
\name{plot_repeat_effects}
\alias{plot_repeat_effects}
\title{Plot the posterior distribution of the repeat effects from the variable selection model}
\usage{
plot_repeat_effects(fit, stan_data, config, outdir = NA)
}
\arguments{
\item{fit}{A CmdStanR fit object}

\item{stan_data}{Stan data used to fit the model}

\item{config}{Configuration list}

\item{outdir}{Output directory}
}
\value{
A ggplot object
}
\description{
Plot the posterior distribution of the repeat effects from the variable selection model
}
