% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics-sample.R
\name{se_mean_sample}
\alias{se_mean_sample}
\title{Squared error of the mean (sample-based version)}
\usage{
se_mean_sample(observed, predicted)
}
\arguments{
\item{observed}{A vector with observed values of size n}

\item{predicted}{nxN matrix of predictive samples, n (number of rows) being
the number of data points and N (number of columns) the number of Monte
Carlo samples. Alternatively, \code{predicted} can just be a vector of size n.}
}
\description{
Squared error of the mean calculated as

\deqn{
  \textrm{mean}(\textrm{observed} - \textrm{mean prediction})^2
}{
  mean(observed - mean prediction)^2
}
The mean prediction is calculated as the mean of the predictive samples.
}
\section{Input format}{
\if{html}{
  \out{<div style="text-align: left">}
  \figure{metrics-sample.png}{options: style="width:750px;max-width:100\%;"}
  \out{</div>}
}
\if{latex}{
  \figure{metrics-sample.png}
}
}

\examples{
observed <- rnorm(30, mean = 1:30)
predicted_values <- matrix(rnorm(30, mean = 1:30))
se_mean_sample(observed, predicted_values)
}
\keyword{metric}
