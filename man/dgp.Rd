% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dgp.R
\name{dgp}
\alias{dgp}
\title{Simulation Data Generation}
\usage{
dgp(
  num_event,
  num_site,
  tau = 1,
  phi_S2S = 1,
  phi_SS = 1,
  path_term = FALSE,
  phi_P2P = NULL,
  num_event_cluster = NULL,
  num_site_cluster = NULL,
  missing_percent = 0
)
}
\arguments{
\item{num_event}{The integer to specify the number of earthquake events.}

\item{num_site}{The integer to specify the number of earthquake station sites.}

\item{tau}{The numeric value to specify the standard deviation of event terms.}

\item{phi_S2S}{The numeric value specify the standard deviation of site terms.}

\item{phi_SS}{The numeric value specify the standard deviation of single station within event residuals.}

\item{path_term}{A logical indicator for whether to generate path term.}

\item{phi_P2P}{The numeric value to specify the standard deviation of path terms.}

\item{num_event_cluster}{The integer to specify the number of earthquakes event clusters.}

\item{num_site_cluster}{The integer to specify the number of earthquakes station site clusters.}

\item{missing_percent}{A decimal between 0 and 1, indicating the percentage of
missingness generated by missing completely at random(MCAR).}
}
\value{
A data frame containing the full simulation dataset.
}
\description{
Generate simulation data of ground motion residuals with event, site, and single station within event residuals.
}
\examples{
gm_data <- dgp(num_event = 10, num_site = 20,
               tau = 1, phi_S2S = 1, phi_SS = 1)
}
