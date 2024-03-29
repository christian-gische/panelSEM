% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_data.R
\name{simulate_data}
\alias{simulate_data}
\title{simulate_data}
\usage{
simulate_data(
  N,
  time_points = 4,
  c_x1_etax,
  c_y1_etax,
  c_x1_etay,
  c_y1_etay,
  c_x_etax = 1,
  c_y_etay = 1,
  c_x1_z1,
  c_x1_z2,
  c_x1_z3,
  c_y1_z1,
  c_y1_z2,
  c_y1_z3,
  c_x_z1,
  c_x_z2,
  c_y_z2,
  c_y_z3,
  c_x_x,
  c_x_y,
  c_y_x,
  c_y_y,
  psi_etax_etax,
  psi_etax_etay,
  psi_etay_etay,
  psi_z1_z1,
  psi_z1_z2,
  psi_z1_z3,
  psi_z2_z2,
  psi_z2_z3,
  psi_z3_z3,
  psi_x1_x1,
  psi_x1_y1,
  psi_y1_y1,
  psi_x_x,
  psi_y_y,
  verbose = 0
)
}
\arguments{
\item{N}{Integer number reflecting number of units (e.g. persons).}

\item{time_points}{Integer number reflecting number of measurement occasions.}

\item{c_x1_etax}{Numeric parameter value.}

\item{c_y1_etax}{Numeric parameter value.}

\item{c_x1_etay}{Numeric parameter value.}

\item{c_y1_etay}{Numeric parameter value.}

\item{c_x_etax}{Numeric parameter value.}

\item{c_y_etay}{Numeric parameter value.}

\item{c_x1_z1}{Numeric parameter value.}

\item{c_x1_z2}{Numeric parameter value.}

\item{c_x1_z3}{Numeric parameter value.}

\item{c_y1_z1}{Numeric parameter value.}

\item{c_y1_z2}{Numeric parameter value.}

\item{c_y1_z3}{Numeric parameter value.}

\item{c_x_z1}{Numeric parameter value.}

\item{c_x_z2}{Numeric parameter value.}

\item{c_y_z2}{Numeric parameter value.}

\item{c_y_z3}{Numeric parameter value.}

\item{c_x_x}{Numeric parameter value.}

\item{c_x_y}{Numeric parameter value.}

\item{c_y_x}{Numeric parameter value.}

\item{c_y_y}{Numeric parameter value.}

\item{psi_etax_etax}{Numeric parameter value.}

\item{psi_etax_etay}{Numeric parameter value.}

\item{psi_etay_etay}{Numeric parameter value.}

\item{psi_z1_z1}{Numeric parameter value.}

\item{psi_z1_z2}{Numeric parameter value.}

\item{psi_z1_z3}{Numeric parameter value.}

\item{psi_z2_z2}{Numeric parameter value.}

\item{psi_z2_z3}{Numeric parameter value.}

\item{psi_z3_z3}{Numeric parameter value.}

\item{psi_x1_x1}{Numeric parameter value.}

\item{psi_x1_y1}{Numeric parameter value.}

\item{psi_y1_y1}{Numeric parameter value.}

\item{psi_x_x}{Numeric parameter value.}

\item{psi_y_y}{Numeric parameter value.}

\item{verbose}{print additional information?}
}
\value{
a data.frame of data of observed variables
}
\description{
create data.frame of observed variables
}
