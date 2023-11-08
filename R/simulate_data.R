# All functions in the following file are used to simulate data sets
# that are used in the examples and tests.

## Changelog:
# JO 0.0.1 2022-12-31 initial programming
# JO 0.0.2 2023-02-13 tested with different numbers of measurement occasions

## Documentation
#' @title simulate_data
#' @description create data.frame of observed variables
#' @param N Integer number reflecting number of units (e.g. persons).
#' @param time_points Integer number reflecting number of measurement occasions.
#' @param c_x1_etax Numeric parameter value.
#' @param c_y1_etax Numeric parameter value.
#' @param c_x1_etay Numeric parameter value.
#' @param c_y1_etay Numeric parameter value.
#' @param c_x_etax Numeric parameter value.
#' @param c_y_etay Numeric parameter value.
#' @param c_x1_z1 Numeric parameter value.
#' @param c_x1_z2 Numeric parameter value.
#' @param c_x1_z3 Numeric parameter value.
#' @param c_y1_z1 Numeric parameter value.
#' @param c_y1_z2 Numeric parameter value.
#' @param c_y1_z3 Numeric parameter value.
#' @param c_x_z1 Numeric parameter value.
#' @param c_x_z2 Numeric parameter value.
#' @param c_y_z2 Numeric parameter value.
#' @param c_y_z3 Numeric parameter value.
#' @param c_x_x Numeric parameter value.
#' @param c_x_y Numeric parameter value.
#' @param c_y_x Numeric parameter value.
#' @param c_y_y Numeric parameter value.
#' @param psi_etax_etax Numeric parameter value.
#' @param psi_etax_etay Numeric parameter value.
#' @param psi_etay_etay Numeric parameter value.
#' @param psi_z1_z1 Numeric parameter value.
#' @param psi_z1_z2 Numeric parameter value.
#' @param psi_z1_z3 Numeric parameter value.
#' @param psi_z2_z2 Numeric parameter value.
#' @param psi_z2_z3 Numeric parameter value.
#' @param psi_z3_z3 Numeric parameter value.
#' @param psi_x1_x1 Numeric parameter value.
#' @param psi_x1_y1 Numeric parameter value.
#' @param psi_y1_y1 Numeric parameter value.
#' @param psi_x_x Numeric parameter value.
#' @param psi_y_y Numeric parameter value.
#' @param verbose print additional information?
#' @return a data.frame of data of observed variables
#' @export
simulate_data <- function(
    # sample size and time points
  N,
  time_points = 4,

  # Directed effects
  ## effects of latent traits etax and etay
  c_x1_etax,
  c_y1_etax,
  c_x1_etay,
  c_y1_etay,

  c_x_etax = 1,
  c_y_etay = 1,

  ## effects of observed predictors z1, z2, and z3
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

  ## autoregressive and cross-lagged effects
  c_x_x,
  c_x_y,
  c_y_x,
  c_y_y,

  # undirected effects
  ## trait
  psi_etax_etax,
  psi_etax_etay,
  psi_etay_etay,

  ## observed predictors
  psi_z1_z1,
  psi_z1_z2,
  psi_z1_z3,
  psi_z2_z2,
  psi_z2_z3,
  psi_z3_z3,

  ## residuals
  ### initial time point
  psi_x1_x1,
  psi_x1_y1,
  psi_y1_y1,
  ### subsequent time points
  psi_x_x,
  psi_y_y,
  verbose = 0
){

  # print console output
  if(verbose >= 2) logger::log_info('Start.')

  ########################
  ## initialize data set
  ########################

  data <- data.frame(matrix(
    NA, nrow = N, ncol = 2*time_points + 3,
    dimnames = list(NULL,
                    c(paste0(c("x", "y"), rep(1:time_points, each = 2)),
                      paste0("z", 1:3)))
  ))

  #####################################################################
  # Time-independent predictors
  # epsilon_z1, epsilon_z2, epsilon_z3, epsilon_etax, epsilon_etay
  #####################################################################

  ## Random intercepts

  mu_epsilon_eta <- rep(0, 2)
  sigma_epsilon_eta <- matrix(c(psi_etax_etax, psi_etax_etay,
                                psi_etax_etay, psi_etay_etay),
                              nrow = 2, ncol = 2, byrow = TRUE)

  eta <- data.frame(mvtnorm::rmvnorm(n = N,
                                     mean = mu_epsilon_eta,
                                     sigma = sigma_epsilon_eta))
  colnames(eta) <- paste0("eta", c("x", "y"))

  ## Time invariant predictors
  mu_epsilon_z <- rep(0, 3)
  sigma_epsilon_z <- matrix(c(psi_z1_z1, psi_z1_z2, psi_z1_z3,
                              psi_z1_z2, psi_z2_z2, psi_z2_z3,
                              psi_z1_z3, psi_z2_z3, psi_z3_z3),
                            nrow = 3, ncol = 3, byrow = TRUE)

  z <- data.frame(mvtnorm::rmvnorm(n = N,
                                   mean = mu_epsilon_z,
                                   sigma = sigma_epsilon_z))
  colnames(z) <- paste0("z", 1:3)
  data$z1 <- z$z1
  data$z2 <- z$z2
  data$z3 <- z$z3

  #####################################################################
  # Initial time point
  #####################################################################
  mu_epsilon_init <- c(0,0)
  sigma_epsilon_init <- matrix(c(psi_x1_x1, psi_x1_y1,
                                 psi_x1_y1, psi_y1_y1),
                               nrow = 2, ncol = 2, byrow = TRUE)

  epsilon_init <- data.frame(mvtnorm::rmvnorm(n = N,
                                              mean = mu_epsilon_init,
                                              sigma = sigma_epsilon_init))
  colnames(epsilon_init) <- c("eps_init_x", "eps_init_y")

  # simulate observed variables of first time point

  data$x1 <- c_x1_etax  * eta$etax + c_x1_etay * eta$etay +
    c_x1_z1 * z$z1 + c_x1_z2 * z$z2 + c_x1_z3 * z$z3 +
    epsilon_init$eps_init_x
  data$y1 <- c_y1_etax  * eta$etax + c_y1_etay * eta$etay +
    c_y1_z1 * z$z1 + c_y1_z2 * z$z2 + c_y1_z3 * z$z3 +
    epsilon_init$eps_init_y

  #####################################################################
  # Subsequent time points
  #####################################################################

  # covariance matrix of the epsilon-variables of (non-initial) time-varying
  # variables

  mu_epsilon_time_varying <- rep(0,2)
  sigma_epsilon_time_varying <- matrix(c(psi_x_x,0,
                                         0, psi_y_y),
                                       nrow = 2, ncol = 2, byrow = TRUE)

  for(tp in 2:time_points){

    residuals <- mvtnorm::rmvnorm(n = N,
                                  mean = mu_epsilon_time_varying,
                                  sigma = sigma_epsilon_time_varying)

    data[,paste0("x",tp)] <- c_x_etax * eta$etax +
      c_x_z1 * z$z1 + c_x_z2 * z$z2 +
      c_x_x * data[,paste0("x",tp-1)] + c_x_y * data[,paste0("y",tp-1)] +
      residuals[,1]

    data[,paste0("y",tp)] <- c_y_etay * eta$etay +
      c_y_z2 * z$z2 + c_y_z3 * z$z3 +
      c_y_x * data[,paste0("x",tp-1)] + c_y_y * data[,paste0("y",tp-1)] +
      residuals[,2]

  }

  # print console output
  if(verbose >= 2) logger::log_info('End.')

  return(data)
}
