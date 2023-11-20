test_that("test linear model", {
  library(panelSEM)
  library(mxsem)
  library(testthat)
  set.seed(23)

  time_points <- 5

  ############################################
  # set values of population parameters SET 1
  ############################################

  # covariance-matrix of the latent traits
  A_sigma_eta <- matrix(nrow = 2, ncol = 2, c(1, 0.2, 0.2, 1))
  sigma_epsilon_eta <- t(A_sigma_eta) %*% A_sigma_eta

  # covariance-matrix of the epsilon_z variables
  A_sigma_z <- matrix(nrow = 3, ncol = 3, c(1, 0.15, 0.03, 0.15, 1, 0.1,
                                            0.03, 0.1, 1))
  sigma_epsilon_z <- t(A_sigma_z) %*% A_sigma_z

  # covariance matrix of the initial residuals
  A_sigma_eps_init <- matrix(nrow = 2, ncol = 2, c(1, .1, .1, 1))
  sigma_epsilon_init <- t(A_sigma_eps_init) %*% A_sigma_eps_init

  population_parameters <- data.frame(
    N = NA,
    # directed effects
    ## latent traits
    c_x1_etax = -.8,
    c_x1_etay = .5,

    c_y1_etax = -.4,
    c_y1_etay = .19,

    c_x_etax = 1,
    c_y_etay = 1,

    ## time independent predictors
    c_x1_z1 = -.5,
    c_x1_z2 = -.1,
    c_x1_z3 = .4,

    c_y1_z1 = -.8,
    c_y1_z2 = .2,
    c_y1_z3 = .6,

    c_x_z1 = .5,
    c_x_z2 = .2,

    c_y_z2 = .15,
    c_y_z3 = .2,

    ## autoregressive and cross-lagged effects
    c_x_x = 0.35,
    c_x_y = 0.1,
    c_y_x = -0.2,
    c_y_y = .7,

    # undirected effects
    ## trait
    psi_etax_etax = sigma_epsilon_eta[1,1],
    psi_etax_etay = sigma_epsilon_eta[1,2],
    psi_etay_etay = sigma_epsilon_eta[2,2],

    ## observed predictors
    psi_z1_z1 = sigma_epsilon_z[1,1],
    psi_z1_z2 = sigma_epsilon_z[1,2],
    psi_z1_z3 = sigma_epsilon_z[1,3],
    psi_z2_z2 = sigma_epsilon_z[2,2],
    psi_z2_z3 = sigma_epsilon_z[2,3],
    psi_z3_z3 = sigma_epsilon_z[3,3],

    ## residuals
    ### initial time point
    psi_x1_x1 = sigma_epsilon_init[1,1],
    psi_x1_y1 = sigma_epsilon_init[1,2],
    psi_y1_y1 = sigma_epsilon_init[2,2],

    ### subsequent time points
    psi_x_x = 1,
    psi_y_y = 1
  )

  population_parameters$N <- 50

  population_parameters$time_points <- time_points

  data <- do.call(what = simulate_data,
                  args = population_parameters)

  for(linear in c(TRUE, FALSE)){
    for(heterogeneity in list("homogeneous",
                              "additive",
                              "cross-lagged",
                              c("additive", "cross-lagged"))){
      for(use_open_mx in c(TRUE)){

        message("Testing:\n linear = ", linear, "\n heterogeneity = ", heterogeneity, "\n use_open_mx = ", use_open_mx)

        model <- fit_panel_sem(data = data,
                               time_varying_variables = list(paste0("x", 1:time_points),
                                                             paste0("y", 1:time_points)),
                               time_invariant_variables = list(c("z1", "z2"),
                                                               c("z2", "z3")),
                               use_open_mx = use_open_mx,
                               heterogeneity = heterogeneity,
                               lbound_variances = use_open_mx,
                               linear = linear)

        does_run <- try(OpenMx::mxRun(model = model$model,
                                      useOptimizer = FALSE))
        if(is(does_run, "try-error") && !(grepl("fit is not finite", attr(does_run, "condition"))))
          stop("Model does not run")

        coef_fit <- model$model |>
          coef() |>
          names() |>
          unique() |>
          sort()

        if(all(heterogeneity == "homogeneous")){
          warning("Skipping heterogeneity = 'homogeneous' tests because the",
          " comparison model is currently not working correctly")
          next
        }

        # remove intercepts
        coef_fit <- coef_fit[!grepl(pattern = "^intercept_",
                                    x = coef_fit)]

        expected_model <- ""

        if(linear){
          expected_model <- paste0(
            expected_model,
            "\n# exogenous predictors
  x1 ~ c_x1_z1 * z1
  x1 ~ c_x1_z2 * z2
  x1 ~ c_x1_z3 * z3

  y1 ~ c_y1_z1 * z1
  y1 ~ c_y1_z2 * z2
  y1 ~ c_y1_z3 * z3

  x2 ~ c_x_z1 * z1 + c_x_z2 * z2
  x3 ~ c_x_z1 * z1 + c_x_z2 * z2
  x4 ~ c_x_z1 * z1 + c_x_z2 * z2
  x5 ~ c_x_z1 * z1 + c_x_z2 * z2

  y2 ~ c_y_z2 * z2 + c_y_z3 * z3
  y3 ~ c_y_z2 * z2 + c_y_z3 * z3
  y4 ~ c_y_z2 * z2 + c_y_z3 * z3
  y5 ~ c_y_z2 * z2 + c_y_z3 * z3

  z1 ~~ psi_z1_z1*z1 + psi_z2_z1*z2 + psi_z3_z1*z3
  z2 ~~ psi_z2_z2*z2 + psi_z3_z2*z3
  z3 ~~ psi_z3_z3*z3
        "
          )
        }else{
          expected_model <- paste0(
            expected_model,
            "\n# exogenous predictors
  x1 ~ c_x1_z1 * z1
  x1 ~ c_x1_z2 * z2
  x1 ~ c_x1_z3 * z3

  y1 ~ c_y1_z1 * z1
  y1 ~ c_y1_z2 * z2
  y1 ~ c_y1_z3 * z3

  x2 ~ {c_x_z1_t2 := c_x_z1 + c_x_y_z1 * data.y1}*z1 + {c_x_z2_t2 := c_x_z2 + c_x_y_z2 * data.y1}*z2
  x3 ~ {c_x_z1_t3 := c_x_z1 + c_x_y_z1 * data.y2}*z1 + {c_x_z2_t3 := c_x_z2 + c_x_y_z2 * data.y2}*z2
  x4 ~ {c_x_z1_t4 := c_x_z1 + c_x_y_z1 * data.y3}*z1 + {c_x_z2_t4 := c_x_z2 + c_x_y_z2 * data.y3}*z2
  x5 ~ {c_x_z1_t5 := c_x_z1 + c_x_y_z1 * data.y4}*z1 + {c_x_z2_t5 := c_x_z2 + c_x_y_z2 * data.y4}*z2

  y2 ~ {c_y_z2_t2 := c_y_z2 + c_y_x_z2 * data.x1}*z2 + {c_y_z3_t2 := c_y_z3 + c_y_x_z3 * data.x1}*z3
  y3 ~ {c_y_z2_t3 := c_y_z2 + c_y_x_z2 * data.x2}*z2 + {c_y_z3_t3 := c_y_z3 + c_y_x_z3 * data.x2}*z3
  y4 ~ {c_y_z2_t4 := c_y_z2 + c_y_x_z2 * data.x3}*z2 + {c_y_z3_t4 := c_y_z3 + c_y_x_z3 * data.x3}*z3
  y5 ~ {c_y_z2_t5 := c_y_z2 + c_y_x_z2 * data.x4}*z2 + {c_y_z3_t5 := c_y_z3 + c_y_x_z3 * data.x4}*z3

  z1 ~~ psi_z1_z1*z1 + psi_z2_z1*z2 + psi_z3_z1*z3
  z2 ~~ psi_z2_z2*z2 + psi_z3_z2*z3
  z3 ~~ psi_z3_z3*z3
        "
          )
        }

        if("additive" %in% heterogeneity){
          expected_model <- paste0(
            expected_model,
            "\n# additive latent predictors
  etax =~ c_x1_etax * x1 + c_y1_etax * y1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
  etay =~ c_x1_etay * x1 + c_y1_etay * y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5

  etax ~~ psi_etax_etax * etax + psi_etay_etax * etay
  etay ~~ psi_etay_etay * etay
        "
          )
        }
        if("cross-lagged" %in% heterogeneity){
          expected_model <- paste0(
            expected_model,
            "\n# cross-lagged latent predictors
  etaxy =~ c_x1_etaxy*x1 + c_y1_etaxy*y1 + data.y1 * x2 + data.y2 * x3 + data.y3 * x4 + data.y4 * x5
  etayx =~ c_x1_etayx*x1 + c_y1_etayx*y1 + data.x1 * y2 + data.x2 * y3 + data.x3 * y4 + data.x4 * y5

  etaxy ~~ psi_etaxy_etaxy * etaxy + psi_etayx_etaxy * etayx
  etayx ~~ psi_etayx_etayx * etayx
        "
          )
        }

        if("additive" %in% heterogeneity &&
           "cross-lagged" %in% heterogeneity
        ){
          # add covariances
          expected_model <- paste0(
            expected_model,
            "\n# (co-)variances
  etaxy =~ data.y1 * x2 + data.y2 * x3 + data.y3 * x4 + data.y4 * x5
  etayx =~ data.x1 * y2 + data.x2 * y3 + data.x3 * y4 + data.x4 * y5

  etaxy ~~ psi_etaxy_etax * etax + psi_etaxy_etay * etay
  etayx ~~ psi_etayx_etax * etax + psi_etayx_etay * etay
        "
          )
        }

        if(all(heterogeneity == "homogeneous")){
          expected_model <- paste0(
            expected_model,
            "\n# homogeneity (co-) variances
  x2 ~~ psi_x1_x2 * x1
  x3 ~~ psi_x2_x3 * x2
  x4 ~~ psi_x3_x4 * x3
  x5 ~~ psi_x4_x5 * x4

  y2 ~~ psi_y1_y2 * y1
  y3 ~~ psi_y2_y3 * y2
  y4 ~~ psi_y3_y4 * y3
  y5 ~~ psi_y4_y5 * y4

  y2 ~~ psi_y_x * x2
  y3 ~~ psi_y_x * x3
  y4 ~~ psi_y_x * x4
  y5 ~~ psi_y_x * x5
        "
          )
        }

        expected_model <- paste0(
          expected_model,
          "\n# autoregressive and cross-lagged effects
  x2 ~ c_x_x*x1 + c_x_y*y1
  x3 ~ c_x_x*x2 + c_x_y*y2
  x4 ~ c_x_x*x3 + c_x_y*y3
  x5 ~ c_x_x*x4 + c_x_y*y4

  y2 ~ c_y_x*x1 + c_y_y*y1
  y3 ~ c_y_x*x2 + c_y_y*y2
  y4 ~ c_y_x*x3 + c_y_y*y3
  y5 ~ c_y_x*x4 + c_y_y*y4

# (co-)variances
  x1 ~~ psi_x1_x1*x1 + psi_y1_x1*y1
  y1 ~~ psi_y1_y1*y1

  x2 ~~ psi_x_x*x2
  x3 ~~ psi_x_x*x3
  x4 ~~ psi_x_x*x4
  x5 ~~ psi_x_x*x5
  y2 ~~ psi_y_y*y2
  y3 ~~ psi_y_y*y3
  y4 ~~ psi_y_y*y4
  y5 ~~ psi_y_y*y5
      "
        )

        fit_mxsem_exp <- mxsem::mxsem(expected_model,
                                      data = model$info_data$data,
                                      scale_loadings = FALSE,
                                      scale_latent_variances = FALSE)

        coef_exp <- fit_mxsem_exp |>
          coef() |>
          names() |>
          unique() |>
          sort()
        # remove intercepts and (co-)variances of exogenous predictors as the
        # latter are fixed to the observed values in panelSEM.
        coef_exp <- coef_exp[!grepl("^one", coef_exp) & !grepl("^psi_z[0-9]_z[0-9]", coef_exp)]

        testthat::expect_true(all(coef_fit == coef_exp))
      }

    }
  }
})
