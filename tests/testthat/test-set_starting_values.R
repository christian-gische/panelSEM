test_that("changing starting values - openmx", {
  set.seed(123)
  # Example copied from ?OpenMx::mxRun
  library(OpenMx)
  data(demoOneFactor)

  manifests <- names(demoOneFactor)
  latents   <- c("G")

  model <- mxModel(type="RAM",
                   manifestVars = names(demoOneFactor),
                   latentVars   = "G",
                   mxPath(from=latents, to=manifests, labels=paste("b", 1:5, sep="")),
                   mxPath(from=manifests, arrows=2, labels=paste("u", 1:5, sep="")),
                   mxPath(from=latents  , arrows=2, free=FALSE, values=1.0),
                   mxData(cov(demoOneFactor), type="cov", numObs=500))
  model <- mxRun(model)

  # get the current parameter values
  starting_values <- omxGetParameters(model)
  # change the values
  starting_values[] <- rnorm(length(starting_values))^2

  # set starting values
  model_start <- set_starting_values_openmx(model           = model,
                                            starting_values = starting_values)

  testthat::expect_true(all(omxGetParameters(model_start) == starting_values))

  # Let's check if this also works if we only supply starting values for a subset
  # of the parameters:
  starting_values <- starting_values[1:4]
  # change the values
  starting_values[] <- rnorm(length(starting_values))^2

  # set starting values
  model_start <- set_starting_values_openmx(model           = model,
                                            starting_values = starting_values)

  testthat::expect_true(all(omxGetParameters(model_start)[names(starting_values)] == starting_values))

  # and check if passing a wrong parameter label results in an error:
  starting_values <- c(starting_values,
                       "hjshdf" = 2)
  testthat::expect_error(model_start <- set_starting_values_openmx(model           = model,
                                                                   starting_values = starting_values)
  )
})


test_that("changing starting values - lavaan", {
  set.seed(123)
  # Example copied from ?lavaan::sem
  library(lavaan)
  model <- '
    # latent variable definitions
       ind60 =~ x1 + x2 + x3
       dem60 =~ y1 + a*y2 + b*y3 + c*y4
       dem65 =~ y5 + a*y6 + b*y7 + c*y8

    # regressions
      dem60 ~ ind60
      dem65 ~ ind60 + dem60
  '

  fit <- sem(model, data = PoliticalDemocracy)

  # extract current estimates
  starting_values <- c(coef(fit))[unique(names(coef(fit)))]

  # change values
  starting_values[] <- runif(length(starting_values), min = 0, max = 1)

  suppressWarnings(
  model_start <- set_starting_values_lavaan(model           = model,
                                            starting_values = starting_values)
  )

  testthat::expect_true(
    all(model_start$ustart[model_start$label %in% names(starting_values)] ==
    starting_values[model_start$label[model_start$label %in% names(starting_values)]]))

  # Let's check if this also works if we only supply starting values for a subset
  # of the parameters:
  starting_values <- starting_values[1:4]
  # change the values
  starting_values[] <- rnorm(length(starting_values))^2

  # set starting values
  model_start <- set_starting_values_lavaan(model           = model,
                                            starting_values = starting_values)

  testthat::expect_true(
    all(model_start$ustart[model_start$label %in% names(starting_values)] ==
          starting_values[model_start$label[model_start$label %in% names(starting_values)]]))

  # and check if passing a wrong parameter label results in an error:
  starting_values <- c(starting_values,
                       "hjshdf" = 2)
  testthat::expect_error(model_start <- set_starting_values_lavaan(model           = model,
                                                                   starting_values = starting_values)
  )
})
