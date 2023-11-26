if (FALSE) {

  #################
  # nnet::multinom
  #################
  ffit <- nnet::multinom(
    state ~ age + lagstate + sex,
    data = gradu::health
  )

  ffit_struct <- gradu::struct(
    ffit,
    state = "lagstate",
    x = list(name = "age", values = 3:59),
    #fixed_predictors = list(sex = "male")
    group = list(name = "sex", values = c("male", "female"))
  )

  plot(ffit_struct)

  #################
  # dynamite
  #################
  bfit <- dynamite::dynamite(
    dynamite::obs(state ~ -1 + varying(~ -1 + lag(state)) + sex, "categorical") +
      dynamite::splines(df = 10),
    data = gradu::health,
    time = "age",
    group = "id",
    backend = "cmdstanr",
    chains = 1,
    refresh = 0,
    thin = 5,
    seed = 1
  )

  save(bfit, file = "bfit.RData")
  load(file = "bfit.RData")

  bfit_struct <- gradu::struct(
    bfit,
    state = "state",
    #fixed_predictors = list(sex = "male")
    group = list(name = "sex", values = c("male", "female"))
  )

  plot(bfit_struct)

  #################
  # effects::Effect
  #################
  ffit_eff <- effects::Effect(
    c("age", "lagstate", "sex"),
    ffit,
    xlevels = list(
      age = 0:100))

  plot(ffit_eff)
}
