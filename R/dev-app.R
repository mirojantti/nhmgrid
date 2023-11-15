if (FALSE) {

  ffit <- nnet::multinom(
    state ~ age + lagstate + sex,
    data = gradu::health)

  ffit_struct <- gradu::struct(
    ffit,
    state_name = "lagstate",
    x = list(name = "age", values = 0:100),
    #fixed_predictors = list(sex = "male"))
    group = list(name = "sex", values = c("male", "female")))

  ffit_eff <- effects::Effect(
    c("age", "lagstate", "sex"),
    ffit,
    xlevels = list(
      age = 0:100))

  plot(ffit_eff)
}
