ffit <- nnet::multinom(state ~ age + lagstate, data = health)
plot(effects::Effect(c("age", "lagstate"), ffit))
