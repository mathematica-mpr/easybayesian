clustered_model  <- rstan::stan_model(model_code = createClusteredStanfile())

unclustere_model  <- rstan::stan_model(model_code = createStanfile())

devtools::use_data(clustered_model, unclustere_model, internal = T)
