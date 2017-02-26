clustered_model  <- rstan::stan_model(model_code = easybayesian::createClusteredStanfile())

unclustere_model  <- rstan::stan_model(model_code = easybayesian::createStanfile())

devtools::use_data(clustered_model, unclustere_model)
