# =============================================================================
# Script to analyse following hypothesis:
# Hypothesis: Lm on correlated features has positive bias in PFI due to
#             increase in model variance in extrapolated areas
# =============================================================================
set.seed(1)
devtools::load_all()

# Data size for PFI estiation
n_test = 10000
# Data size for training the linear model
n_train = 100
# Number of simulations
nsim = 100
# Number of models per simulation
nmodels = 50


simulate_problem = function(dgp){
res = lapply(1:nsim, function(sim){
  # The test data is generated only once per simulation
  test_dat = dgp$generate(n_test)
  features = setdiff(colnames(test_dat), "y")
  shuffled = lapply(features, function(fname){
    test_shuffle = test_dat
    test_shuffle[,fname] = sample(test_shuffle[,fname])
    test_shuffle
   })
  names(shuffled) = features

  res = lapply(1:nmodels, function(r) {
    # Training data is sampled for each model
    train_dat = dgp$generate(n_train)
    mod = lm(y ~ ., data = train_dat)
    pred = predict(mod, test_dat)
    res = data.frame(i = 1:nrow(test_dat),
                     prediction = pred,
                     repetition = r)
    shuffled = lapply(features, function(fname){pred2 = predict(mod, shuffled[[fname]])
  })
  shuffled = data.frame(shuffled)
  names(shuffled) = features
  cbind(res, shuffled)
})

res = rbindlist(res)
res2 = res %>%
  dplyr::group_by(i) %>%
  # 'prediction' is for E_X[V[f]] and feature names for E_{\tilde{X}}[V[f]] with according feature permuted.
  # this computes the variance across the models
  dplyr::summarize(across(c(prediction, X1, X2, X3, X4, X5), .fns = var)) %>%
  select(-i) %>%
  # This computes the expectation across the data
  dplyr::summarize(across(.fns = mean))

v_inflation = res2[c(sprintf("X%i", 1:5))] - res2$prediction
colnames(v_inflation) = sprintf("VI_%s", colnames(v_inflation))
print(v_inflation)
v_inflation
})
res = rbindlist(res)
res
}

s1 = simulate_problem(dgps_list$linearc)
s2 = simulate_problem(dgps_list$linear)


# Variance Inflation mean
print(mean(unlist(lapply(s1, mean))))
# 0.3307201
# variance of VI estimate
print(sd(apply(s1, 1,  mean)))
# 0.07097552

# Variance Inflation mean
print(mean(unlist(lapply(s2, mean))))
#  1.615855e-06

# variance of VI estimate
print(sd(apply(s2, 1,  mean)))
#  5.786476e-05


# Results:
# Variance inflated for mainly X3 and X5 which are negatively correlated
# Slight increase for X1 and X2 which are positively correlated
# X4 which is independent is not inflated
