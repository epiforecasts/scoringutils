# library(scoringutils)
# y = sim_true = true_values <- rpois(100, lambda = 1:100)
#
# dat = sim_estim = predictions <- replicate(5000, rpois(n = 100, lambda = 1:100))
#
#
# predictions = list(dat1 = replicate(5000, rpois(n = 100, lambda = 1:100)),
#                    dat2 = replicate(5000, rpois(n = 100, lambda = 1:100)))
#
#
# call = eval_forecasts(y, predictions)
#
#
# eval_forecasts_prob_int(true_values = y, predictions = predictions)
#
