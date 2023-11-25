library(data.table)

#------------------------------------------------------------------------------#
#---------------------- Metrics Summary and Overview --------------------------#
#------------------------------------------------------------------------------#

ae <- list(
  `Metric` = "Absolute error",
  `Name` = list("ae_point", "ae_median"),
  `Functions` = r"(score(), ae_point()), ae_median_sample()",
  `D` = r"($\checkmark$)",
  `C` = r"($\checkmark$)",
  `B` = r"($-$)",
  `Q` = r"($\checkmark$)",
  `Properties` = "Suitable for scoring the median of a predictive distribution",
  `References` = ""
)

se <- list(
  `Metric` = "Squared error",
  `Name` = list("se_point", "se_mean"),
  `Functions` = r"(score(), se_point(), se_mean_sample())",
  `D` = r"($\checkmark$)",
  `C` = r"($\checkmark$)",
  `B` = r"($-$)",
  `Q` = r"($\checkmark$)",
  `Properties` = "Suitable for scoring the mean of a predictive distribution.",
  `References` = ""
)


crps <- list(
  `Metric` = "(Continuous) ranked probability score (CRPS)",
  `Name` = r"(crps)",
  `Functions` = r"(score(), ae_point)",
  `D` = r"($\checkmark$)",
  `C` = r"($\checkmark$)",
  `B` = r"($-$)",
  `Q` = r"($-$)",
  `Properties` = "Proper scoring rule (smaller is better), takes entire predictive distribution into account (global), penalises over- and under-confidence similarly, stable handling of outliers",
  `References` = ""
)

log_score <- list(
  `Metric` = "Log score",
  `Name` = r"(log_score)",
  `Functions` = r"(score(), logs_sample(), logs_binary())",
  `D` = r"($-$)",
  `C` = r"($\checkmark$)",
  `B` = r"($\checkmark$)",
  `Q` = r"($-$)",
  `Properties` = "Proper scoring rule, smaller is better, equals negative log of the predictive density at observed value (local), penalises over-confidence severely, susceptible to outliers",
  `References` = ""
)

wis <- list(
  Metric = "(Weighted) interval score (WIS)",
  `Name` = r"(interval_score)",
  `Functions` = r"(score(), interval_score())",
  `D` = r"($\checkmark$)",
  `C` = r"($\checkmark$)",
  `B` = r"($-$)",
  `Q` = r"($\checkmark$)",
  `Properties` = "Proper scoring rule, smaller is better, similar properties to CRPS and converges to CRPS for an increasing number of equally spaced intervals",
  `References` = ""
)

dss <- list(
  `Metric` = "Dawid-Sebastiani score (DSS)",
  `Name` = r"(dss)",
  `Functions` = r"(score(), dss_sample())",
  `D` = r"($\checkmark$)",
  `C` = r"($\checkmark$)",
  `B` = r"($-$)",
  `Q` = r"($-$)",
  `Properties` = "Proper scoring rule, smaller is better, evaluates forecast based on mean and sd of predictive distribution (global), susceptible to outliers, penalises over-confidence severely",
  `References` = ""
)

brier_score <- list(
  `Metric` = "Brier score (BS)",
  `Name` = r"(brier_score)",
  `Functions` = r"(score(), brier_score())",
  `D` = r"($-$)",
  `C` = r"($-$)",
  `B` = r"($\checkmark$)",
  `Q` = r"($-$)",
  `Properties` = "Proper scoring rule, smaller is better, equals CRPS for binary outcomes, penalises over- and under-confidence similarly",
  `References` = ""
)

interval_coverage <- list(
  `Metric` = "Interval coverage",
  `Name` = r"(coverage)",
  `Functions` = r"(score())",
  `D` = r"($-$)",
  `C` = r"($-$)",
  `B` = r"($-$)",
  `Q` = r"($\checkmark$)",
  `Properties` = "Proportion of observations falling inside a given central prediction interval (= 'empirical interval coverage'). Used to assess probabilistic calibration." ,
  `References` = ""
)

coverage_deviation <- list(
  `Metric` = "Coverage deviation",
  `Name` = r"(coverage_deviation)",
  `Functions` = r"(score())",
  `D` = r"($-$)",
  `C` = r"($-$)",
  `B` = r"($-$)",
  `Q` = r"($\checkmark$)",
  `Properties` = "Average difference between empirical and nominal interval coverage (coverage that should have been realised)",
  `References` = ""
)

quantile_coverage <- list(
  `Metric` = "Quantile coverage",
  `Name` = r"(quantile_coverage)",
  `Functions` = r"(score())",
  `D` = r"($\checkmark$)",
  `C` = r"($\checkmark$)",
  `B` = r"($-$)",
  `Q` = r"($-$)",
  `Properties` = "Proportion of observations below a given quantile of the predictive CDF. Used to assess probabilistic calibration.",
  `References` = ""
)


dispersion <- list(
  `Metric` = "Dispersion",
  `Name` = r"(dispersion)",
  `Functions` = r"(score(), interval_score())",
  `D` = r"($-$)",
  `C` = r"($-$)",
  `B` = r"($-$)",
  `Q` = r"($\checkmark$)",
  `Properties` = "Dispersion component of WIS, measures width of predictive intervals.",
  `References` = ""
)

mad <- list(
  `Metric` = "Median Absolute Deviation (Dispersion)",
  `Name` = r"(mad)",
  `Functions` = r"(score(), mad_sample())",
  `D` = r"($\checkmark$)",
  `C` = r"($\checkmark$)",
  `B` = r"($-$)",
  `Q` = r"($-$)",
  `Properties` = "Measure for dispersion of a forecast: median of the absolute deviations from the median",
  `References` = ""
)

bias <- list(
  `Metric` = "Bias",
  `Name` = r"(bias)",
  `Functions` = r"(score(), bias_sample(), bias_quantile())",
  `D` = r"($\checkmark$)",
  `C` = r"($\checkmark$)",
  `B` = r"($-$)",
  `Q` = r"($\checkmark$)",
  `Properties` = "Measure of relative tendency to over- or under-predict (aspect of calibration), bounded between -1 and 1 (ideally 0)",
  `References` = ""
)

under_overprediction <- list(
  `Metric` = "Under-, Over-prediction",
  `Name` = list("underprediction", "overprediction"),
  `Functions` = r"(score(), interval_score())",
  `D` = r"($-$)",
  `C` = r"($-$)",
  `B` = r"($-$)",
  `Q` = r"($\checkmark$)",
  `Properties` = "Absolute amount of over-or under-prediction (components of WIS)",
  `References` = ""
)

pit <- list(
  `Metric` = "Probability integral transform (PIT)",
  `Name` = r"(crps)",
  `Functions` = r"(score(), pit())",
  `D` = r"($\checkmark$)",
  `C` = r"($\checkmark$)",
  `B` = r"($-$)",
  `Q` = r"($\checkmark$)",
  `Properties` = "PIT transform is the CDF of the predictive distribution evaluated at the observed values. PIT values should be uniform. ",
  `References` = ""
)

mean_score_ratio <- list(
  `Metric` = "Mean score ratio",
  `Name` = r"(mean_scores_ratio)",
  `Functions` = r"(pairwise_comparison())",
  `D` = r"($\sim$)",
  `C` = r"($\sim$)",
  `B` = r"($\sim$)",
  `Q` = r"($\sim$)",
  `Properties` = "Compares performance of two models. Properties depend on the metric chosen for the comparison.",
  `References` = ""
)

relative_skill <- list(
  `Metric` = "Relative skill",
  `Name` = list("relative_skill"),
  `Functions` = r"(score(), pairwise_comparison())",
  `D` = r"($\sim$)",
  `C` = r"($\sim$)",
  `B` = r"($\sim$)",
  `Q` = r"($\sim$)",
  `Properties` = "Ranks models based on pairwise comparisons, useful in the context of missing forecasts. Properties depend on the metric chosen for the comparison.",
  `References` = ""
)

scaled_relative_skill <- list(
  `Metric` = "Scaled relative skill",
  `Name` = "scaled_rel_skill",
  `Functions` = r"(score(), pairwise_comparison())",
  `D` = r"($\sim$)",
  `C` = r"($\sim$)",
  `B` = r"($\sim$)",
  `Q` = r"($\sim$)",
  `Properties` = "Ranks models based on pairwise comparisons, useful in the context of missing forecasts. Scaled (i.e. divided) by the score of a baseline model. Properties depend on the metric chosen for the comparison.",
  `References` = ""
)

data <- rbind(as.data.table(ae),
              as.data.table(se),
              as.data.table(crps),
              as.data.table(log_score),
              as.data.table(wis),
              as.data.table(dss),
              as.data.table(brier_score),
              as.data.table(interval_coverage),
              as.data.table(coverage_deviation),
              as.data.table(quantile_coverage),
              as.data.table(dispersion),
              as.data.table(mad),
              as.data.table(under_overprediction),
              as.data.table(pit),
              as.data.table(dispersion),
              as.data.table(bias),
              as.data.table(mean_score_ratio),
              as.data.table(relative_skill),
              as.data.table(scaled_relative_skill))

data[, References := NULL]
setnames(data, old = c("Properties"),
         new = c("Info"))

metrics <- data[, lapply(.SD, FUN = function(x) {
  x <- gsub("$\\checkmark$", '+', x, fixed = TRUE)
  x <- gsub("$-$", '-', x, fixed = TRUE)
  x <- gsub("$\\sim$", '~', x, fixed = TRUE)
  return(x)
})]
setnames(metrics, old = c("D", "C", "B", "Q"),
         new = c("Discrete", "Continuous", "Binary", "Quantile"))

usethis::use_data(metrics, overwrite = TRUE)


#------------------------------------------------------------------------------#
#------------------ Detailed explanation of all the metrics -------------------#
#------------------------------------------------------------------------------#

crps <- list(
  `Metric` = "CRPS (Continuous) ranked probability score",
  `Explanation` = r"(The crps is a proper scoring rule that generalises the absolute error to probabilistic forecasts. It measures the 'distance' of the predictive distribution to the observed data-generating distribution. The CRPS is given as
  $$\text{CRPS}(F, y) = \int_{-\infty}^\infty \left( F(x) - 1(x \geq y) \right)^2 dx,$$
  where y is the observed value and F the CDF of predictive distribution. Often An alternative representation is used:
  $$ \text{CRPS}(F, y) = \frac{1}{2} \mathbb{E}_{F} |X - X'| - \mathbb{E}_P |X - y|,$$ where $X$ and $X'$ are independent realisations from the predictive distributions $F$ with finite first moment and $y$ is the observed value. In this representation we can simply replace $X$ and $X'$ by samples and sum over all possible combinations to obtain the CRPS.
  For integer-valued forecasts, the RPS is given as
  $$ \text{RPS}(F, y) = \sum_{x = 0}^\infty (F(x) - 1(x \geq y))^2. $$

  **Usage and caveats**:
  Smaller values are better. The crps is a good choice for most practical purposes that involve decision making, as it takes the entire predictive distribution into account. If two forecasters assign the same probability to the observed event $y$, then the forecaster who assigned high probability to events far away from $y$ will still get a worse score. The crps (in contrast to the log score) can at times be quite lenient towards very poor predictions. Also, due to it's similarity to the absolute error, the level of scores depend a lot on the absolute value of what is predicted, which makes it hard to compare scores of forecasts for quantities that are orders of magnitude apart.)"
)


log_score <- list(
  `Metric` = "Log score",
  `Explanation` = r"(The Log score is a proper scoring rule that is computed as the negative log of the predictive density evaluated at the observed value. It is given as
  $$ \text{log score} = -\log f(y), $$
  where $f$ is the predictive density function and y is the observed value. For integer-valued forecasts, the log score can be computed as
  $$ \text{log score} = -\log p_y, $$
  where $p_y$ is the probability assigned to outcome p by the forecast F.

  **Usage and caveats**:
  Smaller values are better, but sometimes the sign is reversed. The log score is sensitive to outliers, as individual log score contributions can become very large if the event falls in a range of the predictive distribution where $f(y)$ (or $p_y$) is close to zero. Whether or not that is desirable depends ont the application. In scoringutils, the log score cannot be used for integer-valued forecasts, as the implementation requires a predictive density. In contrast to the crps, the log score is a local scoring rule: it's value only depends only on the probability that was assigned to the actual outcome. This property may be desirable for inferential purposes, for example in a Bayesian context (Winkler et al., 1996). In settings where forecasts inform decision making, it may be more appropriate to score forecasts based on the entire predictive distribution.)"
)

wis <- list(
  Metric = "WIS (Weighted) interval score",
  `Explanation` = r"(The (weighted) interval score is a proper scoring rule for quantile forecasts that converges to the crps for an increasing number of intervals. The score can be decomposed into a sharpness (uncertainty) component and penalties for over- and underprediction. For a single interval, the score is computed as
  $$IS_\alpha(F,y) = (u-l) + \frac{2}{\alpha} \cdot (l-y) \cdot \mathbf{1}(y \leq l) + \frac{2}{\alpha} \cdot (y-u) \cdot \mathbf{1}(y \geq u), $$
  where $\mathbf{1}()$ is the indicator function, $y$ is the observed value, and $l$ and $u$ are the $\frac{\alpha}{2}$ and $1 - \frac{\alpha}{2}$ quantiles of the predictive distribution $F$, i.e. the lower and upper bound of a single prediction interval. For a set of $K$ prediction intervals and the median $m$, the score is computed as a weighted sum,
  $$WIS = \frac{1}{K + 0.5} \cdot \left(w_0 \cdot |y - m| + \sum_{k = 1}^{K} w_k \cdot IS_{\alpha}(F, y)\right),$$
  where $w_k$ is a weight for every interval. Usually, $w_k = \frac{\alpha_k}{2}$ and $w_0 = 0.5$.

  **Usage and caveats**:
  Smaller values are better. Applicable to all quantile forecasts, takes the entire predictive distribution into account. Just as the crps, the wis is based on measures of absolute error. When averaging across multiple targets, it will therefore be dominated by targets with higher absolute values. The decomposition into sharpness, over- and underprediction make it easy to interpret scores and use them for model improvement. )"
)

quantile_score <- "yet to come"


dss <- list(
  `Metric` = "DSS Dawid-Sebastiani score",
  `Explanation` = r"(The Dawid-Sebastiani-Score is a proper scoring rule proposed that only relies on the first moments of the predictive distribution and is therefore easy to compute. It is given as

  $$\text{dss}(F, y) = \left( \frac{y - \mu}{\sigma} \right)^2 + 2 \cdot \log \sigma,$$
  where $F$ is the predictive distribution with mean $\mu$ and standard deviation $\sigma$ and $y$ is the observed value.

  **Usage and caveats**:
  The dss is applicable to continuous and integer forecasts and easy to compute. Apart from the ease of computation we see little advantage in using it over other scores.)"
)

brier_score <- list(
  `Metric` = "Brier score",
  `Explanation` = r"(Proper scoring rule for binary forecasts. The Brier score is computed as
  $$\text{Brier Score} = \frac{1}{N} \sum_{n = 1}^{N} (f_n - y_n),$$
  where $f_n$, with $n = 1, \dots, N$ are the predicted probablities that the corresponding events, $y_n \in (0, 1)$ will be equal to one.

  **Usage**:
  Applicable to all binary forecasts.)"
)

interval_coverage <- list(
  `Metric` = "Interval coverage",
  `Explanation` = r"(Interval coverage measures the proportion of observed values that fall in a given prediction interval range. Interval coverage for a single prediction interval range can be calculated as $$IC_{\alpha} = \text{nominal coverage} - \text{empirical coverage},$$
  where nominal coverage is $1 - \alpha$ and empirical coverage is the proportion of observed values actually covered by all $1 - \alpha$ prediction intervals.

  To summarise interval coverage over different over multiple interval ranges, we can compute coverage deviation defined as the mean interval coverage over all $K$ interval ranges $\alpha_k$ with $k = 1, \dots, K$:
  $$\text{Coverage deviation} = \frac{1}{K} \sum_{k = 1}^{K} \text{IC}_{\alpha_k}$$

  **Usage**:
  Interval coverage for a set of chosen intervals, (e.g. 50\% and 90\%) gives a good indication of marginal calibration and is easy to interpret. Reporting coverage deviation has the advantage of summarising calibration in a single number, but loses some of the nuance.)"
)

quantile_coverage <- list(
  `Metric` = "Quantile coverage",
  `Explanation` = r"(Quantile coverage for a given quantile level is the proportion of observed values smaller than the predictions corresponding to that quantile level.

  **Usage**:
  Quantile coverage is similar to interval coverage, but conveys more information. For example, it allows us to look at the 5\% and 95\% quantile separately, instead of jointly at the 90\% prediction interval). This helps to diagnose whether it is the upper or lower end of a prediction interval that is causing problems. Plots of quantile coverage are conceptually very similar to PIT histograms.)"
)

sharpness <- list(
  `Metric` = "Sharpness",
  `Explanation` = r"(Sharpness is the ability to produce narrow forecasts and is a feature of the forecasts only and does not depend on the observations. Sharpness is therefore only of interest conditional on calibration: a very precise forecast is not useful if it is clearly wrong.

  As suggested by Funk et al. (2019), we measure sharpness for continuous and integer forecasts represented by predictive samples as the normalised median absolute deviation about the median (MADN) ), i.e.
  $$ S(F) = \frac{1}{0.675} \cdot \text{median}(|x - \text{median(x)}|), $$
  where $x$ is the vector of all predictive samples and $\frac{1}{0.675}$ is a normalising constant. If the predictive distribution $F$ is the CDF of a normal distribution, then sharpness will equal the standard deviation of $F$.

  For quantile forecasts we can directly use the sharpness component of the weighted interval score. Sharpness is then simply the weighted mean of the widths of the central prediction intervals.)"
)

bias <- list(
  `Metric` = "Bias",
  `Explanation` = r"(Bias is a measure of the tendency of a forecaster to over- or underpredict. For continuous forecasts, bias is given as
  $$B(F, y) = 1 - 2 \cdot (F (y)), $$
  where $F$ is the CDF of the predictive distribution and $y$ is the observed value.

  For integer-valued forecasts, maximum and minimum bias with respect to any data point $y$ are $\pm (1 - p(y))$, where $p(y)$ is the predicted probability mass at the data point. Bias can be calculated as
  $$B(P, y) = 1 - (P(y) + P(y + 1)), $$
  where $P(y)$ is the cumulative probability assigned to all outcomes smaller or equal to $y$, i.e. the cumulative probability mass function corresponding to $p(y)$.

  For quantile forecasts, Bias can be calculated as the maximum percentile rank for which the prediction is smaller than $y$, if the observed value is smaller than the median of the predictive distribution. If the observed value is above the median of the predictive distribution, then bias is the minimum percentile rank for which the corresponding quantile is still larger than the observed value. If the observed value is exactly the median, bias is zero. For a large enough number of quantiles, the percentile rank will equal the proportion of predictive samples below the observed value, and this metric coincides with the one for continuous forecasts.

  **Usage**:
  In contrast to the over- and underprediction penalties of the interval score it is bound between -1 and 1 and represents the tendency of forecasts to be biased rather than the absolute amount of over- and underprediction. It is therefore a more robust measurement, but harder to interpet. It largely depends on the application whether one is more interested in the tendency to be biased or in the absolute value of over- and underpredictions.)"
)

pit <- list(
  `Metric` = "Probability integral transform (PIT)",
  `Explanation` = r"(The probability integral transform (PIT, Dawid 1984) represents a succinct way to visualise deviations between the predictive distribution $F$ and the true data-generating distribution $G$. The idea is to transform the observed values such that agreement between forecasts and data can then be examined by observing whether or not the transformed values follow a uniform distribution. The PIT is given by
  $$u = F (y),$$
  where $u$ is the transformed variable and $F(y)$ is the predictive distribution $F$ evaluated at the observed value $y$. If $F = G$, then $u$ follows a uniform distribution.

  For integer outcomes, the PIT is no longer uniform even when forecasts are ideal. Instead, a randomised PIT can be used:
  $$u = P(y) + v \cdot (P(y) - P(y - 1) ),$$
  where $y$ is again the observed value $P()$ is the cumulative probability assigned to all values smaller or equal to $y$ (where $P(-1) = 0$ by definition, and $v$ is a standard uniform variable independent of $y$. If $P$ is equal to the true data-generating distribution function, then $u$ is standard uniform.  also propose a non-randomised version of the PIT for count data that could be used alternatively.

  **Usage**:
  One can plot a histogram of $u$ values to look for deviations from uniformity. U-shaped histograms often result from predictions that are too narrow, while hump-shaped histograms indicate that predictions may be too wide. Biased predictions will usually result in a triangle-shaped histogram. One can also test for deviations from normality, using for example an Anderson-Darling test. This, however, proves to be overly strict in practice and even slight deviations from perfect calibration are punished in a way that makes it very hard to compare models at all. In addition, errors from forecasts may be correlated (i.e. forecasts made on a given date), potentially violating the assumptions of the Anderson-Darling test. We therefore do not recommend it for most use cases.)"
)

mean_score_ratio <- list(
  `Metric` = "Mean score ratio",
  `Explanation` = r"(The mean score ratio is used to compare two models on the overlapping set of forecast targets for which both models have made a prediction. The mean score ratio is calculated as the mean score achieved by the first model over the mean score achieved by the second model. More precisely, for two models $i, j$, we determine the set of overlapping forecasts, denoted by $\mathcal{A}_{ij}$ and compute the mean score ratio $\theta_{ij}$ as
  $$\theta_{ij} =\frac{\text{mean score model } i \text{ on } \mathcal{A}_{ij}}{\text{mean score model } j \text{ on } \mathcal{A}_{ij}}.$$
  The mean score ratio can in principle be computed for any arbitrary score.

  **Usage**:
  Mean scores ratios are usually calculated in the context of pairwise comparisons, where a set of models is compared by looking at mean score ratios of all possible parings. Whether smaller or larger values are better depends on the orientation of the original score used)"
)

relative_skill <- list(
  `Metric` = "Relative skill",
  `Explanation` = r"(Relative skill scores can be used to obtain a ranking of models based on pairwise comparisons between all models. To compute the relative skill $\theta_i$ of model $i$, we take the geometric mean of all mean score ratios that involve model $i$, i.e.
  $$ \theta_{i} = \left(\prod_{m = 1}^M \theta_{im}\right)^{1/M}, $$
  where M is the number of models.

  **Usage and caveats**:
  Relative skill is a helpful way to obtain a model ranking. Whether smaller or larger values are better depends on the orientation of the original score used.
  It is in principle relatively robust against biases that arise when models only forecast some of the available targets and is a reasonable way to handle missing forecasts. One possible precautionary measure to reduces issues with missing forecasts is to only compare models that have forecasted at least half of all possible targets (this ensures that there is always an overlap between models). If there is no overlap between models, the relative skill implicitly estimates how a model would have forecasted on those missing targets. )"
)

data <- rbind(as.data.frame(crps),
              as.data.frame(log_score),
              as.data.frame(wis),
              as.data.frame(dss),
              as.data.frame(brier_score),
              as.data.frame(interval_coverage),
              as.data.frame(quantile_coverage),
              as.data.frame(pit),
              as.data.frame(sharpness),
              as.data.frame(bias),
              as.data.frame(mean_score_ratio),
              as.data.frame(relative_skill))

saveRDS(data, "inst/metrics-overview/metrics-detailed.Rda")
