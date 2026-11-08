# summarised scores for example_quantile have not changed

    Code
      summarise_example_scores(scores_quantile)
    Output
                        model target_type        wis overprediction underprediction
      1 EuroCOVIDhub-baseline       Cases 28483.6000    14096.10000     10285.00000
      2 EuroCOVIDhub-baseline      Deaths   159.4040       65.89910         2.09851
      3 EuroCOVIDhub-ensemble       Cases 17943.8000    10043.10000      4237.18000
      4 EuroCOVIDhub-ensemble      Deaths    41.4225        7.13825         4.10326
      5       UMass-MechBayes      Deaths    52.6519        8.97860        16.80100
      6  epiforecasts-EpiNow2       Cases 20831.6000    11906.80000      3260.36000
      7  epiforecasts-EpiNow2      Deaths    66.6428       18.89260        15.89330
        dispersion        bias interval_coverage_50 interval_coverage_90  ae_median
      1  4102.5000  0.09796880             0.328125             0.820312 38473.6000
      2    91.4062  0.33906200             0.664062             1.000000   233.2580
      3  3663.5200 -0.05640630             0.390625             0.804688 24101.1000
      4    30.1810  0.07265620             0.875000             1.000000    53.1328
      5    26.8724 -0.02234370             0.460938             0.875000    78.4766
      6  5664.3800 -0.07890630             0.468750             0.789062 27923.8000
      7    31.8569 -0.00512605             0.420168             0.907563   104.7480

# summarised scores for example_sample_continuous have not changed

    Code
      summarise_example_scores(scores_sample_continuous)
    Output
                        model target_type       bias         dss       crps
      1 EuroCOVIDhub-baseline       Cases  0.0367188 7.79282e+66 30453.6000
      2 EuroCOVIDhub-baseline      Deaths  0.3187500 1.29936e+01   165.7890
      3 EuroCOVIDhub-ensemble       Cases -0.0464844 2.29000e+01 19703.1000
      4 EuroCOVIDhub-ensemble      Deaths  0.0660156 9.90995e+00    50.8625
      5       UMass-MechBayes      Deaths -0.0269531 1.00858e+01    60.1902
      6  epiforecasts-EpiNow2       Cases -0.0386719 4.08772e+01 22896.5000
      7  epiforecasts-EpiNow2      Deaths -0.0100840 1.02081e+01    74.7901
        overprediction underprediction dispersion log_score       mad  ae_median
      1     13346.0000     11727.30000  5380.2200  14.55720 18982.200 37648.0000
      2        57.6418         1.74240   106.4050   6.97739   378.546   216.9870
      3     10553.0000      4861.02000  4289.0600  15.63340 17385.300 24749.4000
      4        10.6609         5.39538    34.8062   5.86220   141.972    62.6649
      5        11.2051        19.28190    29.7032   5.94162   123.621    79.6600
      6     13462.9000      3346.58000  6087.0200  22.47250 24929.300 28233.0000
      7        19.5855        13.84910    41.3555   6.02409   167.483   102.1890
            se_mean
      1 5.76996e+09
      2 1.62242e+05
      3 4.20604e+09
      4 1.08023e+04
      5 1.37142e+04
      6 6.08286e+09
      7 3.24311e+04

# summarised scores for example_sample_discrete have not changed

    Code
      summarise_example_scores(scores_sample_discrete)
    Output
                        model target_type       bias      dss       crps
      1 EuroCOVIDhub-baseline       Cases  0.0367188 23.73570 30453.6000
      2 EuroCOVIDhub-baseline      Deaths  0.3189450 12.99360   165.7890
      3 EuroCOVIDhub-ensemble       Cases -0.0464844 22.90000 19703.1000
      4 EuroCOVIDhub-ensemble      Deaths  0.0666016  9.90993    50.8646
      5       UMass-MechBayes      Deaths -0.0281250 10.08640    60.1934
      6  epiforecasts-EpiNow2       Cases -0.0386719 40.87980 22896.5000
      7  epiforecasts-EpiNow2      Deaths -0.0100840 10.20800    74.7885
        overprediction underprediction dispersion log_score       mad  ae_median
      1     13346.0000     11727.30000  5380.2200  14.55720 18982.200 37648.0000
      2        57.6414         1.74102   106.4060   6.97743   378.578   217.0160
      3     10553.0000      4861.01000  4289.0700  15.63350 17385.200 24749.4000
      4        10.6605         5.39805    34.8061   5.86238   142.034    62.6641
      5        11.2086        19.28090    29.7039   5.94239   123.606    79.6602
      6     13462.9000      3346.59000  6087.0300  22.47330 24929.300 28233.0000
      7        19.5840        13.85000    41.3544   6.02388   167.478   102.1850
            se_mean
      1 5.76996e+09
      2 1.62242e+05
      3 4.20604e+09
      4 1.08030e+04
      5 1.37144e+04
      6 6.08286e+09
      7 3.24320e+04

# summarised scores for example_point have not changed

    Code
      summarise_example_scores(scores_point)
    Output
                        model target_type   ae_point    se_point      ape
      1 EuroCOVIDhub-baseline       Cases 38473.6000 5.76679e+09 0.854778
      2 EuroCOVIDhub-baseline      Deaths   233.2580 1.04621e+05 0.622009
      3 EuroCOVIDhub-ensemble       Cases 24101.1000 3.89023e+09 0.436185
      4 EuroCOVIDhub-ensemble      Deaths    53.1328 6.57932e+03 0.163154
      5       UMass-MechBayes      Deaths    78.4766 1.17098e+04 0.282321
      6  epiforecasts-EpiNow2       Cases 27923.8000 5.17333e+09 0.431331
      7  epiforecasts-EpiNow2      Deaths   104.7480 2.63170e+04 0.307258

# summarised scores for example_binary have not changed

    Code
      summarise_example_scores(scores_binary)
    Output
                        model target_type brier_score log_score
      1 EuroCOVIDhub-baseline       Cases    0.238726  0.672756
      2 EuroCOVIDhub-baseline      Deaths    0.145059  0.473885
      3 EuroCOVIDhub-ensemble       Cases    0.260479  0.715685
      4 EuroCOVIDhub-ensemble      Deaths    0.225518  0.643372
      5       UMass-MechBayes      Deaths    0.246016  0.685081
      6  epiforecasts-EpiNow2       Cases    0.255562  0.705209
      7  epiforecasts-EpiNow2      Deaths    0.247684  0.688453

# summarised scores for example_nominal have not changed

    Code
      summarise_example_scores(scores_nominal)
    Output
                        model target_type log_score
      1 EuroCOVIDhub-baseline       Cases  1.416410
      2 EuroCOVIDhub-baseline      Deaths  1.513840
      3 EuroCOVIDhub-ensemble       Cases  1.047690
      4 EuroCOVIDhub-ensemble      Deaths  0.713423
      5       UMass-MechBayes      Deaths  0.649390
      6  epiforecasts-EpiNow2       Cases  1.040320
      7  epiforecasts-EpiNow2      Deaths  0.787658

# summarised scores for example_ordinal have not changed

    Code
      summarise_example_scores(score(example_ordinal))
    Output
                        model target_type log_score      rps
      1 EuroCOVIDhub-baseline       Cases  1.416410 0.567236
      2 EuroCOVIDhub-baseline      Deaths  1.513840 0.419189
      3 EuroCOVIDhub-ensemble       Cases  1.047690 0.380610
      4 EuroCOVIDhub-ensemble      Deaths  0.713423 0.184414
      5       UMass-MechBayes      Deaths  0.649390 0.192847
      6  epiforecasts-EpiNow2       Cases  1.040320 0.422905
      7  epiforecasts-EpiNow2      Deaths  0.787658 0.229701

