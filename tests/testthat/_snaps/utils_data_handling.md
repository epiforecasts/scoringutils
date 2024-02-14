# quantile_to_range works - scalar and vector case

    Code
      out1
    Output
      Key: <forecast_id, observed, interval_range>
         forecast_id observed interval_range lower upper
               <int>    <num>          <num> <int> <int>
      1:           1        5              0     5     5
      2:           1        5             20     4     6
      3:           1        5             40     3     7
      4:           1        5             60     2     8
      5:           1        5             80     1     9

---

    Code
      out3
    Output
      Key: <forecast_id, observed, interval_range>
         forecast_id observed interval_range lower upper
               <int>    <num>          <num> <int> <int>
      1:           1        5              0     5     5
      2:           1        5             20     4     6
      3:           1        5             40     3     7
      4:           1        5             60     2     8
      5:           1        5             80     1    NA

---

    Code
      out4
    Output
      Key: <forecast_id, observed, interval_range>
         forecast_id observed interval_range lower upper
               <int>    <num>          <num> <int> <int>
      1:           1        5              0     5     5
      2:           1        5             20     4     6
      3:           1        5             40     3     7
      4:           1        5             60     2     8
      5:           1        5             80     1    NA
      6:           1        5             90    NA     9

# quantile_to_range works - matrix case

    Code
      out1
    Output
      Key: <forecast_id, observed, interval_range>
          forecast_id observed interval_range lower upper
                <int>    <num>          <num> <int> <int>
       1:           1       21              0    21    21
       2:           1       21             20    16    26
       3:           1       21             40    11    31
       4:           1       21             60     6    36
       5:           1       21             80     1    41
       6:           2       22              0    22    22
       7:           2       22             20    17    27
       8:           2       22             40    12    32
       9:           2       22             60     7    37
      10:           2       22             80     2    42
      11:           3       23              0    23    23
      12:           3       23             20    18    28
      13:           3       23             40    13    33
      14:           3       23             60     8    38
      15:           3       23             80     3    43
      16:           4       24              0    24    24
      17:           4       24             20    19    29
      18:           4       24             40    14    34
      19:           4       24             60     9    39
      20:           4       24             80     4    44
      21:           5       25              0    25    25
      22:           5       25             20    20    30
      23:           5       25             40    15    35
      24:           5       25             60    10    40
      25:           5       25             80     5    45
          forecast_id observed interval_range lower upper

