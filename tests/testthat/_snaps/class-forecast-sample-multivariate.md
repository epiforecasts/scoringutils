# as_forecast_multivariate_sample() creates expected structure

    Code
      print(result)
    Message
      Forecast type: sample_multivariate
      Forecast unit:
      location, model, target_type, target_end_date, horizon, and
      .scoringutils_group_id
    Output
      
      Index: <.scoringutils_group_id>
                predicted sample_id observed location                 model
                    <num>     <int>    <num>   <char>                <char>
          1: 102672.00034         1   106987       DE EuroCOVIDhub-ensemble
          2: 164763.08492         2   106987       DE EuroCOVIDhub-ensemble
          3: 153042.63536         3   106987       DE EuroCOVIDhub-ensemble
          4: 119544.25389         4   106987       DE EuroCOVIDhub-ensemble
          5:  81230.71875         5   106987       DE EuroCOVIDhub-ensemble
         ---                                                               
      35476:    159.84534        36       78       IT  epiforecasts-EpiNow2
      35477:    128.21214        37       78       IT  epiforecasts-EpiNow2
      35478:    190.52560        38       78       IT  epiforecasts-EpiNow2
      35479:    141.06659        39       78       IT  epiforecasts-EpiNow2
      35480:     24.43419        40       78       IT  epiforecasts-EpiNow2
             target_type target_end_date horizon .scoringutils_group_id
                  <char>          <Date>   <num>                  <int>
          1:       Cases      2021-05-08       1                      1
          2:       Cases      2021-05-08       1                      1
          3:       Cases      2021-05-08       1                      1
          4:       Cases      2021-05-08       1                      1
          5:       Cases      2021-05-08       1                      1
         ---                                                           
      35476:      Deaths      2021-07-24       2                    224
      35477:      Deaths      2021-07-24       2                    224
      35478:      Deaths      2021-07-24       2                    224
      35479:      Deaths      2021-07-24       2                    224
      35480:      Deaths      2021-07-24       2                    224

---

    Code
      cat("Class:", class(result), "\n")
    Output
      Class: forecast_sample_multivariate forecast data.table data.frame 
    Code
      cat("Forecast type:", scoringutils:::get_forecast_type(result), "\n")
    Output
      Forecast type: sample_multivariate 
    Code
      cat("Forecast unit:", paste(get_forecast_unit(result), collapse = ", "), "\n")
    Output
      Forecast unit: location, model, target_type, target_end_date, horizon, .scoringutils_group_id 
    Code
      cat("Number of rows:", nrow(result), "\n")
    Output
      Number of rows: 35480 
    Code
      cat("Number of columns:", ncol(result), "\n")
    Output
      Number of columns: 9 
    Code
      cat("Column names:", paste(names(result), collapse = ", "), "\n")
    Output
      Column names: predicted, sample_id, observed, location, model, target_type, target_end_date, horizon, .scoringutils_group_id 
    Code
      cat("Number of unique groups:", length(unique(result$.scoringutils_group_id)),
      "\n")
    Output
      Number of unique groups: 224 

# score.forecast_sample_multivariate() creates expected output structure

    Code
      print(scores)
    Output
           target_end_date target_type forecast_date                 model horizon
                    <Date>      <char>        <Date>                <char>   <num>
        1:      2021-05-08       Cases    2021-05-03 EuroCOVIDhub-ensemble       1
        2:      2021-05-08       Cases    2021-05-03 EuroCOVIDhub-baseline       1
        3:      2021-05-08       Cases    2021-05-03  epiforecasts-EpiNow2       1
        4:      2021-05-08      Deaths    2021-05-03 EuroCOVIDhub-ensemble       1
        5:      2021-05-08      Deaths    2021-05-03 EuroCOVIDhub-baseline       1
       ---                                                                        
      220:      2021-07-24      Deaths    2021-07-12 EuroCOVIDhub-baseline       2
      221:      2021-07-24      Deaths    2021-07-05       UMass-MechBayes       3
      222:      2021-07-24      Deaths    2021-07-12       UMass-MechBayes       2
      223:      2021-07-24      Deaths    2021-07-05  epiforecasts-EpiNow2       3
      224:      2021-07-24      Deaths    2021-07-12  epiforecasts-EpiNow2       2
           energy_score .scoringutils_group_id
                  <num>                  <int>
        1:   23454.2838                     37
        2:   34263.4665                     38
        3:   67609.0941                     39
        4:     138.9681                     40
        5:     401.5740                     41
       ---                                    
      220:     275.4460                    256
      221:     142.7219                    257
      222:      96.7978                    258
      223:     142.3382                    259
      224:     137.3695                    260

---

    Code
      cat("Class:", class(scores), "\n")
    Output
      Class: scores data.table data.frame 
    Code
      cat("Number of rows:", nrow(scores), "\n")
    Output
      Number of rows: 224 
    Code
      cat("Number of columns:", ncol(scores), "\n")
    Output
      Number of columns: 7 
    Code
      cat("Column names:", paste(names(scores), collapse = ", "), "\n")
    Output
      Column names: target_end_date, target_type, forecast_date, model, horizon, energy_score, .scoringutils_group_id 
    Code
      cat("Energy score range:", paste(range(scores$energy_score, na.rm = TRUE),
      collapse = " to "), "\n")
    Output
      Energy score range: 37.8373892350605 to 433525.521054322 
    Code
      cat("Number of non-NA energy scores:", sum(!is.na(scores$energy_score)), "\n")
    Output
      Number of non-NA energy scores: 224 
    Code
      cat("Sample of energy scores:", paste(head(scores$energy_score, 5), collapse = ", "),
      "\n")
    Output
      Sample of energy scores: 23454.2837522622, 34263.4664789642, 67609.0941230609, 138.968094149042, 401.574009570395 

---

    Code
      cat("=== Specific Metrics Test ===\n")
    Output
      === Specific Metrics Test ===
    Code
      cat("Class:", class(scores_specific), "\n")
    Output
      Class: scores data.table data.frame 
    Code
      cat("Number of rows:", nrow(scores_specific), "\n")
    Output
      Number of rows: 224 
    Code
      cat("Number of columns:", ncol(scores_specific), "\n")
    Output
      Number of columns: 7 
    Code
      cat("Column names:", paste(names(scores_specific), collapse = ", "), "\n")
    Output
      Column names: target_end_date, target_type, forecast_date, model, horizon, energy_score, .scoringutils_group_id 
    Code
      cat("Energy score range:", paste(range(scores_specific$energy_score, na.rm = TRUE),
      collapse = " to "), "\n")
    Output
      Energy score range: 37.8373892350605 to 433525.521054322 

