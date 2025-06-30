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

