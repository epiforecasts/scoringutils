# as_forecast_multivariate_point() creates expected structure

    Code
      cat("Class:", class(result), "\n")
    Output
      Class: forecast_multivariate_point forecast data.table data.frame 
    Code
      cat("Forecast type:", scoringutils:::get_forecast_type(result), "\n")
    Output
      Forecast type: multivariate_point 
    Code
      cat("Forecast unit:", toString(get_forecast_unit(result)), "\n")
    Output
      Forecast unit: location, model, target_type, target_end_date, horizon 
    Code
      cat("Number of rows:", nrow(result), "\n")
    Output
      Number of rows: 887 
    Code
      cat("Number of columns:", ncol(result), "\n")
    Output
      Number of columns: 8 
    Code
      cat("Column names:", toString(names(result)), "\n")
    Output
      Column names: observed, predicted, location, model, target_type, target_end_date, horizon, .mv_group_id 
    Code
      cat("Number of unique groups:", length(unique(result$.mv_group_id)), "\n")
    Output
      Number of unique groups: 224 

# score.forecast_multivariate_point() creates expected output

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
      Number of columns: 6 
    Code
      cat("Column names:", toString(names(scores)), "\n")
    Output
      Column names: model, target_type, target_end_date, horizon, variogram_score, .mv_group_id 
    Code
      cat("Variogram score range:", paste(range(scores$variogram_score, na.rm = TRUE),
      collapse = " to "), "\n")
    Output
      Variogram score range: 5.62526554714927 to 838304.038385032 
    Code
      cat("Number of non-NA scores:", sum(!is.na(scores$variogram_score)), "\n")
    Output
      Number of non-NA scores: 224 

