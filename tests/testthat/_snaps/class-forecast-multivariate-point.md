# as_forecast_multivariate_point() creates expected structure

    Code
      cat("Class:", class(result), "\n")
    Output
      Class: forecast_multivariate_point forecast data.table data.frame 
    Code
      cat("Forecast type:", get_forecast_type(result), "\n")
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

# print.forecast_multivariate_point() displays joint_across columns

    Code
      print(result)
    Message
      Forecast type: multivariate_point
      Forecast unit:
      location, model, target_type, target_end_date, and horizon
      Joint across:
      location
    Output
      
      Key: <location, target_end_date, target_type>
      Index: <.mv_group_id>
           observed predicted location                 model target_type
              <num>     <int>   <char>                <char>      <char>
        1:   106987    119258       DE EuroCOVIDhub-ensemble       Cases
        2:   106987    132607       DE EuroCOVIDhub-baseline       Cases
        3:   106987    151179       DE  epiforecasts-EpiNow2       Cases
        4:     1582      1568       DE EuroCOVIDhub-ensemble      Deaths
        5:     1582      1597       DE EuroCOVIDhub-baseline      Deaths
       ---                                                              
      883:       78       131       IT EuroCOVIDhub-baseline      Deaths
      884:       78        79       IT       UMass-MechBayes      Deaths
      885:       78       124       IT       UMass-MechBayes      Deaths
      886:       78       104       IT  epiforecasts-EpiNow2      Deaths
      887:       78       186       IT  epiforecasts-EpiNow2      Deaths
           target_end_date horizon .mv_group_id
                    <Date>   <num>        <int>
        1:      2021-05-08       1            1
        2:      2021-05-08       1            2
        3:      2021-05-08       1            3
        4:      2021-05-08       1            4
        5:      2021-05-08       1            5
       ---                                     
      883:      2021-07-24       2          220
      884:      2021-07-24       3          221
      885:      2021-07-24       2          222
      886:      2021-07-24       3          223
      887:      2021-07-24       2          224

