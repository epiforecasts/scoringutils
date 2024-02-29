# print() works on forecast_* objects

    Code
      print(dat)
    Message
      Forecast type:
      binary
      Forecast unit:
      location, location_name, target_end_date, target_type, forecast_date, model,
      and horizon
    Output
      
           location location_name target_end_date target_type forecast_date
             <char>        <char>          <Date>      <char>        <Date>
        1:       DE       Germany      2021-05-08       Cases    2021-05-03
        2:       DE       Germany      2021-05-08       Cases    2021-05-03
        3:       DE       Germany      2021-05-08       Cases    2021-05-03
        4:       DE       Germany      2021-05-08      Deaths    2021-05-03
        5:       DE       Germany      2021-05-08      Deaths    2021-05-03
       ---                                                                 
      883:       IT         Italy      2021-07-24      Deaths    2021-07-12
      884:       IT         Italy      2021-07-24      Deaths    2021-07-05
      885:       IT         Italy      2021-07-24      Deaths    2021-07-12
      886:       IT         Italy      2021-07-24      Deaths    2021-07-05
      887:       IT         Italy      2021-07-24      Deaths    2021-07-12
                           model horizon predicted observed
                          <char>   <num>     <num>   <fctr>
        1: EuroCOVIDhub-ensemble       1     0.375        0
        2: EuroCOVIDhub-baseline       1     0.475        0
        3:  epiforecasts-EpiNow2       1     0.425        0
        4: EuroCOVIDhub-ensemble       1     0.425        0
        5: EuroCOVIDhub-baseline       1     0.500        1
       ---                                                 
      883: EuroCOVIDhub-baseline       2     0.250        0
      884:       UMass-MechBayes       3     0.475        0
      885:       UMass-MechBayes       2     0.450        0
      886:  epiforecasts-EpiNow2       3     0.375        0
      887:  epiforecasts-EpiNow2       2     0.300        0

---

    Code
      print(dat)
    Message
      Forecast type:
      binary
      Forecast unit:
      location, location_name, target_end_date, target_type, forecast_date, model,
      and horizon
    Output
      
           location location_name target_end_date target_type forecast_date
             <char>        <char>          <Date>      <char>        <Date>
        1:       DE       Germany      2021-05-08       Cases    2021-05-03
        2:       DE       Germany      2021-05-08       Cases    2021-05-03
        3:       DE       Germany      2021-05-08       Cases    2021-05-03
        4:       DE       Germany      2021-05-08      Deaths    2021-05-03
        5:       DE       Germany      2021-05-08      Deaths    2021-05-03
       ---                                                                 
      883:       IT         Italy      2021-07-24      Deaths    2021-07-12
      884:       IT         Italy      2021-07-24      Deaths    2021-07-05
      885:       IT         Italy      2021-07-24      Deaths    2021-07-12
      886:       IT         Italy      2021-07-24      Deaths    2021-07-05
      887:       IT         Italy      2021-07-24      Deaths    2021-07-12
                           model horizon predicted observed
                          <char>   <num>     <num>   <fctr>
        1: EuroCOVIDhub-ensemble       1     0.375        0
        2: EuroCOVIDhub-baseline       1     0.475        0
        3:  epiforecasts-EpiNow2       1     0.425        0
        4: EuroCOVIDhub-ensemble       1     0.425        0
        5: EuroCOVIDhub-baseline       1     0.500        1
       ---                                                 
      883: EuroCOVIDhub-baseline       2     0.250        0
      884:       UMass-MechBayes       3     0.475        0
      885:       UMass-MechBayes       2     0.450        0
      886:  epiforecasts-EpiNow2       3     0.375        0
      887:  epiforecasts-EpiNow2       2     0.300        0

---

    Code
      print(dat)
    Message
      Forecast type:
      binary
      Forecast unit:
      location, location_name, target_end_date, target_type, forecast_date, model,
      and horizon
    Output
      
           location location_name target_end_date target_type forecast_date
             <char>        <char>          <Date>      <char>        <Date>
        1:       DE       Germany      2021-05-08       Cases    2021-05-03
        2:       DE       Germany      2021-05-08       Cases    2021-05-03
        3:       DE       Germany      2021-05-08       Cases    2021-05-03
        4:       DE       Germany      2021-05-08      Deaths    2021-05-03
        5:       DE       Germany      2021-05-08      Deaths    2021-05-03
       ---                                                                 
      883:       IT         Italy      2021-07-24      Deaths    2021-07-12
      884:       IT         Italy      2021-07-24      Deaths    2021-07-05
      885:       IT         Italy      2021-07-24      Deaths    2021-07-12
      886:       IT         Italy      2021-07-24      Deaths    2021-07-05
      887:       IT         Italy      2021-07-24      Deaths    2021-07-12
                           model horizon predicted observed
                          <char>   <num>     <num>   <fctr>
        1: EuroCOVIDhub-ensemble       1     0.375        0
        2: EuroCOVIDhub-baseline       1     0.475        0
        3:  epiforecasts-EpiNow2       1     0.425        0
        4: EuroCOVIDhub-ensemble       1     0.425        0
        5: EuroCOVIDhub-baseline       1     0.500        1
       ---                                                 
      883: EuroCOVIDhub-baseline       2     0.250        0
      884:       UMass-MechBayes       3     0.475        0
      885:       UMass-MechBayes       2     0.450        0
      886:  epiforecasts-EpiNow2       3     0.375        0
      887:  epiforecasts-EpiNow2       2     0.300        0

---

    Code
      print(dat)
    Message
      Forecast type:
      binary
      Forecast unit:
      location, location_name, target_end_date, target_type, forecast_date, model,
      and horizon
    Output
      
           location location_name target_end_date target_type forecast_date
             <char>        <char>          <Date>      <char>        <Date>
        1:       DE       Germany      2021-05-08       Cases    2021-05-03
        2:       DE       Germany      2021-05-08       Cases    2021-05-03
        3:       DE       Germany      2021-05-08       Cases    2021-05-03
        4:       DE       Germany      2021-05-08      Deaths    2021-05-03
        5:       DE       Germany      2021-05-08      Deaths    2021-05-03
       ---                                                                 
      883:       IT         Italy      2021-07-24      Deaths    2021-07-12
      884:       IT         Italy      2021-07-24      Deaths    2021-07-05
      885:       IT         Italy      2021-07-24      Deaths    2021-07-12
      886:       IT         Italy      2021-07-24      Deaths    2021-07-05
      887:       IT         Italy      2021-07-24      Deaths    2021-07-12
                           model horizon predicted observed
                          <char>   <num>     <num>   <fctr>
        1: EuroCOVIDhub-ensemble       1     0.375        0
        2: EuroCOVIDhub-baseline       1     0.475        0
        3:  epiforecasts-EpiNow2       1     0.425        0
        4: EuroCOVIDhub-ensemble       1     0.425        0
        5: EuroCOVIDhub-baseline       1     0.500        1
       ---                                                 
      883: EuroCOVIDhub-baseline       2     0.250        0
      884:       UMass-MechBayes       3     0.475        0
      885:       UMass-MechBayes       2     0.450        0
      886:  epiforecasts-EpiNow2       3     0.375        0
      887:  epiforecasts-EpiNow2       2     0.300        0

---

    Code
      print(dat)
    Message
      Forecast type:
      quantile
      Forecast unit:
      location, target_end_date, target_type, location_name, forecast_date, model,
      and horizon
    Output
      
      Key: <location, target_end_date, target_type>
             location target_end_date target_type observed location_name
               <char>          <Date>      <char>    <num>        <char>
          1:       DE      2021-05-08       Cases   106987       Germany
          2:       DE      2021-05-08       Cases   106987       Germany
          3:       DE      2021-05-08       Cases   106987       Germany
          4:       DE      2021-05-08       Cases   106987       Germany
          5:       DE      2021-05-08       Cases   106987       Germany
         ---                                                            
      20397:       IT      2021-07-24      Deaths       78         Italy
      20398:       IT      2021-07-24      Deaths       78         Italy
      20399:       IT      2021-07-24      Deaths       78         Italy
      20400:       IT      2021-07-24      Deaths       78         Italy
      20401:       IT      2021-07-24      Deaths       78         Italy
             forecast_date quantile_level predicted                 model horizon
                    <Date>          <num>     <int>                <char>   <num>
          1:    2021-05-03          0.010     82466 EuroCOVIDhub-ensemble       1
          2:    2021-05-03          0.025     86669 EuroCOVIDhub-ensemble       1
          3:    2021-05-03          0.050     90285 EuroCOVIDhub-ensemble       1
          4:    2021-05-03          0.100     95341 EuroCOVIDhub-ensemble       1
          5:    2021-05-03          0.150     99171 EuroCOVIDhub-ensemble       1
         ---                                                                     
      20397:    2021-07-12          0.850       352  epiforecasts-EpiNow2       2
      20398:    2021-07-12          0.900       397  epiforecasts-EpiNow2       2
      20399:    2021-07-12          0.950       499  epiforecasts-EpiNow2       2
      20400:    2021-07-12          0.975       611  epiforecasts-EpiNow2       2
      20401:    2021-07-12          0.990       719  epiforecasts-EpiNow2       2

---

    Code
      print(dat)
    Message
      Forecast type:
      quantile
      Forecast unit:
      location, target_end_date, target_type, location_name, forecast_date, model,
      and horizon
    Output
      
      Key: <location, target_end_date, target_type>
             location target_end_date target_type observed location_name
               <char>          <Date>      <char>    <num>        <char>
          1:       DE      2021-05-08       Cases   106987       Germany
          2:       DE      2021-05-08       Cases   106987       Germany
          3:       DE      2021-05-08       Cases   106987       Germany
          4:       DE      2021-05-08       Cases   106987       Germany
          5:       DE      2021-05-08       Cases   106987       Germany
         ---                                                            
      20397:       IT      2021-07-24      Deaths       78         Italy
      20398:       IT      2021-07-24      Deaths       78         Italy
      20399:       IT      2021-07-24      Deaths       78         Italy
      20400:       IT      2021-07-24      Deaths       78         Italy
      20401:       IT      2021-07-24      Deaths       78         Italy
             forecast_date quantile_level predicted                 model horizon
                    <Date>          <num>     <int>                <char>   <num>
          1:    2021-05-03          0.010     82466 EuroCOVIDhub-ensemble       1
          2:    2021-05-03          0.025     86669 EuroCOVIDhub-ensemble       1
          3:    2021-05-03          0.050     90285 EuroCOVIDhub-ensemble       1
          4:    2021-05-03          0.100     95341 EuroCOVIDhub-ensemble       1
          5:    2021-05-03          0.150     99171 EuroCOVIDhub-ensemble       1
         ---                                                                     
      20397:    2021-07-12          0.850       352  epiforecasts-EpiNow2       2
      20398:    2021-07-12          0.900       397  epiforecasts-EpiNow2       2
      20399:    2021-07-12          0.950       499  epiforecasts-EpiNow2       2
      20400:    2021-07-12          0.975       611  epiforecasts-EpiNow2       2
      20401:    2021-07-12          0.990       719  epiforecasts-EpiNow2       2

---

    Code
      print(dat)
    Message
      Forecast type:
      quantile
      Forecast unit:
      location, target_end_date, target_type, location_name, forecast_date, model,
      and horizon
    Output
      
      Key: <location, target_end_date, target_type>
             location target_end_date target_type observed location_name
               <char>          <Date>      <char>    <num>        <char>
          1:       DE      2021-05-08       Cases   106987       Germany
          2:       DE      2021-05-08       Cases   106987       Germany
          3:       DE      2021-05-08       Cases   106987       Germany
          4:       DE      2021-05-08       Cases   106987       Germany
          5:       DE      2021-05-08       Cases   106987       Germany
         ---                                                            
      20397:       IT      2021-07-24      Deaths       78         Italy
      20398:       IT      2021-07-24      Deaths       78         Italy
      20399:       IT      2021-07-24      Deaths       78         Italy
      20400:       IT      2021-07-24      Deaths       78         Italy
      20401:       IT      2021-07-24      Deaths       78         Italy
             forecast_date quantile_level predicted                 model horizon
                    <Date>          <num>     <int>                <char>   <num>
          1:    2021-05-03          0.010     82466 EuroCOVIDhub-ensemble       1
          2:    2021-05-03          0.025     86669 EuroCOVIDhub-ensemble       1
          3:    2021-05-03          0.050     90285 EuroCOVIDhub-ensemble       1
          4:    2021-05-03          0.100     95341 EuroCOVIDhub-ensemble       1
          5:    2021-05-03          0.150     99171 EuroCOVIDhub-ensemble       1
         ---                                                                     
      20397:    2021-07-12          0.850       352  epiforecasts-EpiNow2       2
      20398:    2021-07-12          0.900       397  epiforecasts-EpiNow2       2
      20399:    2021-07-12          0.950       499  epiforecasts-EpiNow2       2
      20400:    2021-07-12          0.975       611  epiforecasts-EpiNow2       2
      20401:    2021-07-12          0.990       719  epiforecasts-EpiNow2       2

---

    Code
      print(dat)
    Message
      Forecast type:
      quantile
      Forecast unit:
      location, target_end_date, target_type, location_name, forecast_date, model,
      and horizon
    Output
      
      Key: <location, target_end_date, target_type>
             location target_end_date target_type observed location_name
               <char>          <Date>      <char>    <num>        <char>
          1:       DE      2021-05-08       Cases   106987       Germany
          2:       DE      2021-05-08       Cases   106987       Germany
          3:       DE      2021-05-08       Cases   106987       Germany
          4:       DE      2021-05-08       Cases   106987       Germany
          5:       DE      2021-05-08       Cases   106987       Germany
         ---                                                            
      20397:       IT      2021-07-24      Deaths       78         Italy
      20398:       IT      2021-07-24      Deaths       78         Italy
      20399:       IT      2021-07-24      Deaths       78         Italy
      20400:       IT      2021-07-24      Deaths       78         Italy
      20401:       IT      2021-07-24      Deaths       78         Italy
             forecast_date quantile_level predicted                 model horizon
                    <Date>          <num>     <int>                <char>   <num>
          1:    2021-05-03          0.010     82466 EuroCOVIDhub-ensemble       1
          2:    2021-05-03          0.025     86669 EuroCOVIDhub-ensemble       1
          3:    2021-05-03          0.050     90285 EuroCOVIDhub-ensemble       1
          4:    2021-05-03          0.100     95341 EuroCOVIDhub-ensemble       1
          5:    2021-05-03          0.150     99171 EuroCOVIDhub-ensemble       1
         ---                                                                     
      20397:    2021-07-12          0.850       352  epiforecasts-EpiNow2       2
      20398:    2021-07-12          0.900       397  epiforecasts-EpiNow2       2
      20399:    2021-07-12          0.950       499  epiforecasts-EpiNow2       2
      20400:    2021-07-12          0.975       611  epiforecasts-EpiNow2       2
      20401:    2021-07-12          0.990       719  epiforecasts-EpiNow2       2

---

    Code
      print(dat)
    Message
      Forecast type:
      point
      Forecast unit:
      location, target_end_date, target_type, location_name, forecast_date, model,
      and horizon
    Output
      
      Key: <location, target_end_date, target_type>
           location target_end_date target_type observed location_name forecast_date
             <char>          <Date>      <char>    <num>        <char>        <Date>
        1:       DE      2021-05-08       Cases   106987       Germany    2021-05-03
        2:       DE      2021-05-08       Cases   106987       Germany    2021-05-03
        3:       DE      2021-05-08       Cases   106987       Germany    2021-05-03
        4:       DE      2021-05-08      Deaths     1582       Germany    2021-05-03
        5:       DE      2021-05-08      Deaths     1582       Germany    2021-05-03
       ---                                                                          
      883:       IT      2021-07-24      Deaths       78         Italy    2021-07-12
      884:       IT      2021-07-24      Deaths       78         Italy    2021-07-05
      885:       IT      2021-07-24      Deaths       78         Italy    2021-07-12
      886:       IT      2021-07-24      Deaths       78         Italy    2021-07-05
      887:       IT      2021-07-24      Deaths       78         Italy    2021-07-12
           predicted                 model horizon
               <int>                <char>   <num>
        1:    119258 EuroCOVIDhub-ensemble       1
        2:    132607 EuroCOVIDhub-baseline       1
        3:    151179  epiforecasts-EpiNow2       1
        4:      1568 EuroCOVIDhub-ensemble       1
        5:      1597 EuroCOVIDhub-baseline       1
       ---                                        
      883:       131 EuroCOVIDhub-baseline       2
      884:        79       UMass-MechBayes       3
      885:       124       UMass-MechBayes       2
      886:       104  epiforecasts-EpiNow2       3
      887:       186  epiforecasts-EpiNow2       2

---

    Code
      print(dat)
    Message
      Forecast type:
      point
      Forecast unit:
      location, target_end_date, target_type, location_name, forecast_date, model,
      and horizon
    Output
      
      Key: <location, target_end_date, target_type>
           location target_end_date target_type observed location_name forecast_date
             <char>          <Date>      <char>    <num>        <char>        <Date>
        1:       DE      2021-05-08       Cases   106987       Germany    2021-05-03
        2:       DE      2021-05-08       Cases   106987       Germany    2021-05-03
        3:       DE      2021-05-08       Cases   106987       Germany    2021-05-03
        4:       DE      2021-05-08      Deaths     1582       Germany    2021-05-03
        5:       DE      2021-05-08      Deaths     1582       Germany    2021-05-03
       ---                                                                          
      883:       IT      2021-07-24      Deaths       78         Italy    2021-07-12
      884:       IT      2021-07-24      Deaths       78         Italy    2021-07-05
      885:       IT      2021-07-24      Deaths       78         Italy    2021-07-12
      886:       IT      2021-07-24      Deaths       78         Italy    2021-07-05
      887:       IT      2021-07-24      Deaths       78         Italy    2021-07-12
           predicted                 model horizon
               <int>                <char>   <num>
        1:    119258 EuroCOVIDhub-ensemble       1
        2:    132607 EuroCOVIDhub-baseline       1
        3:    151179  epiforecasts-EpiNow2       1
        4:      1568 EuroCOVIDhub-ensemble       1
        5:      1597 EuroCOVIDhub-baseline       1
       ---                                        
      883:       131 EuroCOVIDhub-baseline       2
      884:        79       UMass-MechBayes       3
      885:       124       UMass-MechBayes       2
      886:       104  epiforecasts-EpiNow2       3
      887:       186  epiforecasts-EpiNow2       2

---

    Code
      print(dat)
    Message
      Forecast type:
      point
      Forecast unit:
      location, target_end_date, target_type, location_name, forecast_date, model,
      and horizon
    Output
      
      Key: <location, target_end_date, target_type>
           location target_end_date target_type observed location_name forecast_date
             <char>          <Date>      <char>    <num>        <char>        <Date>
        1:       DE      2021-05-08       Cases   106987       Germany    2021-05-03
        2:       DE      2021-05-08       Cases   106987       Germany    2021-05-03
        3:       DE      2021-05-08       Cases   106987       Germany    2021-05-03
        4:       DE      2021-05-08      Deaths     1582       Germany    2021-05-03
        5:       DE      2021-05-08      Deaths     1582       Germany    2021-05-03
       ---                                                                          
      883:       IT      2021-07-24      Deaths       78         Italy    2021-07-12
      884:       IT      2021-07-24      Deaths       78         Italy    2021-07-05
      885:       IT      2021-07-24      Deaths       78         Italy    2021-07-12
      886:       IT      2021-07-24      Deaths       78         Italy    2021-07-05
      887:       IT      2021-07-24      Deaths       78         Italy    2021-07-12
           predicted                 model horizon
               <int>                <char>   <num>
        1:    119258 EuroCOVIDhub-ensemble       1
        2:    132607 EuroCOVIDhub-baseline       1
        3:    151179  epiforecasts-EpiNow2       1
        4:      1568 EuroCOVIDhub-ensemble       1
        5:      1597 EuroCOVIDhub-baseline       1
       ---                                        
      883:       131 EuroCOVIDhub-baseline       2
      884:        79       UMass-MechBayes       3
      885:       124       UMass-MechBayes       2
      886:       104  epiforecasts-EpiNow2       3
      887:       186  epiforecasts-EpiNow2       2

---

    Code
      print(dat)
    Message
      Forecast type:
      point
      Forecast unit:
      location, target_end_date, target_type, location_name, forecast_date, model,
      and horizon
    Output
      
      Key: <location, target_end_date, target_type>
           location target_end_date target_type observed location_name forecast_date
             <char>          <Date>      <char>    <num>        <char>        <Date>
        1:       DE      2021-05-08       Cases   106987       Germany    2021-05-03
        2:       DE      2021-05-08       Cases   106987       Germany    2021-05-03
        3:       DE      2021-05-08       Cases   106987       Germany    2021-05-03
        4:       DE      2021-05-08      Deaths     1582       Germany    2021-05-03
        5:       DE      2021-05-08      Deaths     1582       Germany    2021-05-03
       ---                                                                          
      883:       IT      2021-07-24      Deaths       78         Italy    2021-07-12
      884:       IT      2021-07-24      Deaths       78         Italy    2021-07-05
      885:       IT      2021-07-24      Deaths       78         Italy    2021-07-12
      886:       IT      2021-07-24      Deaths       78         Italy    2021-07-05
      887:       IT      2021-07-24      Deaths       78         Italy    2021-07-12
           predicted                 model horizon
               <int>                <char>   <num>
        1:    119258 EuroCOVIDhub-ensemble       1
        2:    132607 EuroCOVIDhub-baseline       1
        3:    151179  epiforecasts-EpiNow2       1
        4:      1568 EuroCOVIDhub-ensemble       1
        5:      1597 EuroCOVIDhub-baseline       1
       ---                                        
      883:       131 EuroCOVIDhub-baseline       2
      884:        79       UMass-MechBayes       3
      885:       124       UMass-MechBayes       2
      886:       104  epiforecasts-EpiNow2       3
      887:       186  epiforecasts-EpiNow2       2

---

    Code
      print(dat)
    Message
      Forecast type:
      sample
      Forecast unit:
      location, location_name, target_end_date, target_type, forecast_date, model,
      and horizon
    Output
      
             location location_name target_end_date target_type forecast_date
               <char>        <char>          <Date>      <char>        <Date>
          1:       DE       Germany      2021-05-08       Cases    2021-05-03
          2:       DE       Germany      2021-05-08       Cases    2021-05-03
          3:       DE       Germany      2021-05-08       Cases    2021-05-03
          4:       DE       Germany      2021-05-08       Cases    2021-05-03
          5:       DE       Germany      2021-05-08       Cases    2021-05-03
         ---                                                                 
      35476:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35477:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35478:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35479:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35480:       IT         Italy      2021-07-24      Deaths    2021-07-12
                             model horizon    predicted sample_id observed
                            <char>   <num>        <num>     <int>    <num>
          1: EuroCOVIDhub-ensemble       1 102672.00034         1   106987
          2: EuroCOVIDhub-ensemble       1 164763.08492         2   106987
          3: EuroCOVIDhub-ensemble       1 153042.63536         3   106987
          4: EuroCOVIDhub-ensemble       1 119544.25389         4   106987
          5: EuroCOVIDhub-ensemble       1  81230.71875         5   106987
         ---                                                              
      35476:  epiforecasts-EpiNow2       2    159.84534        36       78
      35477:  epiforecasts-EpiNow2       2    128.21214        37       78
      35478:  epiforecasts-EpiNow2       2    190.52560        38       78
      35479:  epiforecasts-EpiNow2       2    141.06659        39       78
      35480:  epiforecasts-EpiNow2       2     24.43419        40       78

---

    Code
      print(dat)
    Message
      Forecast type:
      sample
      Forecast unit:
      location, location_name, target_end_date, target_type, forecast_date, model,
      and horizon
    Output
      
             location location_name target_end_date target_type forecast_date
               <char>        <char>          <Date>      <char>        <Date>
          1:       DE       Germany      2021-05-08       Cases    2021-05-03
          2:       DE       Germany      2021-05-08       Cases    2021-05-03
          3:       DE       Germany      2021-05-08       Cases    2021-05-03
          4:       DE       Germany      2021-05-08       Cases    2021-05-03
          5:       DE       Germany      2021-05-08       Cases    2021-05-03
         ---                                                                 
      35476:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35477:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35478:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35479:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35480:       IT         Italy      2021-07-24      Deaths    2021-07-12
                             model horizon    predicted sample_id observed
                            <char>   <num>        <num>     <int>    <num>
          1: EuroCOVIDhub-ensemble       1 102672.00034         1   106987
          2: EuroCOVIDhub-ensemble       1 164763.08492         2   106987
          3: EuroCOVIDhub-ensemble       1 153042.63536         3   106987
          4: EuroCOVIDhub-ensemble       1 119544.25389         4   106987
          5: EuroCOVIDhub-ensemble       1  81230.71875         5   106987
         ---                                                              
      35476:  epiforecasts-EpiNow2       2    159.84534        36       78
      35477:  epiforecasts-EpiNow2       2    128.21214        37       78
      35478:  epiforecasts-EpiNow2       2    190.52560        38       78
      35479:  epiforecasts-EpiNow2       2    141.06659        39       78
      35480:  epiforecasts-EpiNow2       2     24.43419        40       78

---

    Code
      print(dat)
    Message
      Forecast type:
      sample
      Forecast unit:
      location, location_name, target_end_date, target_type, forecast_date, model,
      and horizon
    Output
      
             location location_name target_end_date target_type forecast_date
               <char>        <char>          <Date>      <char>        <Date>
          1:       DE       Germany      2021-05-08       Cases    2021-05-03
          2:       DE       Germany      2021-05-08       Cases    2021-05-03
          3:       DE       Germany      2021-05-08       Cases    2021-05-03
          4:       DE       Germany      2021-05-08       Cases    2021-05-03
          5:       DE       Germany      2021-05-08       Cases    2021-05-03
         ---                                                                 
      35476:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35477:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35478:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35479:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35480:       IT         Italy      2021-07-24      Deaths    2021-07-12
                             model horizon    predicted sample_id observed
                            <char>   <num>        <num>     <int>    <num>
          1: EuroCOVIDhub-ensemble       1 102672.00034         1   106987
          2: EuroCOVIDhub-ensemble       1 164763.08492         2   106987
          3: EuroCOVIDhub-ensemble       1 153042.63536         3   106987
          4: EuroCOVIDhub-ensemble       1 119544.25389         4   106987
          5: EuroCOVIDhub-ensemble       1  81230.71875         5   106987
         ---                                                              
      35476:  epiforecasts-EpiNow2       2    159.84534        36       78
      35477:  epiforecasts-EpiNow2       2    128.21214        37       78
      35478:  epiforecasts-EpiNow2       2    190.52560        38       78
      35479:  epiforecasts-EpiNow2       2    141.06659        39       78
      35480:  epiforecasts-EpiNow2       2     24.43419        40       78

---

    Code
      print(dat)
    Message
      Forecast type:
      sample
      Forecast unit:
      location, location_name, target_end_date, target_type, forecast_date, model,
      and horizon
    Output
      
             location location_name target_end_date target_type forecast_date
               <char>        <char>          <Date>      <char>        <Date>
          1:       DE       Germany      2021-05-08       Cases    2021-05-03
          2:       DE       Germany      2021-05-08       Cases    2021-05-03
          3:       DE       Germany      2021-05-08       Cases    2021-05-03
          4:       DE       Germany      2021-05-08       Cases    2021-05-03
          5:       DE       Germany      2021-05-08       Cases    2021-05-03
         ---                                                                 
      35476:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35477:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35478:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35479:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35480:       IT         Italy      2021-07-24      Deaths    2021-07-12
                             model horizon    predicted sample_id observed
                            <char>   <num>        <num>     <int>    <num>
          1: EuroCOVIDhub-ensemble       1 102672.00034         1   106987
          2: EuroCOVIDhub-ensemble       1 164763.08492         2   106987
          3: EuroCOVIDhub-ensemble       1 153042.63536         3   106987
          4: EuroCOVIDhub-ensemble       1 119544.25389         4   106987
          5: EuroCOVIDhub-ensemble       1  81230.71875         5   106987
         ---                                                              
      35476:  epiforecasts-EpiNow2       2    159.84534        36       78
      35477:  epiforecasts-EpiNow2       2    128.21214        37       78
      35478:  epiforecasts-EpiNow2       2    190.52560        38       78
      35479:  epiforecasts-EpiNow2       2    141.06659        39       78
      35480:  epiforecasts-EpiNow2       2     24.43419        40       78

---

    Code
      print(dat)
    Message
      Forecast type:
      sample
      Forecast unit:
      location, location_name, target_end_date, target_type, forecast_date, model,
      and horizon
    Output
      
             location location_name target_end_date target_type forecast_date
               <char>        <char>          <Date>      <char>        <Date>
          1:       DE       Germany      2021-05-08       Cases    2021-05-03
          2:       DE       Germany      2021-05-08       Cases    2021-05-03
          3:       DE       Germany      2021-05-08       Cases    2021-05-03
          4:       DE       Germany      2021-05-08       Cases    2021-05-03
          5:       DE       Germany      2021-05-08       Cases    2021-05-03
         ---                                                                 
      35476:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35477:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35478:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35479:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35480:       IT         Italy      2021-07-24      Deaths    2021-07-12
                             model horizon predicted sample_id observed
                            <char>   <num>     <num>     <int>    <num>
          1: EuroCOVIDhub-ensemble       1    102672         1   106987
          2: EuroCOVIDhub-ensemble       1    164763         2   106987
          3: EuroCOVIDhub-ensemble       1    153043         3   106987
          4: EuroCOVIDhub-ensemble       1    119544         4   106987
          5: EuroCOVIDhub-ensemble       1     81231         5   106987
         ---                                                           
      35476:  epiforecasts-EpiNow2       2       160        36       78
      35477:  epiforecasts-EpiNow2       2       128        37       78
      35478:  epiforecasts-EpiNow2       2       191        38       78
      35479:  epiforecasts-EpiNow2       2       141        39       78
      35480:  epiforecasts-EpiNow2       2        24        40       78

---

    Code
      print(dat)
    Message
      Forecast type:
      sample
      Forecast unit:
      location, location_name, target_end_date, target_type, forecast_date, model,
      and horizon
    Output
      
             location location_name target_end_date target_type forecast_date
               <char>        <char>          <Date>      <char>        <Date>
          1:       DE       Germany      2021-05-08       Cases    2021-05-03
          2:       DE       Germany      2021-05-08       Cases    2021-05-03
          3:       DE       Germany      2021-05-08       Cases    2021-05-03
          4:       DE       Germany      2021-05-08       Cases    2021-05-03
          5:       DE       Germany      2021-05-08       Cases    2021-05-03
         ---                                                                 
      35476:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35477:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35478:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35479:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35480:       IT         Italy      2021-07-24      Deaths    2021-07-12
                             model horizon predicted sample_id observed
                            <char>   <num>     <num>     <int>    <num>
          1: EuroCOVIDhub-ensemble       1    102672         1   106987
          2: EuroCOVIDhub-ensemble       1    164763         2   106987
          3: EuroCOVIDhub-ensemble       1    153043         3   106987
          4: EuroCOVIDhub-ensemble       1    119544         4   106987
          5: EuroCOVIDhub-ensemble       1     81231         5   106987
         ---                                                           
      35476:  epiforecasts-EpiNow2       2       160        36       78
      35477:  epiforecasts-EpiNow2       2       128        37       78
      35478:  epiforecasts-EpiNow2       2       191        38       78
      35479:  epiforecasts-EpiNow2       2       141        39       78
      35480:  epiforecasts-EpiNow2       2        24        40       78

---

    Code
      print(dat)
    Message
      Forecast type:
      sample
      Forecast unit:
      location, location_name, target_end_date, target_type, forecast_date, model,
      and horizon
    Output
      
             location location_name target_end_date target_type forecast_date
               <char>        <char>          <Date>      <char>        <Date>
          1:       DE       Germany      2021-05-08       Cases    2021-05-03
          2:       DE       Germany      2021-05-08       Cases    2021-05-03
          3:       DE       Germany      2021-05-08       Cases    2021-05-03
          4:       DE       Germany      2021-05-08       Cases    2021-05-03
          5:       DE       Germany      2021-05-08       Cases    2021-05-03
         ---                                                                 
      35476:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35477:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35478:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35479:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35480:       IT         Italy      2021-07-24      Deaths    2021-07-12
                             model horizon predicted sample_id observed
                            <char>   <num>     <num>     <int>    <num>
          1: EuroCOVIDhub-ensemble       1    102672         1   106987
          2: EuroCOVIDhub-ensemble       1    164763         2   106987
          3: EuroCOVIDhub-ensemble       1    153043         3   106987
          4: EuroCOVIDhub-ensemble       1    119544         4   106987
          5: EuroCOVIDhub-ensemble       1     81231         5   106987
         ---                                                           
      35476:  epiforecasts-EpiNow2       2       160        36       78
      35477:  epiforecasts-EpiNow2       2       128        37       78
      35478:  epiforecasts-EpiNow2       2       191        38       78
      35479:  epiforecasts-EpiNow2       2       141        39       78
      35480:  epiforecasts-EpiNow2       2        24        40       78

---

    Code
      print(dat)
    Message
      Forecast type:
      sample
      Forecast unit:
      location, location_name, target_end_date, target_type, forecast_date, model,
      and horizon
    Output
      
             location location_name target_end_date target_type forecast_date
               <char>        <char>          <Date>      <char>        <Date>
          1:       DE       Germany      2021-05-08       Cases    2021-05-03
          2:       DE       Germany      2021-05-08       Cases    2021-05-03
          3:       DE       Germany      2021-05-08       Cases    2021-05-03
          4:       DE       Germany      2021-05-08       Cases    2021-05-03
          5:       DE       Germany      2021-05-08       Cases    2021-05-03
         ---                                                                 
      35476:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35477:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35478:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35479:       IT         Italy      2021-07-24      Deaths    2021-07-12
      35480:       IT         Italy      2021-07-24      Deaths    2021-07-12
                             model horizon predicted sample_id observed
                            <char>   <num>     <num>     <int>    <num>
          1: EuroCOVIDhub-ensemble       1    102672         1   106987
          2: EuroCOVIDhub-ensemble       1    164763         2   106987
          3: EuroCOVIDhub-ensemble       1    153043         3   106987
          4: EuroCOVIDhub-ensemble       1    119544         4   106987
          5: EuroCOVIDhub-ensemble       1     81231         5   106987
         ---                                                           
      35476:  epiforecasts-EpiNow2       2       160        36       78
      35477:  epiforecasts-EpiNow2       2       128        37       78
      35478:  epiforecasts-EpiNow2       2       191        38       78
      35479:  epiforecasts-EpiNow2       2       141        39       78
      35480:  epiforecasts-EpiNow2       2        24        40       78

---

    Code
      print(dat)
    Message
      Forecast type:
      quantile
      Forecast unit:
      location, target_end_date, target_type, horizon, and model
      Score columns:
      interval_coverage, interval_coverage_deviation, quantile_coverage, and
      quantile_coverage_deviation
    Output
      
      Key: <location, target_end_date, target_type, horizon, model, interval_range>
             observed quantile_level predicted location target_end_date target_type
                <num>          <num>     <int>   <char>          <Date>      <char>
          1:   106987          0.500    132607       DE      2021-05-08       Cases
          2:   106987          0.450    132402       DE      2021-05-08       Cases
          3:   106987          0.550    132812       DE      2021-05-08       Cases
          4:   106987          0.400    131912       DE      2021-05-08       Cases
          5:   106987          0.600    133302       DE      2021-05-08       Cases
         ---                                                                       
      20397:       78          0.950       370       IT      2021-07-24      Deaths
      20398:       78          0.025        47       IT      2021-07-24      Deaths
      20399:       78          0.975       517       IT      2021-07-24      Deaths
      20400:       78          0.010        39       IT      2021-07-24      Deaths
      20401:       78          0.990       691       IT      2021-07-24      Deaths
             horizon                 model interval_range interval_coverage
               <num>                <char>          <num>            <lgcl>
          1:       1 EuroCOVIDhub-baseline              0             FALSE
          2:       1 EuroCOVIDhub-baseline             10             FALSE
          3:       1 EuroCOVIDhub-baseline             10             FALSE
          4:       1 EuroCOVIDhub-baseline             20             FALSE
          5:       1 EuroCOVIDhub-baseline             20             FALSE
         ---                                                               
      20397:       3  epiforecasts-EpiNow2             90              TRUE
      20398:       3  epiforecasts-EpiNow2             95              TRUE
      20399:       3  epiforecasts-EpiNow2             95              TRUE
      20400:       3  epiforecasts-EpiNow2             98              TRUE
      20401:       3  epiforecasts-EpiNow2             98              TRUE
             interval_coverage_deviation quantile_coverage
                                   <num>            <lgcl>
          1:                        0.00              TRUE
          2:                       -0.10              TRUE
          3:                       -0.10              TRUE
          4:                       -0.20              TRUE
          5:                       -0.20              TRUE
         ---                                              
      20397:                        0.10              TRUE
      20398:                        0.05             FALSE
      20399:                        0.05              TRUE
      20400:                        0.02             FALSE
      20401:                        0.02              TRUE
             quantile_coverage_deviation
                                   <num>
          1:                       0.500
          2:                       0.550
          3:                       0.450
          4:                       0.600
          5:                       0.400
         ---                            
      20397:                       0.050
      20398:                      -0.025
      20399:                       0.025
      20400:                      -0.010
      20401:                       0.010

