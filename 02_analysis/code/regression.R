# (b) regression
## preparation
master <- readr::read_csv(here::here("01_data", "raw", "master.csv"))|> 
  dplyr::select(-1)


## 1.
data_for_regression <- master |> 
  dplyr::group_by(unitid) |> 
  dplyr::mutate(yearofsem = if(any(semester == 1)){min(year[semester == 1])}else{9999}) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(yearstosem = year - yearofsem) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(treated = if(yearstosem >= 0){1} else{0})


## 2.
estimatr::lm_robust(gradrate4yr ~ treated, data = data_for_regression)


## 4.
pre_quater <- master |> 
  dplyr::group_by(unitid) |> 
  dplyr::filter(any(quarter == 1)) |> 
  dplyr::ungroup() 

pre_quater_for_regression <- pre_quater |>
  dplyr::group_by(unitid) |> 
  dplyr::mutate(yearofsem = if(any(semester == 1)){min(year[semester == 1])}else{9999}) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(yearstosem = year - yearofsem) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(treated = if(yearstosem >= 0){yearstosem + 1}else{0}) |> 
  dplyr::mutate(sem1yr  = if(treated == 1){1}else{0},
                sem2yrs = if(treated == 2){1}else{0},
                sem3yrs = if(treated == 3){1}else{0},
                sem4yrs = if(treated == 4){1}else{0},
                sem5_6yrs = if(treated %in% c(5,6)){1}else{0},
                sem7_above = if(treated >= 7){1}else{0})
                
estimatr::lm_robust(gradrate4yr ~ sem1yr + sem2yrs + sem3yrs + sem4yrs + sem5_6yrs + sem7_above,
                    data = pre_quater_for_regression)


## 5.
switchers <- master |> 
  dplyr::group_by(unitid) |> 
  dplyr::filter(any(quarter == 1)) |> 
  dplyr::filter(any(semester == 1)) |> 
  dplyr::ungroup() 

only_quater <- dplyr::anti_join(pre_quater, switchers) 

switchers_for_trendcheck <- switchers |>
  dplyr::group_by(unitid) |> 
  dplyr::mutate(yearofsem = if(any(semester == 1)){min(year[semester == 1])}else{NA}) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(yearstosem = year - yearofsem) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(treated = if(yearstosem >= 0){yearstosem + 1}else{0})

switchers_for_plot <- switchers_for_trendcheck |> 
  dplyr::filter(treated == 0) |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(mean = mean(gradrate4yr)) |> 
  dplyr::ungroup()

only_quater_for_plot <- only_quater |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(mean = mean(gradrate4yr)) |> 
  dplyr::ungroup()

ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = year, y = mean), color = "red" , data = only_quater_for_plot) +
  ggplot2::geom_line(ggplot2::aes(x = year, y = mean), color = "blue", data = switchers_for_plot) +
  ggplot2::labs(x = "year",
                y = "Four-year graduation rates rate") +
  ggplot2::theme_bw()
  