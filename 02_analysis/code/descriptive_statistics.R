# descriptive statistics

# read raw data
master <- readr::read_csv(here::here("01_data", "raw", "master.csv")) |> 
  dplyr::select(-1)


# 1.
switchers <- master |> 
  dplyr::group_by(unitid) |> 
  dplyr::filter(any(quarter == 1)) |> 
  dplyr::filter(any(semester == 1)) |> 
  dplyr::ungroup() 
never_swichers <- dplyr::anti_join(master, switchers) 

make_descriptive_statistics <- function(dataframe, type){
  descriptive_statistics <-  dataframe[,c(3, 13, 12, 14, 17, 6, 15, 16)] |>
    colMeans(na.rm = TRUE) |> 
    round(digits = 2) |> 
    as.data.frame() |> 
    magrittr::set_colnames(type)
}
descriptive_statistics_all             <- make_descriptive_statistics(dataframe = master, type = "all")
descriptive_statistics_switchers       <- make_descriptive_statistics(dataframe = switchers, type = "switchers")
descriptive_statistics_never_switchers <- make_descriptive_statistics(dataframe = never_swichers, type = "never_swichers")

descriptive_statistics <- cbind(descriptive_statistics_all, descriptive_statistics_never_switchers, descriptive_statistics_switchers)


## 2. and 3.
master_by_year <- master |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(gradrate4yr_mean = mean(gradrate4yr), semester_rate = mean(semester))

graph_gradrate <- ggplot2::ggplot(data = master_by_year, ggplot2::aes(x = year, y = gradrate4yr_mean)) +
  ggplot2::geom_line() +
  ggplot2::xlim(c(1990, 2010)) +
  ggplot2::ylim(c(0.3, 0.45)) +
  ggplot2::labs(title  = "Rate of Schools on Semesters",
                x = "Year",
                y = "Rate of schools on semesters") +
  ggplot2::theme_bw() 
graph_gradrate
ggplot2::ggsave(graph_gradrate, file = here::here("02_analysis", "output", "gradrate.png"))  

graph_semester_rate <- ggplot2::ggplot(data = master_by_year, ggplot2::aes(x = year, y = semester_rate)) +
  ggplot2::geom_line() +
  ggplot2::xlim(c(1990, 2010)) +
  ggplot2::ylim(c(0.87, 1.00)) +
  ggplot2::labs(title  = "Four-Year Graduation Rates",
                x = "Year",
                y = "Four-year graduation rates") +
  ggplot2::theme_bw() 
graph_semester_rate
ggplot2::ggsave(graph_semester_rate, file = here::here("02_analysis", "output", "semester_rate.png"))  


## 4.
make_scatter_plot <- function(variable, label){
  data_scatter_plot <- master |> 
    dplyr::mutate("per_men_cohort" = 1 - per_women_cohort)
  variable <- rlang::enquo(variable)
  ggplot2::ggplot(data = data_scatter_plot) +
    ggplot2::geom_point(ggplot2::aes(x = !!variable, y = gradrate4yr)) +
    ggplot2::labs(x = paste0(label),
                  y = "Four-year graduation rate",
                  title = paste0("scatter plot of ", label, " and Four-year graduation rate")) +
    ggplot2::theme_bw() 
}

make_scatter_plot(variable = per_women_cohort, label = "per women cohort")
make_scatter_plot(variable = per_men_cohort, label = "per men cohort")
make_scatter_plot(variable = per_white_cohort, label = "per white cohort")
make_scatter_plot(variable = costs, label = "Total expenditures")
make_scatter_plot(variable = instatetuition, label = "In-state tuition")
