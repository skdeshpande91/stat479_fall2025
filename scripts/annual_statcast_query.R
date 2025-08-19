## Code modified from Bill Petti's original annual Statcast scraper:
# https://billpetti.github.io/2021-04-02-build-statcast-database-rstats-version-3.0/
# Main change is in the column names of the fielders

annual_statcast_query <- function(season) {
  
  data_base_column_types <- 
    readr::read_csv("https://app.box.com/shared/static/q326nuker938n2nduy81au67s2pf9a3j.csv")
  
  dates <- 
    seq.Date(as.Date(paste0(season, '-03-01')),
             as.Date(paste0(season, '-12-01')), 
             by = '4 days')
  
  date_grid <- 
    tibble::tibble(start_date = dates, 
                   end_date = dates + 3)
  
  safe_savant <- 
    purrr::safely(baseballr::scrape_statcast_savant)
  
  payload <- 
    purrr::map(.x = seq_along(date_grid$start_date),
               ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                 payload <- 
                   safe_savant(start_date = date_grid$start_date[.x], 
                               end_date = date_grid$end_date[.x], 
                               type = 'pitcher')
                 return(payload)
               })
  
  payload_df <- purrr::map(payload, 'result')
  
  number_rows <- 
    purrr::map_df(.x = seq_along(payload_df),
                  ~{number_rows <- 
                    tibble::tibble(week = .x, 
                                   number_rows = length(payload_df[[.x]]$game_date))
                  }) |>
    dplyr::filter(number_rows > 0) |>
    dplyr::pull(week)
  
  payload_df_reduced <- payload_df[number_rows]
  
  payload_df_reduced_formatted <- 
    purrr::map(.x = seq_along(payload_df_reduced), 
               ~{cols_to_transform <- 
                 c("pitcher", "fielder_2", "fielder_3",
                   "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                   "fielder_8", "fielder_9")
               df <- 
                 purrr::pluck(payload_df_reduced, .x) |>
                 dplyr::mutate_at(.vars = cols_to_transform, as.numeric) |>
                 dplyr::mutate_at(.vars = cols_to_transform, function(x) {ifelse(is.na(x), 999999999, x)})
               character_columns <- 
                 data_base_column_types |>
                 dplyr::filter(class == "character") |>
                 dplyr::pull(variable)
               numeric_columns <- 
                 data_base_column_types |>
                 dplyr::filter(class == "numeric") |>
                 dplyr::pull(variable)
               integer_columns <- 
                 data_base_column_types |>
                 dplyr::filter(class == "integer") |>
                 dplyr::pull(variable)
               df <- 
                 df |>
                 dplyr::mutate_if(names(df) %in% character_columns, as.character) |>
                 dplyr::mutate_if(names(df) %in% numeric_columns, as.numeric) |>
                 dplyr::mutate_if(names(df) %in% integer_columns, as.integer)
               return(df)
               })
  
  combined <- payload_df_reduced_formatted |>
    dplyr::bind_rows()
  
  return(combined)
}
