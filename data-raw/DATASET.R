library(readr)
library(dplyr)
quartet <- read_csv(here::here("data-raw/quartet.csv"), 
                    col_types=cols(
                        set = col_character(),
                        x = col_double(),
                        y = col_double()
                    )) |> 
    mutate(set=factor(set, levels=c("I", "II", "III", "IV")))
usethis::use_data(quartet, overwrite=TRUE)
