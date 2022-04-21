library(dplyr)

## code to prepare `DATASET` dataset goes here

dat_bias_values <- rio::import("data-raw/bias_values.csv") %>%
  dplyr::rename_with(~paste0("bias_",.), dplyr::matches("_"))

dat_bias_values[dat_bias_values == "serious"] <- "high"
dat_bias_values$domain <- "all"

usethis::use_data(dat_bias_values, overwrite = TRUE)

dat_ind_values <- rio::import("data-raw/indirectness_values.csv")  %>%
  rename_with(~paste0("ind_",.), dplyr::matches("_"))

dat_ind_values[dat_ind_values == "serious"] <- "high"
dat_ind_values$domain <- "all"

usethis::use_data(dat_ind_values, overwrite = TRUE)

# Load general data

dat_gen <- read.csv("data-raw/dat_gen.csv")

# Create bias dataset

dat_bias <- read.csv("data-raw/ldl_ad_rob.csv",
                    stringsAsFactors = F)

dat_bias[dat_bias == "Serious"] <- "High"

dat_bias <- dat_bias %>%
  left_join(dat_gen) %>%
  mutate(vi = sei^2,
         study = paste("Study",1:nrow(.))) %>%
  # mutate(study = paste(author,year)) %>%
  select(result_id, study, type, yi, vi, everything(), -c(overall,sei,X,year,author)) %>%
  tri_to_long() %>%
  tri_absolute_direction_invert() %>%
  tri_to_wide() %>%
  head(20)

usethis::use_data(dat_bias, overwrite = TRUE)

# Create indirectness dataset

dat_ind <- read.csv("data-raw/ldl_rob_indirect.csv",
                    stringsAsFactors = F)

dat_ind[dat_ind == "Serious"] <- "High"

dat_ind <- dat_ind %>%
  left_join(dat_gen) %>%
  mutate(vi = sei^2,
         study = paste("Study",1:nrow(.))) %>%
  # mutate(study = paste(author,year)) %>%
  select(result_id, study, type, yi, vi, everything(), -c(sei,X,year,author)) %>%
  tri_to_long() %>%
  tri_absolute_direction_invert() %>%
  tri_to_wide() %>%
  head(20)

usethis::use_data(dat_ind, overwrite = TRUE)


