tg_conc <- grep('^tg\\d+', vars, value = TRUE)
tg_pct <- grep('^pct_tg\\d+', vars, value = TRUE)
tg_totals <- c('TotalTG', 'TAG')
ne_conc <- grep('^ne\\d+', vars, value = TRUE)
ne_pct <- grep('^pct_ne\\d+', vars, value = TRUE)
ne_totals <- c('TotalNE')
outcomes <- c("Waist", "TAG", "HDL", "MeanArtPressure", "Glucose0")
covariates <- c('VN', 'BaseAge', 'BiEthnicity', 'Sex')
