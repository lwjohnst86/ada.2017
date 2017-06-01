# Renaming ----------------------------------------------------------------

renaming_outcomes <- function(x) {
    x %>%
        gsub("MeanArtPressure", "MAP", .) %>%
        gsub("Glucose0", "FG", .) %>%
        gsub("TAG", "Tg", .) %>%
        gsub("Waist", "WC", .)
}

renaming_fats <- function(x) {
    x %>%
        gsub('.*(\\d\\d)(\\d)', '\\1:\\2', .) %>%
        gsub('n(\\d)$', 'n-\\1', .) %>%
        gsub('D(\\d\\d)$', 'D-\\1', .) %>%
        gsub('^pct_', '', .) %>%
        gsub('Total(TG|NE)', 'Total', .) %>%
        gsub('^TAG$', 'Clinical TAG', .)
}
