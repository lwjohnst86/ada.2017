# Tables ------------------------------------------------------------------

#' Create a table of the basic characteristics of PROMISE.
#'
#' @param data Project data.
#' @param caption Table caption.
#'
table_basic <- function(data = project_data, caption = NULL) {
    data %>%
        dplyr::mutate(Ethnicity = ifelse(VN == 0, as.character(Ethnicity), NA),
                      Ethnicity = as.factor(Ethnicity),
                      Sex = ifelse(VN == 0, as.character(Sex), NA),
                      Sex = as.factor(Sex)) %>%
        carpenter::outline_table('f.VN') %>%
        carpenter::add_rows(c('TAG', 'Chol', 'HDL', "MeanArtPressure",
                              'Age', 'BMI', 'Waist'),
                            carpenter::stat_meanSD, digits = 1) %>%
        carpenter::add_rows(c('Ethnicity', 'Sex'), carpenter::stat_nPct, digits = 0) %>%
        carpenter::renaming('rows', renaming_table_rows) %>%
        carpenter::renaming("header", c('Measure', 'Baseline', '3-yr', '6-yr')) %>%
        carpenter::build_table(caption = caption)
}

renaming_table_rows <- function(x) {
    x %>%
        gsub('MeanArtPressure', 'MAP (mmHg)', .) %>%
        gsub('TAG', 'Tg (mmol/L)', .) %>%
        gsub('Chol', 'Chol (mmol/L)', .) %>%
        gsub('LDL', 'LDL (mmol/L)', .) %>%
        gsub('ALT', 'ALT (U/L)', .) %>%
        gsub('HDL', 'HDL (mmol/L)', .) %>%
        gsub('Age', 'Age (yrs)', .) %>%
        gsub('BMI', 'BMI (kg/m^2^)', .) %>%
        gsub('Waist', 'WC (cm)', .)
}
