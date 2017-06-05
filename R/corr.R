# Functions to run the correlation analysis
#
# Analyze -----------------------------------------------------------------

#' Correlation analysis.
#'
#' @param data Project data
#' @param x The covariates and outcomes
#' @param y The fatty acids.
#'
#' @export
corr_tagfa_mets <- function(fa_type = c("mol", "conc")) {
    switch(fa_type, mol = {tagfa <- tg_pct}, conc = {tagfa <- tg_conc})
    project_data %>%
        dplyr::filter(VN == 0) %>%
        mason::design('cor') %>%
        mason::add_settings(method = 'pearson', use = 'complete.obs') %>%
        mason::add_variables('yvars', outcomes) %>%
        mason::add_variables('xvars', tagfa) %>%
        mason::construct() %>%
        mason::scrub() %>%
        dplyr::mutate(Vars2 = renaming_outcomes(Vars2)) %>%
        dplyr::mutate(Vars1 = renaming_fats(Vars1)) %>%
        dplyr::mutate(
            order1 = substr(Vars1, nchar(Vars1), nchar(Vars1)),
            order1 = ifelse(order1 == 0, 10, order1),
            order1 = ifelse(order1 == 'l', 20, order1),
            order1 = as.integer(order1)
        ) %>%
        dplyr::arrange(dplyr::desc(order1)) %>%
        dplyr::select(-order1) %>%
        dplyr::mutate(Vars2 = forcats::fct_inorder(Vars2),
                      Vars1 = forcats::fct_inorder(Vars1),
                      Correlations = round(Correlations, 2))
    }

# Plotting ----------------------------------------------------------------

#' Correlation heatmap plot.
#'
#' @param results Correlation results
#'
#' @export
plot_heatmap <- function(results, text = TRUE, unit = 'nmol/mL') {
     results %>%
        seer::view_heatmap(
            y = 'Vars1',
            x = 'Vars2',
            ylab = paste0('Triacylglycerol fatty acids (', unit, ')'),
            colours = c("#558C89", "#D9853B"),
            number.colours = 5,
            values.text = text,
            values.size = 4) +
        graph_theme(ticks = FALSE, legend.pos = 'right') +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
}
