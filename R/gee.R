# Functions for the GEE analysis
#
# Grab or combine data ----------------------------------------------------

#' Prepare the project data for analysis through GEE.
#'
#' @param data project data
#' @export
prep_gee_data <- function(data) {
    data %>%
        dplyr::group_by(VN) %>%
        dplyr::mutate(
            Waist = as.numeric(scale(Waist)),
            ALT = as.numeric(scale(ALT)),
            TAG = as.numeric(scale(TAG)),
            Glucose0 = as.numeric(scale(Glucose0)),
            invHDL = as.numeric(scale(invHDL)),
            HDL = as.numeric(scale(HDL)),
            MeanArtPressure = as.numeric(scale(MeanArtPressure)),
            BaseAge = as.numeric(scale(BaseAge))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(SID, VN)
}

# Analyze -----------------------------------------------------------------

#' Run GEE models on the prepared project data.
#'
#' @param data The project data
#' @param y outcomes (IS, BCF)
#' @param x predictors (TAGFA)
#' @param covariates to adjust for
#' @param intvar interaction variable
#' @param rename_x Function to rename x variables
#' @param rename_y Function to rename y variables
#' @export
analyze_gee <- function(data = project_data,
                        y = outcomes,
                        x = c("Comp1", "Comp2"),
                        covars = covariates,
                        rename_y = renaming_outcomes) {

    data %>%
        mason::design('gee') %>%
        mason::add_settings(family = stats::gaussian(),
                            corstr = 'ar1', cluster.id = 'SID') %>%
        mason::add_variables('yvars', y) %>%
        mason::add_variables('xvars', x) %>%
        mason::add_variables('covariates', covars) %>%
        mason::construct() %>%
        mason::scrub() %>%
        mason::polish_filter('Xterm$', 'term') %>%
        dplyr::mutate(Yterms = rename_y(Yterms)) %>%
        dplyr::mutate(Xterms = factor(
            Xterms,
            levels = c("Comp2", "Comp1"),
            labels = c("C2", "C1"),
            ordered = TRUE
        ))

}

# Plotting ----------------------------------------------------------------

#' Plot the GEE results in a Forest plot style.
#'
#' @param results Results data frame from the GEE analysis
#'
#' @export
plot_gee_main <- function(results) {
    results %>%
        seer::view_main_effect(
            groups = '~Yterms',
            legend.title = 'P-value',
            xlab = 'Points indicate standard deviation difference with 95% CI\nin the outcomes for every 1 unit increase in the cluster',
            ylab = 'PLS clusters of\n TGFA composition',
            group.label.switch = 'both'
            ) +
        graph_theme(ticks = FALSE) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(2)) +
        ggplot2::coord_cartesian(xlim = c(-0.25, 0.25))
}
