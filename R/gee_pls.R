
#' Plots the GEE models using the scores from the PLS modeling.
#'
#' @param pls_results PLS results
#'
plot_pls_gee <- function(pls_results) {
    sid_vn <- project_data %>%
        dplyr::filter(VN == 0) %>%
        dplyr::select_(.dots = c(outcomes, tg_pct, 'SID', covariates)) %>%
        stats::na.omit() %>%
        dplyr::full_join(dplyr::select(project_data, SID, VN)) %>%
        dplyr::select_(.dots = c(outcomes, 'SID', 'VN'))

    gee_pls_fig <- pls_results %>% {
        dplyr::bind_cols(tibble::as_data_frame(.$model$Y),
                         tibble::as_data_frame(unclass(.$scores))[1:2])
        } %>%
        dplyr::rename(Comp1 = `Comp 1`, Comp2 = `Comp 2`) %>%
        dplyr::full_join(sid_vn) %>%
        dplyr::select(SID, VN, Comp1, Comp2) %>%
        dplyr::full_join(project_data) %>%
        dplyr::arrange(SID, VN) %>%
        tidyr::fill(Comp1, Comp2) %>%
        prep_gee_data() %>%
        analyze_gee() %>%
        plot_gee_main() +
        ggplot2::facet_grid( ~ Yterms, scales = 'free_y', space = 'free')

    return(gee_pls_fig)
}
