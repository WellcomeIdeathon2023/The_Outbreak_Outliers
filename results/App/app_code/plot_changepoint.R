
plot_changepoint <- function(.dataset, .timeframe, .lwr_date, .upr_date, .event){
    .timeframe <- rlang::enquo(.timeframe)
    
    if (rlang::is_missing(.lwr_date)) .lwr_date <- min(.dataset$Daily, na.rm = TRUE)
    if (rlang::is_missing(.upr_date)) .upr_date <- max(.dataset$Daily, na.rm = TRUE)
    
    .dataset <- .subset_by_date(.dataset, .lwr_date, .upr_date)
    
    .chng_point_date <- .find_chngpt_date(.dataset, !!.timeframe, .event)
    
    .aggdataset <- .aggregate_dataset(.dataset, !!.timeframe)
    
    .chng_pt_index <- .find_chngpt_index(.aggdataset, .chng_point_date)
    
    .seg_out <- chng_pt_regression(.aggdataset, .chng_pt_index)
    
    .plot_segmented(.seg_out$dataset, !!.timeframe, .seg_out$break_point_date)
}

.subset_by_date <- function(.dataset, .lwr_date, .upr_date){
    .dataset %>%
        dplyr::filter(dplyr::between(Daily, .lwr_date, .upr_date)) %>%
        dplyr::arrange(Daily)
}

.find_chngpt_date <- function(.dataset, .timeframe, .event){
    .timeframe <- rlang::enquo(.timeframe)
    
    .dataset %>%
        dplyr::filter(Daily == .event) %>%
        dplyr::select(!!.timeframe) %>%
        dplyr::pull(!!.timeframe) %>%
        .[1]
}

.aggregate_dataset <- function(.dataset, .timeframe){
    .timeframe <- rlang::enquo(.timeframe)
    
    .dataset %>%
        dplyr::group_by(!!.timeframe) %>%
        dplyr::summarise(sentiment = mean(sentiment, na.rm = TRUE)) %>%
        dplyr::mutate(x_axis = dplyr::row_number())
}

.find_chngpt_index <- function(.dataset, .chng_point_date){
    date_vec <- 
        .dataset %>%
        dplyr::select(-c(sentiment, x_axis)) %>%
        dplyr::pull()
    which(date_vec == .chng_point_date)
}

chng_pt_regression <- function(.dataset, .chng_pt_index){
    # Get base generalised linear model
    .fit <- glm(sentiment ~ x_axis, data = .dataset, family = 'gaussian')
    
    # Find estimated change point
    .seg_fit <- 
        segmented::segmented(.fit, seg.Z = ~x_axis, psi=.chng_pt_index, npsi = 1)
    
    # Append fitted values to original dataset
    .dataset <- 
        .dataset %>%
        dplyr::mutate(.fitted = .seg_fit$fitted.values)
    
    # Find best breakpoint
    .brk_pt <- .seg_fit$psi[2]
    
    # Find date associated with changepoint
    .brk_dt <- 
        .dataset %>%
        dplyr::select(-c(sentiment, x_axis, .fitted)) %>%
        dplyr::pull() %>%
        .[round(.brk_pt)]
    
    # Find if it is significant
    .p_val <- 
        segmented::davies.test(.fit, 
                               seg.Z =  ~ x_axis, 
                               values = .chng_pt_index)$p.value
    
    
    # Find aapc
    .aapc <- segmented::aapc(.seg_fit, conf.level = 0.95)
    .aapc_est <- format(round(.aapc[1],4), nsmall = 4, scientific = FALSE)
    .aapc_lwr <- format(round(.aapc[3],4), nsmall = 4, scientific = FALSE)
    .aapc_upr <- format(round(.aapc[4],4), nsmall = 4, scientific = FALSE)
    
    list(dataset = .dataset,
         break_point = .brk_pt,
         break_point_date = .brk_dt,
         pval = .p_val, 
         pcntchng = as.character(glue::glue("{.aapc_est} [{.aapc_lwr}, {.aapc_upr}]")))
}

.plot_segmented <- function(.dataset, .timeframe, .brk_dt){
    .timeframe <- rlang::enquo(.timeframe)
    
    .y_brk_dt <- 
        .dataset %>%
        dplyr::filter(!!.timeframe == .brk_dt) %>%
        dplyr::pull(.fitted) %>%
        .[1]
    
    .dataset %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = !!.timeframe, y = sentiment)) +
        ggplot2::geom_point() +
        ggplot2::geom_line(mapping = ggplot2::aes(y = .fitted), linetype = 'dashed') +
        ggplot2::geom_label(x = .brk_dt, y = .y_brk_dt,
                                  label = 'Change point') +
        ggplot2::scale_x_date(date_labels = '%d %b %Y') +
        ggplot2::labs(x = '')
        
}