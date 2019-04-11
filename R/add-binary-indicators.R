#' Add binary indicators for a categorical factor variable to a data frame.
#'
#' @param df The input data frame
#' @param variable The variable to split into binary indicators
#'
#' @return Modified data frame with binary indicators added.
#' @export
#'

add_binary_indicators <- function(df, variable) {
  var <- enquo(variable)
  df_tmp <-
    df %>%
    tbl_df() %>%
    # select(!!var) %>%
    mutate(!!quo_name(var) := factor(!!var))
  var_levels <- unique(df_tmp[, quo_name(var)]) %>% pull(!!var) %>% levels()
  newvars <- c()
  for (vv in var_levels) {
    newvar <- paste0(quo_name(var), "_", vv)
    newvars <- c(newvars, newvar)
    df_tmp <-
      df_tmp %>%
      mutate(!!newvar := case_when(
        !!var %in% vv ~ 1,
        !(!!var %in% vv) ~ 0
      ))
  }
  attr(df_tmp, "binary_variables") <- newvars
  df_tmp
}