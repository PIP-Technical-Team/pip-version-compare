compare_tables <- function(df_new, 
                           df_old, 
                           group_cols, 
                           tolerance = 0) {
  # Are the two tables identical
  out <- testthat::compare(df_new, df_old)
  if (out$equal) {
    return(out)
  } else {
    # If different, display the full diff
    out <- compareDF::compare_df(df_new = df_new, 
                                 df_old = df_old, 
                                 change_markers = c("new", "old", "unchanged"),
                                 keep_unchanged_cols = TRUE,
                                 group_col = group_cols,
                                 tolerance = tolerance,
                                 stop_on_error = FALSE)
    out <- compareDF::create_output_table(comparison_output = out, 
                                          output_type = "html",
                                          change_col_name = "table")
    return(out)
  }
}



#' Helper function to compare specific auxiliary tables
#'
#' @param aux character: name of auxiliary table to compare
#' @param version1 character: Data version #1
#' @param version2 character: Data version #2
#' @param server1 character: Server #1
#' @param server2 character: Server #2
#'
#' @return list
#' @export
compare_aux <- function(aux, 
                        version1, 
                        version2, 
                        server1, 
                        server2) {
  aux1 <- get_aux(table = aux, version = version1, server = server1)
  aux2 <- get_aux(table = aux, version = version2, server = server2)
  out <- testthat::compare(aux1, aux2)
  
  return(out)
}

#' Helper function to compare specific auxiliary tables
#'
#' @param aux character: name of auxiliary tables to compare one by one
#' @param version1 character: Data version #1
#' @param version2 character: Data version #2
#' @param server1 character: Server #1
#' @param server2 character: Server #2
#'
#' @return list
#' @export
compare_aux_all <- function(aux, 
                        version1, 
                        version2, 
                        server1, 
                        server2) {
  out <- purrr::map(tables, compare_aux, 
                    version1 = version1, 
                    version2 = version2, 
                    server1 = server1, 
                    server2 = server2)
  names(out) <- aux
  
  out <- purrr::keep(out, ~ .x$equal == FALSE)
  
  return(out)
}