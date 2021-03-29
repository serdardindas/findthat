#' Find Strings in All Columns
#'
#' @param df input data frame
#' @param ... your strings for search. Must be lowercase
#'
#' @return Filtered Data Frame contains row that your strings looking for.
#' @export
#'
#' @examples
find_words_all <- function(df, ...) {

  columns <- colnames(df)

  words <- as.vector(c(...))

  words_count <- length(as.vector(c(...)))

  result_df <- NULL


  for (i in columns)
  {
    search_column <- df %>% dplyr::pull(i)

    final_df <- NULL



    for (i in 1:words_count)
    {
      dummy_df <-

        df %>%  dplyr::filter(stringr::str_detect(stringr::str_to_lower(search_column), words[i]))

      final_df <- dplyr::bind_rows(final_df, dummy_df)
    }


    result_df <- dplyr::bind_rows(final_df, result_df)
  }


  result_df <- result_df %>%  unique()

  return(result_df)
}
