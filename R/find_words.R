#' Find One Strings in Specific Column
#'
#' @param df input data frame
#' @param column column that you search your script
#' @param ... your strings for search. Must be lowercase
#'
#' @return Filtered Data Frame contains row that your strings looking for.
#' @export
#'
#' @examples
#'
find_words <- function(df,column,...) {

  words <- as.vector(c(...))

  words_count <- length(as.vector(c(...)))

  search_column <- df %>% dplyr::pull(column)

  final_df <- NULL


      for(i in 1:words_count)
      {
        dummy_df <- df %>%  dplyr::filter(stringr::str_detect(stringr::str_to_lower(search_column),words[i]))

        final_df <- dplyr::bind_rows(final_df,dummy_df)
      }


  final_df <- final_df %>% unique()

  return(final_df)

}
