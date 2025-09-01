#' @import dplyr
#' @import rlang
NULL

#' any_grepl
#'
#' grepl wrapper for any matches in any vector of patterns
#' allows spaces, unlike passing e.g. "pattern1 | pattern2" to grepl directly
#' NA_chracter_ and empty strings are treated as valid patterns and can match
#'
#' @param pattern_vector the vector of strings to look for
#' @param find_in_vector the vector of strings to look in
#' @param ignore.case per base grepl
#' @param perl per base grepl
#' @param fixed per base grepl
#' @param useBytes per base grepl
#' @param return_indices logical, if TRUE returns indices of matches instead of boolean vector
#'
#' @return a boolean vector or integer vector of indices (if return_indices == TRUE)
#' @export
#'
#' @examples
#' any_grepl(c("cat", "shoe"), c("shoe dog", "hat dog", "cat dog"))
any_grepl <- function(pattern_vector, find_in_vector, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE, return_indices = FALSE){

  # Input validation
  if (!is.character(pattern_vector) || !is.character(find_in_vector)) {
    stop("Both pattern_vector and find_in_vector must be character vectors")
  }
  if (length(pattern_vector) == 0) {
    stop("pattern_vector must have length > 0")
  }
  if (length(find_in_vector) == 0) {
    stop("find_in_vector must have length > 0")
  }

  # Separate NA, empty string, and non-empty patterns
  na_pattern <- is.na(pattern_vector)
  empty_pattern <- !na_pattern & pattern_vector == ""
  non_empty_patterns <- pattern_vector[!na_pattern & !empty_pattern]

  # Combine non-empty patterns into a single regex for performance
  combined_pattern <- if (length(non_empty_patterns) > 0) {
    paste(non_empty_patterns, collapse = "|")
  } else {
    character(0)
  }

  # Perform matching
  matches <- rep(FALSE, length(find_in_vector))
  # Match non-empty patterns
  if (length(combined_pattern) > 0) {
    matches[!is.na(find_in_vector)] <- grepl(combined_pattern, find_in_vector[!is.na(find_in_vector)],
                                             ignore.case = ignore.case, perl = perl,
                                             fixed = fixed, useBytes = useBytes)
  }

  # Match NA patterns
  if (any(na_pattern)) {
    matches[is.na(find_in_vector)] <- TRUE
  }

  # Match empty string patterns
  if (any(empty_pattern)) {
    matches[find_in_vector == ""] <- TRUE
  }

  # Return result based on return_indices parameter
  if (return_indices) {
    return(which(matches))
  } else {
    return(matches)
  }
}

#' any_gsub
#'
#' gsub wrapper to replace all matches to a vector of strings with a single replacement value
#' uses fixed = TRUE in gsub and trims whitespace at the end by default
#' NA_character_ and empty strings are treated as valid patterns and can match
#'
#' @param pattern_vector the vector of strings to look for
#' @param replacement_value the string to replace with
#' @param find_in_vector the vector of strings to look in
#' @param trimws_at_end whether to trim white space
#' @param ignore.case per base gsub
#' @param perl per base gsub
#' @param fixed per base gsub
#' @param useBytes per base gsub
#'
#' @return find_in_vector with replacements made
#' @export
#'
#' @examples
#' any_gsub(c("cat", "hat"), "", c("shoe dog", "hat dog", "cat dog"))
any_gsub <- function(pattern_vector, replacement_value = "", find_in_vector, trimws_at_end = FALSE, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {

  # Input validation
  if (!is.character(pattern_vector) || !is.character(find_in_vector)) {
    stop("Both pattern_vector and find_in_vector must be character vectors")
  }
  if (length(pattern_vector) == 0) {
    stop("pattern_vector must have length > 0")
  }
  if (length(find_in_vector) == 0) {
    stop("find_in_vector must have length > 0")
  }

  na_pattern <- is.na(pattern_vector)
  empty_pattern <- !na_pattern & pattern_vector == ""
  non_empty_patterns <- pattern_vector[!na_pattern & !empty_pattern]

  # Combine non-empty patterns into a single regex for performance
  combined_pattern <- if (length(non_empty_patterns) > 0) {
    paste(non_empty_patterns, collapse = "|")
  } else {
    character(0)
  }

  # Perform replacements
  out <- find_in_vector

  # Replace non-empty patterns
  if (length(combined_pattern) > 0) {
    out[!is.na(out)] <- gsub(combined_pattern, replacement_value, out[!is.na(out)],
                             ignore.case = ignore.case, perl = perl,
                             fixed = fixed, useBytes = useBytes)
  }

  # Replace NA patterns
  if (any(na_pattern)) {
    out[is.na(out)] <- replacement_value
  }

  # Replace empty string patterns
  if (any(empty_pattern)) {
    out[out == ""] <- replacement_value
  }

  # Trim whitespace if requested
  if (trimws_at_end) {
    out <- trimws(out)
  }

  return(out)
}

#' strsplit_keep_delim
#'
#' splits a string by a given delimiter and returns as a flat vector of strings, including the delimiters
#' i.e. like strsplit, but adds the delimiters back in
#' allows delimiter to be of length 0, e.g. "", in which case would return with no splits, but will error if string_to_split of length 0
#'
#' @param string_to_split the string to split
#' @param split the delimiter to split by
#'
#' @return a vector of strings
#' @export
#'
#' @examples
#' strsplit_keep_delim("a-b-c", "-")
strsplit_keep_delim <- function(string_to_split, split){
  # previously called strsplit_without_deletion

  # validation
  stopifnot("string_to_split must be a string of length > 0" = (is.character(string_to_split) && nchar(string_to_split) > 0))
  stopifnot("split must be a string" = is.character(split))

  # if split not found, return unchanged
  if (gregexpr(split, string_to_split, fixed = TRUE)[[1]][1] == -1) {
    return(string_to_split)
  }

  # split using strsplit
  split_string <- unlist(strsplit(string_to_split, split, fixed = TRUE))

  out <- character(length(split_string) + length(gregexpr(split, string_to_split, fixed = TRUE)[[1]])) # pre-allocate a character vector with the correct length
  out[c(TRUE, FALSE)] <- split_string # then assign the elements of string_split
  out[c(FALSE, TRUE)] <- split # and the delimiters

  # remove any empty elements
  out <- out[out != ""]

  return(out)
}

#' insert_into_str
#'
#' inserts a string before or after a position in another string, with the position identified by a third string
#' by default inserts before/after every instance of string_to_find but can optionally specify which instances to insert before/after
#'
#' @param string_to_insert_into the string to return, complete with insertions
#' @param string_to_insert the string to be inserted
#' @param string_to_find the string to find to determine the insert location
#' @param position before or after the string_to_find
#' @param instances_to_insert_at instance(s) of string_to_find to insert next to. by default "ALL" but can be any number or vector of numbers <= number of instances.
#' @param strict_mode if TRUE and instance_to_insert_at > the number of instances, results in an error. if FALSE, just inserts for all instances.
#' @param insert_at_end_if_not_found when TRUE and string_to_find not found, string_to_insert will be inserted at the end of string_to_insert_into
#'
#' @return a string, string_to_insert_into, with insertions
#' @export
#'
#' @examples insert_into_str("this is a test string", "new ", "test")
#'
insert_into_str <- function(string_to_insert_into, string_to_insert, string_to_find, position = "before", instances_to_insert_at = "ALL", strict_mode = FALSE, insert_at_end_if_not_found = TRUE){

  # validation
  stopifnot("string_to_insert_into, string_to_insert, and string_to_find must all be strings" = (is.character(string_to_insert_into) && is.character(string_to_insert) && is.character(string_to_find)))
  stopifnot("string_to_insert_into must be longer than 0" = nchar(string_to_insert_into) > 0)
  stopifnot("instances_to_insert_at must be numeric, numeric vector, or 'ALL'" = is.numeric(instances_to_insert_at) || instances_to_insert_at == "ALL")
  stopifnot("position must be before or after" = position == "before" || position == "after")
  stopifnot("strict_mode and insert_at_end_if_not_found must be TRUE or FALSE" = is.logical(strict_mode) && is.logical(insert_at_end_if_not_found))

  # handle cases where string_to_find not found
  if (!grepl(string_to_find, string_to_insert_into)) {
    if (insert_at_end_if_not_found == TRUE) {
      warning(paste0("'", string_to_find, "' not found in '", string_to_insert_into, "'. Inserting at end instead."))
      return(paste0(string_to_insert_into, string_to_insert))
    } else {
      warning(paste0("'", string_to_find, "' not found in '", string_to_insert_into, "'. Returning without insertion."))
      return(string_to_insert_into)
    }
  }

  # split by string_to_find (but keeping string_to_find)
  split_string <- strsplit_keep_delim(string_to_insert_into, string_to_find)
  # find positions in vector of strings matching string_to_find - these are potential positions to insert at
  insert_positions <- which(split_string == string_to_find)

  # get count of instances of string_to_find
  found_positions <- gregexpr(string_to_find, string_to_insert_into, fixed = TRUE)[[1]]

  # define which instances of string_to_find to insert at
  if (all((length(instances_to_insert_at) == 1) & (instances_to_insert_at == "ALL"))) {
    instances_to_insert_at <- seq_along(found_positions)
  } else {
    # handle cases where more instances_to_insert_at than instances found
    if (max(instances_to_insert_at) > length(found_positions)) {
      if (strict_mode == TRUE) {stop("using strict_mode = TRUE and fewer instances of string_to_find than instances_to_insert_at given")} else {
        instances_to_insert_at <- instances_to_insert_at[instances_to_insert_at <= length(found_positions)]
      }
    }
  }
  insert_positions <- insert_positions[instances_to_insert_at]

  # initialize return variable and insert
  out <- split_string
  if (position == "before") {
    out[insert_positions] <- paste0(string_to_insert, string_to_find)
  } else if (position == "after") {
    out[insert_positions] <- paste0(string_to_find, string_to_insert)
  }

  # collapse back into a single string
  out <- paste0(out, collapse = "")

  return(out)
}

#' gsub_by_dict
#'
#' searches in string_to_change for any value in find_column_name in reference_dict and replaces with the associated replacement_column_name value
#' for as many matches as found
#'
#' @param reference_dict either a dataframe with columns with names matching find_column_name and replacement_column_name, or a named list
#' @param string_to_change the string to apply gsub to
#' @param find_column_name required if reference_dict is a data frame: the name of the column to look in for a matching substring
#' @param replacement_column_name required if reference_dict is a data frame: the name of the column to find the associated replacement string
#'
#' @return string_to_change, with all replacements made
#' @export
#'
#' @examples
#' gsub_by_dict(list("apple" = "fruit", "carrot" = "vegetable", "rose" = "flower"),
#'   "I have an apple and a carrot.")
gsub_by_dict <- function(reference_dict, string_to_change, find_column_name = "find_values", replacement_column_name = "replace_values") {

  stopifnot("string_to_change must be a string of length > 0" = (is.character(string_to_change) && nchar(string_to_change) > 0))

  if (is.data.frame(reference_dict)) {
    # If reference_dict is a data frame
    stopifnot("if reference_dict is a data frame, find_column_name and replacement_column_name must match column names" = c(find_column_name, replacement_column_name) %in% names(reference_dict))

    if (!(identical(unique(reference_dict[[find_column_name]]), reference_dict[[find_column_name]]))) {
      warning("Found duplicate values in reference_dict[[find_column_name]]. will only use first match")
    }

    matched_value <- reference_dict[[find_column_name]][sapply(reference_dict[[find_column_name]], function(x) grepl(x, string_to_change, fixed = TRUE))]
    replacement_value <- reference_dict[[replacement_column_name]][sapply(reference_dict[[find_column_name]], function(x) grepl(x, string_to_change, fixed = TRUE))]
  } else if (is.list(reference_dict) && !is.null(names(reference_dict))) {
    # If reference_dict is a named list

    if (!(identical(unique(names(reference_dict)), names(reference_dict)))) {
      warning("Found duplicate values in names(reference_dict). will only use first match")
    }

    matched_value <- names(reference_dict)[sapply(names(reference_dict), function(x) grepl(x, string_to_change, fixed = TRUE))]
    replacement_value <- reference_dict[matched_value]
  } else {
    stop("reference_dict must be either a data frame or a named list.")
  }

  for (i in seq_along(matched_value)) {
    string_to_change <- gsub(matched_value[i], replacement_value[i], string_to_change, fixed = TRUE)
  }

  return(string_to_change)
}

#' query_dict
#'
#' queries a data frame as if a 'dictionary'
#'
#' @param dict a data frame which must include key_label and value_label as column names (can also include other names that aren't used)
#' @param key_label the name of the data frame column to look in for the key
#' @param key the value to look for in the key_label column
#' @param value_label the name of the data frame column to look in for the return value
#' @param .default the value to return if the key can't be found in the key_label column, or if there if the return value has length 0. NA by default.
#'
#' @return the value(s) in the value_label column in the same row(s) as the key value(s) in the key_label column. (only one key value can be given but if there are multiple instances of this value in the key_label column, all associated return values will be returned)
#' @export
#'
#' @examples
#' item_price_lookup <- data.frame(item = c("apple", "carrot"), price = c("£0.20","£0.50"))
#' query_dict(item_price_lookup, "item", "apple", "price")
query_dict <- function(dict, key_label, key, value_label, .default = NA){

  stopifnot("dict must be a data frame where key_label and value_label are column names" = all(is.data.frame(dict) & c(key_label, value_label) %in% names(dict)))

  row_index <- which(dict[key_label] == key)
  col_index <- which(names(dict) == value_label)
  if (length(row_index) == 0 || length(col_index) == 0) {
    return(.default)
  }
  return(dict[row_index, col_index])
}

letters702 <- c(letters, sapply(letters, function(x) paste0(x, letters)))

ASCII_punctuation_and_symbols <- c(" ", ".", ",", "!", ";", ":", "'", "\"", "-", "_", "[", "]", "{", "}", "<", ">", "=", "+", "*", "&", "^", "%", "$", "#", "@", "~", "`", "|", "\\", "/", "?", ")")

#' remove_trailing_characters
#'
#' removes a specified set of characters when found (in any order) at the end of a given string
#'
#' @param string_to_return the string to remove characters from
#' @param characters_to_remove a vector of characters to remove
#'
#' @return the string_to_return with any characters_to_remove removed from the end
#' @export
#'
#' @examples
#' remove_trailing_characters("SC10ab", c("a", "b"))
remove_trailing_characters <- function(string_to_return, characters_to_remove){
  characters <- unlist(strsplit(string_to_return, "", fixed = TRUE))
  last_char_index <- max(which(!characters %in% characters_to_remove))
  return(paste(characters[1:last_char_index], collapse = ""))
}

#' number_to_word_rank
#'
#' accepts a number, returns the respective ranking word up to 50, e.g. 1 returns "first"
#'
#' @param number a numeric value to convert to a ranking word
#'
#' @return a ranking word, e.g. "first"
#' @export
#'
#' @examples
#' number_to_word_rank(1)
number_to_word_rank <- function(number){
  stopifnot("must be numeric between 1 and 50" = is.numeric(number) && number >= 1 && number <= 50)

  ranking_dict <- data.frame(word_rank = c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth",
                                           "eleventh", "twelfth", "thirteenth", "fourteenth", "fifteenth", "sixteenth", "seventeenth", "eighteenth", "nineteenth", "twentieth",
                                           "twentyfirst", "twentysecond", "twentythird", "twentyfourth", "twentyfifth", "twentysixth", "twentyseventh", "twentyeighth", "twentyninth", "thirtieth",
                                           "thirty-first", "thirty-second", "thirty-third", "thirty-fourth", "thirty-fifth", "thirty-sixth", "thirty-seventh", "thirty-eighth", "thirty-ninth", "fortieth",
                                           "forty-first", "forty-second", "forty-third", "forty-fourth", "forty-fifth", "forty-sixth", "forty-seventh", "forty-eighth", "forty-ninth", "fiftieth"),
                         number_rank = 1:50)
  out <- query_dict(ranking_dict, "number_rank", number, "word_rank")

  return(out)
}

#' word_rank_to_number
#'
#' accepts a word number or ranking word, e.g. "one" or "first", and returns the associated numeric, for numbers 1-50
#'
#' @param word_string a word number, e.g. "one" or ranking word, e.g. "first"
#'
#' @return the numeric associated with the given word number or ranking word
#' @export
#'
#' @examples
#' word_rank_to_number("first")
#' word_rank_to_number("one")
word_rank_to_number <- function(word_string){

  # accepts a string expected to contain a word from one to twenty or first to twentieth, and returns as a number
  ranking_dict <- data.frame(word_rank = c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth",
                                           "eleventh", "twelfth", "thirteenth", "fourteenth", "fifteenth", "sixteenth", "seventeenth", "eighteenth", "nineteenth", "twentieth",
                                           "twentyfirst", "twentysecond", "twentythird", "twentyfourth", "twentyfifth", "twentysixth", "twentyseventh", "twentyeighth", "twentyninth", "thirtieth",
                                           "thirty-first", "thirty-second", "thirty-third", "thirty-fourth", "thirty-fifth", "thirty-sixth", "thirty-seventh", "thirty-eighth", "thirty-ninth", "fortieth",
                                           "forty-first", "forty-second", "forty-third", "forty-fourth", "forty-fifth", "forty-sixth", "forty-seventh", "forty-eighth", "forty-ninth", "fiftieth",

                                           "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
                                           "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty",
                                           "twentyone", "twentytwo", "twentythree", "twentyfour", "twentyfive", "twentysix", "twentyseven", "twentyeight", "twentynine", "thirty",
                                           "thirty-one", "thirty-two", "thirty-three", "thirty-four", "thirty-five", "thirty-six", "thirty-seven", "thirty-eight", "thirty-nine", "forty",
                                           "forty-one", "forty-two", "forty-three", "forty-four", "forty-five", "forty-six", "forty-seven", "forty-eight", "forty-nine", "fifty"),

                             number_rank = rep(1:50, 2))

  stopifnot("must be number word e.g. one or ranking word e.g. first associated with a number between 1 and 50" = word_string %in% ranking_dict$word_rank)

  out <- as.numeric(ranking_dict$number_rank[ranking_dict$word_rank == word_string])
  return(out)
}

#' check_for_common_alphabets
#'
#' returns a comma delineated string of common alphabet names recognised in the input_string
#' looks for Latin, Cyrillic, Arabic, CJK, Devanagari and Bengali
#'
#' @param input_string a string
#'
#' @return a comma delineated string of common alphabet names recognised in the input_string
#' @export
#'
#' @examples
#' check_for_common_alphabets("Latin, Русский, عربي, 漢語, বাংলা, देवनागरी")
check_for_common_alphabets <- function(input_string){
  alphabets <- c()
  for (char in utf8ToInt(input_string)) {
    if (char >= 0x0000 && char <= 0x007F || char >= 0x0080 && char <= 0x00FF || char >= 0x0100 && char <= 0x017F || char >= 0x0180 && char <= 0x024F || char >= 0x1E00 && char <= 0x1EFF) {alphabets[['Latin']] <- TRUE}
    if (char >= 0x0400 && char <= 0x04FF) {alphabets[['Cyrillic']] <- TRUE}
    if (char >= 0x0600 && char <= 0x06FF || char >= 0xFB50 && char <= 0xFDFF || char >= 0xFE70 && char <= 0xFEFE) {alphabets[['Arabic']] <- TRUE}
    if (char >= 0x4E00 && char <= 0x9FFF || char >= 0x3400 && char <= 0x4DBF || char >= 0x20000 && char <= 0x2A6DF || char >= 0x2A700 && char <= 0x2B73F || char >= 0x2B740 && char <= 0x2B81F || char >= 0x2B820 && char <= 0x2CEAF || char >= 0xF900 && char <= 0xFAFF) {alphabets[['Chinese']] <- TRUE}
    if (char >= 0x0900 && char <= 0x097F) {alphabets[['Devanagari']] <- TRUE}
    if (char >= 0x0980 && char <= 0x09FF) {alphabets[['Bengali']] <- TRUE}
  }

  out <- paste0(names(alphabets), collapse = ", ")

  return(out)
}

#' null_to_NA
#'
#' accepts a list and converts all NULLs in the list to NAs
#'
#' @param x a vector to sapply through
#'
#' @return the input list, with all NULLs converted to NAs
#' @export
#'
#' @examples
#' null_to_NA(c(NULL, 1, 2, 3, NULL))
null_to_NA <- function(x){
  #
  x[sapply(x, is.null)] <- NA
  return(x)
}

#' sub_char_by_index
#'
#' accepts a string and replaces just the character(s) at a given index with a specified string
#' index must be a single numeric or range of consecutive numerics
#' if indexes given a range spanning multiple character positions, replaces with only one instance of the replacement string
#' replacement string can be "", removing the character(s) at the given index
#'
#' @param string the string to replace within
#' @param index the index of the character(s) in the given string to replace
#' @param replacement the replacement character(s)
#'
#' @return the string with the character(s) at the given index replaced
#' @export
#'
#' @examples sub_char_by_index("testing", 3, "x")
sub_char_by_index <- function(string, index, replacement){

  stopifnot("index must be single numeric position or consecutive range" = is.numeric(index) && ((length(index) == 1) || (max(index) - min(index) + 1 == length(index))))
  stopifnot("string must be nchar > 0" = nchar(string) > 0)

  return(paste0(substr(string, 1, min(index) - 1), replacement, substr(string, max(index) + 1, nchar(string))))
}

#' closest_matching_string
#'
#' returns closest matching string from strings_vector to string_to_check
#'
#' @param strings_vector vector of strings
#' @param string_to_check single string to measure distance against
#' @param method optionally specify a distance method. method = lv by default.
#'
#' @return string from strings_vector that best matches string_to_check
#' @export
#'
#' @examples
#' closest_matching_string(c("shoe", "dog", "cat"), "dug")
closest_matching_string <- function(strings_vector, string_to_check, method = "lv") {

  stopifnot("strings_vector and string_to_check must be strings, and latter must be just a single string" = is.character(strings_vector) && is.character(string_to_check) && length(string_to_check))

  distances <- stringdist::stringdistmatrix(string_to_check, strings_vector, method = method)
  if (length(distances[distances == min(distances)]) > 1) {
    warning("Multiple strings with the same distance found. Returning the first instance.")
  }
  min_index <- which.min(distances)
  return(strings_vector[min_index])
}

#' labelled_map_dfc
#'
#' simple wrapper around map_dfc to reapply the original variable labels (these are usually dropped)
#'
#' @param .x dataframe
#' @param .f function
#' @param ... function arguments
#'
#' @return .x transformed to have applied .f to each column per purrr::map_dfc, but with variable labels reapplied at the end
#' @export
#'
#' @examples
#' labelled_map_dfc(get_minimal_labelled_test_dat(), function(x) ifelse(is.na(x), 0, x))
labelled_map_dfc <- function(.x, .f, ...){

  stopifnot(".x must be a data frame and .f a function" = is.data.frame(.x) && is.function(.f))

  .x_label <- sapply(.x, attr, "label")
  .x_labels <- sjlabelled::get_labels(.x, values = "as.name")

  .f <- purrr::as_mapper(.f, ...)
  res <- purrr::map(.x, .f, ...)
  out <- dplyr::bind_cols(res)

  .x_label_not_null <- !sapply(.x_label, is.null)
  for (i in names(.x)[.x_label_not_null]) {
    attr(out[[i]], "label") <- unlist(unname(.x_label[i]))
  }

  if (any(sapply(.x_labels, is.null))) {
    out <- sjlabelled::set_labels(out, labels = .x_labels)
  }

  return(out)
}

#' value_from_labels
#'
#' given a vector with value labels, returns the value associated with the label_to_find
#' returns NA if label not found
#'
#' @param labelled_var a vector with value labels
#' @param label_to_find string corresponding to one of the value labels for labelled_var
#'
#' @return the value associated with the label_to_find
#' @export
#'
#' @examples
#' labelled_test_dat <- get_minimal_labelled_test_dat()
#' value_from_labels(labelled_test_dat$csat, "Neutral")
value_from_labels <- function(labelled_var, label_to_find){

  stopifnot("labelled_var must be numeric" = is.numeric(labelled_var))

  labels_found <- sjlabelled::get_labels(labelled_var, values = "as.name")
  out <- as.numeric(names(labels_found)[labels_found %in% label_to_find])
  if (length(out) == 0) { out <- NA }

  return(out)

}

#' merge_left
#'
#' a dplyr left_join, but when there is a column in x and y, attempts to merge instead of creating a duplicate column
#' i.e.
#' if x already has a value for a given id and is in y, use x (by default, with overwrite == FALSE)
#' if x id not in y then keep x value
#' if y id not in x then add y value
#' optionally with overwrite == TRUE, y values will replace x values
#'
#' @param x a data frame to join to
#' @param y the data frame to join with
#' @param by the id by which to join
#' @param overwrite boolean. if TRUE, y values will replace x values. if FALSE, y will fill in gaps in x values but not overwrite.
#'
#' @return the joined and merged data frame
#' @export
#'
#' @examples
#' df1 <- data.frame(id = c(1, 2, 3), x = c("a", "b", "c"), y = c(10, 20, 30))
#' df2 <- data.frame(id = c(1, 2, 4), x = c("A", "B", "D"), z = c(100, 200, 400))
#' merge_left(df1, df2, by = "id", overwrite = TRUE)
merge_left <- function(x, y, by, overwrite = FALSE){

  stopifnot("x and y must be data frames" = is.data.frame(x), is.data.frame(y))
  stopifnot("by must be a string matching a column name common to x and y" = (is.character(by) && length(by) == 1 && by %in% names(x) && by %in% names(y)))
  stopifnot("overwrite must be TRUE or FALSE" = is.logical(overwrite), length(overwrite) == 1)

  names_in_x_and_y <- names(x)[names(x) %in% names(y) & (names(x) != by)]

  temp_joined <- x %>% dplyr::left_join(y, by = by)

  # merge logic
  names_x_suffix <- paste0(names_in_x_and_y, ".x")
  names_y_suffix <- paste0(names_in_x_and_y, ".y")

  for (i in seq_along(names_in_x_and_y)) {

    if (overwrite == TRUE) {
      temp_joined[[names_in_x_and_y]] <- ifelse(is.na(temp_joined[[names_y_suffix[i]]]),
                                 temp_joined[[names_x_suffix[i]]],
                                 temp_joined[[names_y_suffix[i]]])
    } else if (overwrite == FALSE) {
      temp_joined[[names_in_x_and_y]] <- ifelse(is.na(temp_joined[[names_x_suffix[i]]]),
                                 temp_joined[[names_y_suffix[i]]],
                                 temp_joined[[names_x_suffix[i]]])
    }
  }

  # remove unmerged versions of variables with suffixes
  vars_to_remove <- c(paste0(names_in_x_and_y, ".x"), paste0(names_in_x_and_y, ".y"))
  temp_joined <- temp_joined %>% dplyr::select(-dplyr::all_of(vars_to_remove))

  # reorder
  temp_joined <- temp_joined %>% dplyr::select(names(x)[names(x) %in% names(temp_joined)],
                                        names(temp_joined)[!names(temp_joined) %in% names(x)])

  return(temp_joined)
}

#' check_key_consistency
#'
#' checks that values in all other columns for every unique value of key_name are the same for every instance of that value of key_name
#' if false, returns a data frame to help diagnose inconsistencies
#' note: doesn't check uniqueness of key_name, i.e. there can be multiple (duplicate) instances of the same value of key_name
#'
#' @param temp_dat dataframe to check in
#' @param key_name unique key
#'
#' @return TRUE if passes consistency checks, else returns a data frame to help diagnose inconsistencies
#' @export
#'
#' @examples
#' one_inconsistent_df <- data.frame(id = c(1, 1, 2, 2, 3),
#'   value1 = c("A", "A", "B", "C", "D"),
#'   value2 = c(10, 10, 20, 20, 30))
#' concatenate_by_group(one_inconsistent_df, "id")
check_key_consistency <- function(temp_dat, key_name){

  stopifnot("Key name must be a string matching a name in the data frame" = is.data.frame(temp_dat) && is.character(key_name) && length(key_name) == 1 && key_name %in% names(temp_dat))
  # Check for consistency in other columns for each unique key
  problem_matrix <- temp_dat %>%
    dplyr::group_by(!! rlang::sym(key_name)) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ dplyr::n_distinct(.) > 1))

  # If there are no non-unique columns, the key is consistent
  if (!any(problem_matrix[, setdiff(names(problem_matrix), key_name)])) {
    return(TRUE)
  } else {
    # Return a data frame containing only the problematic rows and columns
    col_indices <- which(colSums(problem_matrix) > 0)
    row_indices <- which(temp_dat[[key_name]] %in% problem_matrix[[key_name]][unlist(lapply(dplyr::select(problem_matrix[col_indices], -all_of(key_name)), which))])
    problematic_data <- temp_dat[row_indices, col_indices]

    return(list(status = "Key is inconsistent", problematic_data = problematic_data))
  }
}

#' concatenate_by_group
#'
#' returns a data frame grouped by group_var where in case of multiple rows within a group, the value for that group is a concatenation of unique values for those rows
#'
#' @param temp_dat a dataframe
#' @param group_var the name of the grouping variable
#'
#' @importFrom rlang .data
#' @return the grouped dataframe with concatenated values
#' @export
#'
#' @examples
#' temp_dat <- dplyr::tibble(
#'   ID = c(1, 1, 2, 2, 3),
#'   Name = c("John", "John", "Alice", "Alice", "Bob"),
#'   City = c("New York", "Los Angeles", "Chicago", "Chicago", "Houston"),
#'   Age = c(25, 25, 30, 30, 35)
#' )
#' concatenate_by_group(temp_dat, "ID")
concatenate_by_group <- function(temp_dat, group_var){

  if(!group_var %in% names(temp_dat)) {
    stop("ID variable does not exist in the dataframe")
  }

  # index to retain order
  temp_dat$row_id <- seq_len(nrow(temp_dat))

  temp_dat <- temp_dat %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) %>%
    dplyr::mutate(first_row_id = dplyr::first(.data$row_id)) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(names(temp_dat)[!names(temp_dat) %in% group_var]))) %>% # Arrange all except ID for more consistent ordering of concatenated values in string
    dplyr::summarise(dplyr::across(dplyr::all_of(names(temp_dat)[!names(temp_dat) %in% group_var & names(temp_dat) != "row_id"]),
                                   ~paste(unique(.), collapse = ", "), .names = "concat_{.col}"),
                     first_row_id = dplyr::first(.data$first_row_id),
                     .groups = 'drop') %>%
    dplyr::ungroup()

  # Arrange by row_id and remove temporary columns
  temp_dat <- temp_dat %>% dplyr::arrange(.data$first_row_id) %>% dplyr::select(-.data$first_row_id)

  return(temp_dat)

}

#' puncts_to_pattern
#'
#' Convert punctuation vector to regex pattern
#'
#' Takes a vector of punctuation characters or strings (allowing whitespace) and returns a regex-safe pattern
#' that matches any one of them. Escapes special regex characters and whitespace.
#'
#' @param puncts Character vector of punctuation marks
#' @return A string suitable for use in regex pattern matching
puncts_to_pattern <- function(puncts) {
  # Characters that need escaping in regex
  special_chars <- c(".", "|", "(", ")", "[", "]", "{", "}", "^", "$", "*", "+", "?", "\\", " ")

  # Split into single chars and multi-char strings
  single_chars <- puncts[nchar(puncts) == 1]
  multi_chars <- puncts[nchar(puncts) > 1]

  # Process single characters
  if (length(single_chars) > 0) {
    escaped_singles <- sapply(single_chars, function(p) {
      if(p == " ") "\\s"
      else if(p %in% special_chars) paste0("\\", p)
      else p
    })
    singles_pattern <- if (length(escaped_singles) > 0) {
      sprintf("[%s]", paste0(escaped_singles, collapse = ""))
    }
  }

  # Process multi-character strings
  if (length(multi_chars) > 0) {
    escaped_multis <- sapply(multi_chars, function(p) {
      # Escape any special regex characters within the string
      chars <- strsplit(p, "")[[1]]
      escaped_chars <- sapply(chars, function(c) {
        if(c == " ") "\\s"
        else if(c %in% special_chars) paste0("\\", c)
        else c
      })
      paste0(escaped_chars, collapse = "")
    })
    multis_pattern <- paste0("(", paste(escaped_multis, collapse = "|"), ")")
  }

  # Combine patterns
  if (length(single_chars) == 0) return(multis_pattern)
  if (length(multi_chars) == 0) return(singles_pattern)
  paste0("(", singles_pattern, "|", multis_pattern, ")")

}

#' Reorder Substrings Within a String
#'
#' Takes a string with multiple parts separated by a specified separator and reorders
#' those parts according to a given order vector.
#'
#' @param string A character string containing substrings separated by a separator
#' @param separator A character string specifying the separator between substrings. Defaults to " - "
#' @param new_order A numeric vector specifying the new order of substrings. Defaults to c(1, 3, 2)
#'
#' @return A character string with substrings reordered according to new_order.
#'         If the number of substrings doesn't match the length of new_order,
#'         returns the original string unchanged with a warning.
#' @export
#'
#' @examples
#' reorder_substrings("A - B - C", " - ", c(1, 3, 2))
#' reorder_substrings("Question - Statement - Brand", " - ", c(3, 1, 2))
reorder_substrings <- function(string, separator = " - ", new_order = c(1, 3, 2)){
  parts <- strsplit(string, separator, fixed = TRUE)[[1]]

  if(length(parts) != length(new_order)){
    warning(paste0("Number of substrings found does not match length of new_order vector for ", string, ". returning string unchanged."))
    return(string)
  }

  reordered <- parts[new_order]
  return(paste(reordered, collapse = separator))
}

#' Get Unique Substrings Containing a Specified String
#'
#' Searches through character strings, splits them by a separator, and returns
#' unique substrings that contain a specified search string.
#'
#' @param full_string A character vector of strings to search through
#' @param string_to_find A character string to search for within the substrings
#' @param separator A character string specifying the separator to split by. Defaults to " - "
#'
#' @return A character vector of unique substrings that contain the specified string.
#'         Returns NULL if no matches are found.
#' @export
#'
#' @examples
#' strings <- c("Q1 - Brand A - Statement", "Q2 - Brand B - Statement", "Q3 - Brand A - Other")
#' get_unique_substrings_containing_string(strings, "Brand A")
#' get_unique_substrings_containing_string(strings, "Statement")
get_unique_substrings_containing_string <- function(full_string, string_to_find, separator = " - ") {
  result <- unique(unlist(lapply(full_string, function(x) {
    parts <- unlist(strsplit(x, separator, fixed = TRUE))
    matches <- parts[grepl(string_to_find, parts, fixed = TRUE)]
    if (length(matches) > 0) matches else NULL
  })))
  return(result)
}

#' Find All Duplicated Values in a Vector
#'
#' Identifies all values that appear more than once in a vector, including both
#' the first and subsequent occurrences (unlike base R's duplicated() which only
#' identifies subsequent occurrences).
#'
#' @param x A vector to check for duplicates
#' @param return_type A character string specifying the return format:
#'        "logical" returns a logical vector, "indices" returns numeric indices,
#'        "values" returns the actual duplicated values
#'
#' @return Depending on return_type:
#'         - "logical": A logical vector the same length as x
#'         - "indices": A numeric vector of indices where duplicates occur
#'         - "values": A vector of the actual duplicated values
#' @export
#'
#' @examples
#' x <- c("a", "b", "c", "b", "d", "a")
#' all_duplicated(x, "logical")
#' all_duplicated(x, "indices")
#' all_duplicated(x, "values")
all_duplicated <- function(x, return_type = "logical") {
  duplicated_logical <- duplicated(x) | duplicated(x, fromLast = TRUE)
  if(return_type == "logical") {
    return(unlist(duplicated_logical))
  } else if (return_type == "indices") {
    return(which(duplicated_logical))
  } else if (return_type == "values") {
    return(x[which(duplicated_logical)])
  } else {
    stop("Invalid return_type. Use 'logical', 'indices', or 'values'.")
  }
}

#' Append "oe" to Variable Labels for Open-Ended Duplicates
#'
#' Identifies character variables in survey data that have duplicate variable labels
#' with their non-open-ended counterparts and appends "oe" to the variable name prefix
#' in their labels to distinguish them.
#'
#' @param temp_dat A survey data dataframe
#' @param temp_dpdict A dpdict dataframe with variable metadata including variable_labels
#'
#' @return The updated dpdict with "oe" appended to variable labels for open-ended
#'         duplicates. Variable labels are modified from format "Q1 - Statement" to
#'         "Q1oe - Statement" for affected variables.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assumes temp_dat has character variables with duplicate labels
#' temp_dat <- data.frame(
#'   Q1_1 = c(1, 2, 3),
#'   Q1_1_oe = c("Other text", "More text", "Additional text")
#' )
#' attr(temp_dat$Q1_1, "label") <- "Q1 - Brand preference"
#' attr(temp_dat$Q1_1_oe, "label") <- "Q1 - Brand preference"
#'
#' temp_dpdict <- create_dict(temp_dat)
#' updated_dpdict <- append_oe_to_oe_duplicates(temp_dat, temp_dpdict)
#' }
append_oe_to_oe_duplicates <- function(temp_dat, temp_dpdict){
  # gets all variable labels that are oes with labels that are duplicates of the non oe version
  duplicates_to_oes <- unname(all_duplicated(temp_dpdict$variable_labels) & unlist(map_vec(temp_dat, is.character)))
  # appends "oe" to variable name prefix for these variables
  temp_dpdict$variable_labels[duplicates_to_oes] <- gsub("^([A-Za-z0-9]+)( - )", "\\1oe\\2", temp_dpdict$variable_labels[duplicates_to_oes])
  return(temp_dpdict)
}

#' Null-coalescing operator `%||%`
#'
#' Returns the first argument that is **not** `NULL`.
#' It is handy for providing concise “default-value” fall-backs:
#' `x %||% y` reads as *“give me `x`, otherwise `y`”*.
#'
#' @usage a \%||\% b
#'
#' @param a,b Objects to test. If `a` is not `NULL`, it is returned;
#'   otherwise `b` is returned unchanged.
#'
#' @return `a` when `!is.null(a)`; otherwise `b`.
#'
#' @examples
#' NULL %||% 10        #> 10
#' ""   %||% "default" #> ""
#' 1:3  %||% 0         #> 1 2 3
#'
#' @export
#' @name %||%
`%||%` <- function(a, b) if (!is.null(a)) a else b


#' Find and replace in variable labels.
#' @param df A dataframe with variables that may have `label` attributes.
#' @param string_to_find Character vector of strings to find in labels.
#' @param string_to_replace_with Single string to replace matches with.
#' @param trimws_at_end Logical. Whether to trim whitespace after replacement.
#' @param ignore.case Logical. Whether to ignore case when matching.
#' @param perl Logical. Whether to use Perl-compatible regular expressions.
#' @param fixed Logical. Whether to treat patterns as fixed strings.
#' @param useBytes Logical. Whether to use byte-wise matching.
#' @return A dataframe with updated `label` attributes
#' @examples
#' df <- data.frame(a = 1:3)
#' attr(df$a, "label") <- "Example   label   with   spaces"
#' df <- find_and_replace_in_var_labels(df, " {2,}", " ")
#' attr(df$a, "label")  # "Example label with spaces"
#' @export
find_and_replace_in_var_labels <- function(df, string_to_find, string_to_replace_with,
                                           trimws_at_end = FALSE, ignore.case = FALSE,
                                           perl = FALSE, fixed = FALSE, useBytes = FALSE) {

  # Extract all labels at once
  all_labels <- vapply(df, function(x) {
    lbl <- attr(x, "label")
    if (is.null(lbl)) NA_character_ else lbl
  }, character(1))

  # Find which variables have labels
  has_labels <- !is.na(all_labels)

  if (any(has_labels)) {
    # Apply any_gsub to all labels at once
    new_labels <- any_gsub(string_to_find, string_to_replace_with, all_labels[has_labels],
                           trimws_at_end = trimws_at_end, ignore.case = ignore.case,
                           perl = perl, fixed = fixed, useBytes = useBytes)

    # Reassign updated labels
    var_names_with_labels <- names(df)[has_labels]
    for (i in seq_along(var_names_with_labels)) {
      attr(df[[var_names_with_labels[i]]], "label") <- new_labels[i]
    }
  }

  return(df)
}


#' Convert Integer Variables to Double While Preserving Labels
#'
#' Converts integer variables to double storage type while preserving all
#' labelling attributes. Useful for ensuring consistent variable types when
#' integer vs double differences cause issues with grouping functions.
#'
#' @param x A vector, data frame, or survey_data object
#' @param labelled_only Logical. If TRUE, only converts integers that have labels.
#'        If FALSE, converts all integers. Default TRUE.
#'
#' @return Object of same type as input with integers converted to doubles
#' @export
#'
#' @examples
#' # For individual vector
#' int_var <- structure(c(1L, 2L, 3L),
#'                      class = c("haven_labelled", "vctrs_vctr", "integer"),
#'                      labels = c("Low" = 1, "High" = 3))
#' double_var <- convert_integers_to_double(int_var)
#'
#' # For data frame
#' df <- data.frame(x = 1:3, y = c(1.1, 2.2, 3.3))
#' df_converted <- convert_integers_to_double(df)
convert_integers_to_double <- function(x, labelled_only = TRUE) {
  UseMethod("convert_integers_to_double")
}

#' @export
convert_integers_to_double.default <- function(x, labelled_only = TRUE) {
  # Check if it's an integer
  if (!is.integer(x)) {
    return(x)
  }

  # If labelled_only is TRUE, check if variable has labels
  if (labelled_only && !sjlabelled::is_labelled(x)) {
    return(x)
  }

  # Store all attributes
  attrs <- attributes(x)

  # Convert to double
  x_double <- as.double(x)

  # Restore attributes, updating class if needed
  if (!is.null(attrs)) {
    attributes(x_double) <- attrs

    # Update class attribute to replace "integer" with "double"
    if ("class" %in% names(attrs)) {
      new_class <- gsub("integer", "double", attrs$class)
      class(x_double) <- new_class
    }
  }

  return(x_double)
}

#' @export
convert_integers_to_double.data.frame <- function(x, labelled_only = TRUE) {
  # Apply to each column
  x[] <- lapply(x, convert_integers_to_double, labelled_only = labelled_only)
  return(x)
}

#' @export
convert_integers_to_double.survey_data <- function(x, labelled_only = TRUE) {
  x$dat <- convert_integers_to_double(x$dat, labelled_only = labelled_only)
  return(x)
}


#' Bind columns while preserving metadata
#'
#' Wrapper around dplyr::bind_cols that preserves per-column metadata, in
#' particular variable labels (attr(, "label")) and value labels (attr(, "labels")).
#' Any other simple attributes (except structural ones like names/class/dim) on
#' the input columns are also preserved on the corresponding output columns.
#'
#' This function masks dplyr::bind_cols and is intended to be a drop-in
#' replacement. It accepts the same signature and passes all arguments through
#' to dplyr.
#'
#' @inheritParams dplyr::bind_cols
#' @return A data frame, as from dplyr::bind_cols, with metadata reapplied
#' @export
bind_cols <- function(...) {

  # Capture evaluated arguments to forward and to inspect metadata
  arg_list <- list(...)

  # Track if any survey_data objects were provided and keep first dpdict
  any_survey <- FALSE
  base_dpdict <- NULL

  # Helper to unwrap survey_data inputs for binding while tracking dpdict
  unwrap_for_bind <- function(x) {
    if (inherits(x, "survey_data")) {
      if (!any_survey) {
        any_survey <<- TRUE
        base_dpdict <<- x$dpdict
      }
      return(x$dat)
    }
    x
  }

  # Transform arguments for forwarding
  if (length(arg_list) == 1L && is.list(arg_list[[1L]]) && !is.data.frame(arg_list[[1L]])) {
    inner <- arg_list[[1L]]
    inner <- lapply(inner, unwrap_for_bind)
    arg_list_call <- list(inner)
    names(arg_list_call) <- names(arg_list)
  } else {
    arg_list_call <- lapply(arg_list, unwrap_for_bind)
    names(arg_list_call) <- names(arg_list)
  }

  # Build a list of per-output-column attributes in bind order (scan transformed inputs)
  col_attrs <- list()
  for (item in arg_list_call) {
    if (is.null(item)) next
    if (is.list(item) && !is.data.frame(item)) {
      for (sub in item) {
        if (is.data.frame(sub)) {
          for (nm in names(sub)) {
            col_attrs[[length(col_attrs) + 1L]] <- attributes(sub[[nm]])
          }
        } else {
          col_attrs[[length(col_attrs) + 1L]] <- attributes(sub)
        }
      }
    } else if (is.data.frame(item)) {
      for (nm in names(item)) {
        col_attrs[[length(col_attrs) + 1L]] <- attributes(item[[nm]])
      }
    } else {
      col_attrs[[length(col_attrs) + 1L]] <- attributes(item)
    }
  }

  # Delegate binding to dplyr with transformed arguments
  out <- do.call(dplyr::bind_cols, arg_list_call)

  # Reapply captured attributes by column position
  excluded <- c("names", "class", "dim", "dimnames", "row.names", "tsp")
  n_apply <- min(length(col_attrs), ncol(out))
  if (n_apply > 0) {
    for (i in seq_len(n_apply)) {
      attrs <- col_attrs[[i]]
      if (is.null(attrs)) next

      # Variable label
      if (!is.null(attrs$label)) {
        attr(out[[i]], "label") <- attrs$label
      }

      # Value labels
      if (!is.null(attrs$labels)) {
        out[[i]] <- sjlabelled::set_labels(out[[i]], labels = attrs$labels)
      }

      # Other non-structural attributes
      other_names <- setdiff(names(attrs), c(excluded, "label", "labels"))
      for (an in other_names) {
        attr(out[[i]], an) <- attrs[[an]]
      }
    }
  }

  # If any survey_data inputs were provided, update dpdict accordingly and wrap
  if (isTRUE(any_survey)) {
    # Use mutate.survey_data to add columns so dpdict is updated by existing utilities
    # Base survey_data is the first survey_data argument encountered
    base_survey <- NULL
    for (orig in list(...)) {
      if (inherits(orig, "survey_data")) { base_survey <- orig; break }
    }
    # Build a RHS data frame via bind_cols to apply name repair and collect columns from other args
    rhs_args <- list()
    for (i in seq_along(arg_list_call)) {
      obj <- arg_list_call[[i]]
      # Skip the first survey_data (already in base_survey)
      if (inherits(list(...)[[i]], "survey_data")) {
        if (identical(list(...)[[i]], base_survey)) next else obj <- obj
      }
      rhs_args[[length(rhs_args) + 1L]] <- obj
    }
    rhs_df <- if (length(rhs_args) > 0) do.call(dplyr::bind_cols, rhs_args) else data.frame()

    # Splice columns into mutate to trigger mutate.survey_data metadata update
    res <- dplyr::mutate(base_survey, !!!as.list(rhs_df))
    return(res)
  }

  out
}


#' Bind rows while preserving metadata
#'
#' Wrapper around dplyr::bind_rows that preserves column metadata across inputs,
#' preferring metadata from the first dataset on conflicts and emitting a single
#' aggregated warning. Preserved metadata includes variable labels
#' (attr(, "label")) and value labels (attr(, "labels")). Other simple
#' attributes (excluding structural ones like names/class/dim) are also
#' preserved based on the first dataset in which the column appears.
#'
#' This function masks dplyr::bind_rows and is intended to be a drop-in
#' replacement. It accepts the same signature and passes all arguments through
#' to dplyr.
#'
#' @inheritParams dplyr::bind_rows
#' @return A data frame, as from dplyr::bind_rows, with metadata reconciled and reapplied
#' @export
bind_rows <- function(...) {

  # Capture evaluated arguments to forward
  arg_list <- list(...)

  # Track if any survey_data objects were provided and keep first dpdict
  any_survey <- FALSE
  base_dpdict <- NULL

  unwrap_for_bind <- function(x) {
    if (inherits(x, "survey_data")) {
      if (!any_survey) {
        any_survey <<- TRUE
        base_dpdict <<- x$dpdict
      }
      return(x$dat)
    }
    x
  }

  # Transform arguments for forwarding
  if (length(arg_list) == 1L && is.list(arg_list[[1L]]) && !is.data.frame(arg_list[[1L]])) {
    inner <- arg_list[[1L]]
    inner <- lapply(inner, unwrap_for_bind)
    arg_list_call <- list(inner)
    names(arg_list_call) <- names(arg_list)
  } else {
    arg_list_call <- lapply(arg_list, unwrap_for_bind)
    names(arg_list_call) <- names(arg_list)
  }

  # Build a flat list of data frames to inspect (respect both variadic and list-input forms) from transformed args
  df_list <- list()
  if (length(arg_list_call) == 1L && is.list(arg_list_call[[1L]]) && !is.data.frame(arg_list_call[[1L]])) {
    df_list <- arg_list_call[[1L]]
  } else {
    nms <- names(arg_list_call)
    for (i in seq_along(arg_list_call)) {
      nm <- if (length(nms)) nms[[i]] else ""
      if (!is.null(nm) && nm %in% c(".id", ".name_repair")) next
      df_list[[length(df_list) + 1L]] <- arg_list_call[[i]]
    }
  }

  # Metadata accumulator by column name
  meta_map <- new.env(parent = emptyenv())
  label_conflicts <- character(0)
  vlabel_conflict_vars <- character(0)

  # Helper to merge value labels with first-dataset precedence
  merge_value_labels <- function(base_map, add_vec) {
    if (is.null(add_vec) || length(add_vec) == 0) return(list(map = base_map, conflicted = FALSE))

    # add_vec is a named vector: names are label text, values are codes
    add_codes <- unname(add_vec)
    add_texts <- names(add_vec)
    conflicted <- FALSE
    for (j in seq_along(add_codes)) {
      code_chr <- as.character(add_codes[[j]])
      text <- add_texts[[j]]
      if (!is.null(base_map[[code_chr]])) {
        if (!identical(base_map[[code_chr]], text)) {
          conflicted <- TRUE
          # Prefer base_map (first dataset); do nothing
        }
      } else {
        base_map[[code_chr]] <- text
      }
    }
    list(map = base_map, conflicted = conflicted)
  }

  # Walk inputs in order to accumulate baseline and detect conflicts
  if (length(df_list) > 0) {
    for (idx in seq_along(df_list)) {
      df <- df_list[[idx]]
      if (!is.data.frame(df)) next
      for (nm in names(df)) {
        x <- df[[nm]]
        x_attrs <- attributes(x)
        x_label <- if (!is.null(x_attrs)) x_attrs$label else NULL
        x_vlabels <- if (!is.null(x_attrs)) x_attrs$labels else NULL
        x_other <- if (!is.null(x_attrs)) x_attrs[setdiff(names(x_attrs), c("names", "class", "dim", "dimnames", "row.names", "tsp", "label", "labels"))] else NULL

        if (!exists(nm, envir = meta_map, inherits = FALSE)) {
          # First occurrence establishes baseline
          meta_map[[nm]] <- list(label = x_label, vlabel_map = as.list(setNames(if (length(x_vlabels)) as.list(names(x_vlabels)) else list(), character(0))), other = x_other)
          # Build initial value-label map (code -> text)
          if (!is.null(x_vlabels) && length(x_vlabels) > 0) {
            base_map <- new.env(parent = emptyenv())
            codes <- unname(x_vlabels)
            texts <- names(x_vlabels)
            for (j in seq_along(codes)) base_map[[as.character(codes[[j]])]] <- texts[[j]]
            meta_map[[nm]]$vlabel_map <- as.list(as.list(base_map))
          } else {
            meta_map[[nm]]$vlabel_map <- list()
          }
        } else {
          # Compare labels
          base <- meta_map[[nm]]
          if (!is.null(x_label) && !identical(base$label, x_label)) {
            label_conflicts <- unique(c(label_conflicts, nm))
          }
          # Merge value labels with precedence
          base_map <- as.list(base$vlabel_map)
          # Convert list-like map back to named vector structure for merge helper
          # (we will keep base_map as list code->text during merge)
          merged <- merge_value_labels(as.list(base_map), x_vlabels)
          if (isTRUE(merged$conflicted)) {
            vlabel_conflict_vars <- unique(c(vlabel_conflict_vars, nm))
          }
          base$vlabel_map <- merged$map
          meta_map[[nm]] <- base
        }
      }
    }
  }

  # Perform the actual bind
  out <- do.call(dplyr::bind_rows, arg_list_call)

  # Reapply metadata to result
  for (nm in names(out)) {
    if (!exists(nm, envir = meta_map, inherits = FALSE)) next
    info <- meta_map[[nm]]

    # Variable label
    if (!is.null(info$label)) {
      attr(out[[nm]], "label") <- info$label
    }

    # Value labels: convert code->text map back to named vector (text = names, code = values)
    if (length(info$vlabel_map) > 0) {
      # Determine code type based on output column storage
      to_type <- if (is.integer(out[[nm]])) "integer" else if (is.numeric(out[[nm]])) "double" else if (is.character(out[[nm]])) "character" else "character"
      codes_chr <- names(info$vlabel_map)
      texts <- unlist(unname(info$vlabel_map), use.names = FALSE)
      # Coerce codes
      codes <- switch(to_type,
        integer = suppressWarnings(as.integer(codes_chr)),
        double  = suppressWarnings(as.double(codes_chr)),
        character = as.character(codes_chr),
        as.character(codes_chr)
      )
      # Build named vector: names are texts, values are codes
      if (length(codes) == length(texts) && length(codes) > 0) {
        lab_vec <- stats::setNames(codes, texts)
        out[[nm]] <- sjlabelled::set_labels(out[[nm]], labels = lab_vec)
      }
    }

    # Other attributes from first occurrence
    if (!is.null(info$other) && length(info$other) > 0) {
      other_names <- names(info$other)
      for (an in other_names) {
        attr(out[[nm]], an) <- info$other[[an]]
      }
    }
  }

  # Single aggregated warning for conflicts
  if (length(label_conflicts) + length(vlabel_conflict_vars) > 0) {
    parts <- character(0)
    if (length(label_conflicts) > 0) {
      parts <- c(parts, paste0("variable labels differed for: ", paste(sort(unique(label_conflicts)), collapse = ", ")))
    }
    if (length(vlabel_conflict_vars) > 0) {
      parts <- c(parts, paste0("value label text differed for some codes in: ", paste(sort(unique(vlabel_conflict_vars)), collapse = ", ")))
    }
    warning(paste0("bind_rows metadata conflicts detected; using metadata from the first dataset (by column). Details: ", paste(parts, collapse = "; ")))
  }

  if (isTRUE(any_survey)) {
    return(structure(list(dat = out, dpdict = base_dpdict), class = "survey_data"))
  }

  out
}
