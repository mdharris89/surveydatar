#' get_minimal_unlabelled_test_dat
#' creates a simple dataframe with columns for uid, csat, and nps
#'
#' @param n number of rows to create
#' @param random_seed for reproducability
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' get_minimal_unlabelled_test_dat()
get_minimal_unlabelled_test_dat <- function(n=100, random_seed = 123) {

  set.seed(random_seed)
  unlabelled_test_dat <- data.frame(uid = 1:n,
                                    csat = sample(c(1, 2, 3, 4, 5, NA), n, replace = TRUE),
                                    nps_response = sample(c(0:10, NA), n, replace = TRUE))

  return(unlabelled_test_dat)
}

#' get_minimal_labelled_test_dat
#' creates a simple dataframe with columns for uid, csat, and nps - and then adds variable and value labels
#'
#' @param n number of rows to create
#' @param random_seed for reproducability
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' get_minimal_labelled_test_dat()
get_minimal_labelled_test_dat <- function(n = 100, random_seed = 123) {

  set.seed(random_seed)

  labelled_test_dat <- get_minimal_unlabelled_test_dat()

  labelled_test_dat$uid <- sjlabelled::set_label(labelled_test_dat$uid, label = "uid")
  labelled_test_dat$csat <- sjlabelled::set_label(labelled_test_dat$csat, label = "Satisfaction")
  labelled_test_dat$csat <- sjlabelled::set_labels(labelled_test_dat$csat, labels = c("Very dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very satisfied"))

  labelled_test_dat$nps_response <- sjlabelled::set_label(labelled_test_dat$nps_response, label = "NPS response")

  return(labelled_test_dat)
}

#' get_basic_test_dat
#' creates a basic dataframe meant to replicate survey data for testing
#' with columns for respid, numeric, oe, boolean, and 3 versions of each of ordinal, categorical and binary categorical - as labelled, factors, and characters
#'
#' @param n number of rows to create
#' @param random_seed for reproducability
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' get_basic_test_dat()
get_basic_test_dat <- function(n = 100, random_seed = 123){

  set.seed(random_seed)

  basic_test <- data.frame(
                  respid = sjlabelled::set_label(as.character(1:n), label = "Example respid variable"),
                  randomnumeric = sjlabelled::set_label(sample(c(stats::rnorm(n, mean = 10, sd = 2), NA), size = n, replace = TRUE, prob = c(rep((1-.25)/n, n), .25)), label = "Example random non-integer numeric variable"),
                  randomoe = sjlabelled::set_label(sample(c(replicate(n, paste(sample(c(letters, " "), 5, replace = TRUE), collapse = "")), NA), size = n, replace = TRUE, prob = c(rep((1-.25)/n, n), .25)), label = "Example oe - a single 'word' of random characters"),
                  booleans = sjlabelled::set_label(sample(c(TRUE, FALSE, NA), size = n, replace = TRUE), label = "Example boolean variable"),
                  labelledordinal = haven::labelled(sample(c(1, 2, 3, 4, NA), size = n, replace = TRUE), label = "Example satisfaction question (labelled numeric)", labels = c("Very dissatisfied" = 1, "Somewhat dissatisfied" = 2, "Somewhat satisfied" = 3, "Very satisfied" = 4)),
                  labelledcategorical = haven::labelled(sample(c(1, 2, 3, 4, NA), size = n, replace = TRUE), label = "Example region question (labelled numeric)", labels = c("North" = 1, "East" = 2, "South" = 3, "West" = 4)),
                  labelledbinarycategorical = haven::labelled(sample(c(0, 1, NA), size = n, replace = TRUE), label = "Example categorical question (labelled numeric)", labels = c("No" = 0, "Yes" = 1)),
                  ordinalasfactor = sjlabelled::set_label(as.ordered(sample(c("18-24", "25-34", "35-54", "54-70", NA), size = n, replace = TRUE)), label = "Example age groups question (ordinal)"),
                  categoricalasfactor = sjlabelled::set_label(sjlabelled::as_factor(sample(c("UK", "FR", "DE", NA), size = n, replace = TRUE)), label = "Example country question (factor)"),
                  binarycategoricalasfactor = sjlabelled::set_label(sjlabelled::as_factor(sample(c("No", "Yes", NA), size = n, replace = TRUE)), label = "Example categorical yes/no question (factor)"),
                  ordinalascharacter = sjlabelled::set_label(sample(c("Secondary school", "Undergraduate university", "Masters", "PhD", NA), size = n, replace = TRUE), label = "Example education question (should be ordinal but as string)"),
                  categoricalascharacter = sjlabelled::set_label(sample(c("Red", "Green", "Blue", NA), size = n, replace = TRUE), label = "Example categorical question (as string)"),
                  binarycategoricalascharacter = sjlabelled::set_label(sample(c("No", "Yes", NA), size = n, replace = TRUE), label = "Example categorical yes/no question (as string)"))

  return(basic_test)
}

#' get_big_test_dat
#' creates a dataframe meant to replicate all common question types in survey data
#'
#' @param n number of rows to create
#' @param random_seed for reproducability
#'
#' @return a dataframe
#' @export
#'
#' @examples get_big_test_dat()
get_big_test_dat <- function(n=100, random_seed = 123){

  set.seed(random_seed)

  big_test <- data.frame(respid = sjlabelled::set_label(as.character(1:n), label = "Example respid variable"),
                     randomnumeric = sjlabelled::set_label(sample(c(rnorm(n, mean = 10, sd = 2), NA), size = n, replace = TRUE, prob = c(rep((1-.25)/n, n), .25)), label = "Example random non-integer numeric variable"),
                     randomoe = sjlabelled::set_label(sample(c(replicate(n, paste(sample(c(letters, " "), 5, replace = TRUE), collapse = "")), NA), size = n, replace = TRUE, prob = c(rep((1-.25)/n, n), .25)), label = "Example oe - a single 'word' of random characters"),
                     booleans = sjlabelled::set_label(sample(c(TRUE, FALSE, NA), size = n, replace = TRUE), label = "Example boolean variable"),
                     labelledordinal = haven::labelled(sample(c(1, 2, 3, 4, NA), size = n, replace = TRUE), label = "Example satisfaction question (labelled numeric)", labels = c("Very dissatisfied" = 1, "Somewhat dissatisfied" = 2, "Somewhat satisfied" = 3, "Very satisfied" = 4)),
                     labelledcategorical = haven::labelled(sample(c(1, 2, 3, 4, NA), size = n, replace = TRUE), label = "Example region question (labelled numeric)", labels = c("North" = 1, "East" = 2, "South" = 3, "West" = 4)),
                     labelledbinarycategorical = haven::labelled(sample(c(0, 1, NA), size = n, replace = TRUE), label = "Example categorical question (labelled numeric)", labels = c("No" = 0, "Yes" = 1)),
                     labelledmultiordinal_1 = haven::labelled(sample(c(1, 2, 3, 4, 5, NA), size = n, replace = TRUE), label = "Example likert question (labelled numeric) - statement 1", labels = c("Strongly disagree" = 1, "Somewhat disagree" = 2, "Neither agree nor disagree" = 3, "Somewhat agree" = 4, "Strongly agree" = 5)),
                     labelledmultiordinal_2 = haven::labelled(sample(c(1, 2, 3, 4, 5, NA), size = n, replace = TRUE), label = "Example likert question (labelled numeric) - statement 2", labels = c("Strongly disagree" = 1, "Somewhat disagree" = 2, "Neither agree nor disagree" = 3, "Somewhat agree" = 4, "Strongly agree" = 5)),
                     labelledmultiresponse_1 = haven::labelled(sample(c(0, 1, NA), size = n, replace = TRUE), label = "Example multiple response question (labelled numeric) - statement 1", labels = c("Not selected" = 0, "Selected" = 1)),
                     labelledmultiresponse_2 = haven::labelled(sample(c(0, 1, NA), size = n, replace = TRUE), label = "Example multiple response question (labelled numeric) - statement 2", labels = c("Not selected" = 0, "Selected" = 1)),
                     labelledmultiresponsegrid_1_1 = haven::labelled(sample(c(0, 1, NA), size = n, replace = TRUE), label = "Example multiple response grid question (labelled numeric) - statement 1 - attribute 1", labels = c("Not selected" = 0, "Selected" = 1)),
                     labelledmultiresponsegrid_1_2 = haven::labelled(sample(c(0, 1, NA), size = n, replace = TRUE), label = "Example multiple response grid question (labelled numeric) - statement 1 - attribute 2", labels = c("Not selected" = 0, "Selected" = 1)),
                     labelledmultiresponsegrid_2_1 = haven::labelled(sample(c(0, 1, NA), size = n, replace = TRUE), label = "Example multiple response grid question (labelled numeric) - statement 2 - attribute 1", labels = c("Not selected" = 0, "Selected" = 1)),
                     labelledmultiresponsegrid_2_2 = haven::labelled(sample(c(0, 1, NA), size = n, replace = TRUE), label = "Example multiple response grid question (labelled numeric) - statement 2 - attribute 2", labels = c("Not selected" = 0, "Selected" = 1)),
                     ordinalasfactor = sjlabelled::set_label(as.ordered(sample(c("18-24", "25-34", "35-54", "54-70", NA), size = n, replace = TRUE)), label = "Example age groups question (ordinal)"),
                     categoricalasfactor = sjlabelled::set_label(sjlabelled::as_factor(sample(c("UK", "FR", "DE", NA), size = n, replace = TRUE)), label = "Example country question (factor)"),
                     multiresponseasfactor_1 = sjlabelled::set_label(sjlabelled::as_factor(sample(c("Not selected", "Selected", NA), size = n, replace = TRUE)), label = "Example multiple response question (factor) - statement 1"),
                     multiresponseasfactor_2 = sjlabelled::set_label(sjlabelled::as_factor(sample(c("Not selected", "Selected", NA), size = n, replace = TRUE)), label = "Example multiple response question (factor) - statement 2"),
                     binarycategoricalasfactor = sjlabelled::set_label(sjlabelled::as_factor(sample(c("No", "Yes", NA), size = n, replace = TRUE)), label = "Example categorical yes/no question (factor)"),
                     ordinalascharacter = sjlabelled::set_label(sample(c("Secondary school", "Undergraduate university", "Masters", "PhD", NA), size = n, replace = TRUE), label = "Example education question (should be ordinal but as string)"),
                     categoricalascharacter = sjlabelled::set_label(sample(c("Red", "Green", "Blue", NA), size = n, replace = TRUE), label = "Example categorical question (as string)"),
                     binarycategoricalascharacter = sjlabelled::set_label(sample(c("No", "Yes", NA), size = n, replace = TRUE), label = "Example categorical yes/no question (as string)"),
                     multiresponseascharacter_1 = sjlabelled::set_label(sample(c("Not selected", "Selected", NA), size = n, replace = TRUE), label = "Example multiple response question (as string) - statement 1"),
                     multiresponseascharacter_2 = sjlabelled::set_label(sample(c("Not selected", "Selected", NA), size = n, replace = TRUE), label = "Example multiple response question (as string) - statement 2"))

  # variables based on other variables
  big_test$labelledmultiresponsebasedlabelledcategorical_1 <- haven::labelled(vapply(big_test$labelledcategorical, function(x) ifelse(x == 1, sample(c(0, 1), size = 1), NA_real_), double(1)), label = "Example multi response question based on labelledcategorical == 1 else NA (e.g. routed brand funnel question)", labels = c("Not selected" = 0, "Selected" = 1))
  big_test$labelledmultiresponsebasedlabelledcategorical_2 <- haven::labelled(vapply(big_test$labelledcategorical, function(x) ifelse(x == 2, sample(c(0, 1), size = 1), NA_real_), double(1)), label = "Example multi response question based on labelledcategorical == 2 else NA (e.g. routed brand funnel question)", labels = c("Not selected" = 0, "Selected" = 1))
  big_test$labelledmultiresponsebasedlabelledcategorical_3 <- haven::labelled(vapply(big_test$labelledcategorical, function(x) ifelse(x == 3, sample(c(0, 1), size = 1), NA_real_), double(1)), label = "Example multi response question based on labelledcategorical == 3 else NA (e.g. routed brand funnel question)", labels = c("Not selected" = 0, "Selected" = 1))
  big_test$labelledmultiresponsebasedlabelledcategorical_4 <- haven::labelled(vapply(big_test$labelledcategorical, function(x) ifelse(x == 4, sample(c(0, 1), size = 1), NA_real_), double(1)), label = "Example multi response question based on labelledcategorical == 4 else NA (e.g. routed brand funnel question)", labels = c("Not selected" = 0, "Selected" = 1))

  return(big_test)
}


