#' get_minimal_unlabelled_test_dat
#'
#' creates a very simple dataframe with columns for uid, csat, and nps
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
#'
#' creates a very simple dataframe with columns for uid, csat, and nps - and then adds variable and value labels
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
#'
#' creates a quite simple dataframe meant to replicate basic survey data for testing
#'
#' Includes columns for respid, numeric, oe, boolean, and 3 versions of each of ordinal, categorical and binary categorical - as labelled, factors, and characters
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
#'
#' creates a bigger but still well formatted dataframe meant to replicate all common question types in survey data
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

#' get_routing_test_dat
#'
#' creates a dataframe specifically for testing more complex routing patterns to test metadata handling
#'
#' @param n number of rows to create
#' @param random_seed for reproducability
#'
#' @return a dataframe
#' @export
#'
#' @examples get_routing_test_dat()
get_routing_test_dat <- function(n=100, random_seed = 123) {
  set.seed(random_seed)

  # Create base screening questions
  routing_test <- data.frame(
    # Base screening question
    screen1 = haven::labelled(
      sample(1:3, n, replace=TRUE),
      label="Screening: Which category best describes you?",
      labels = c("Student" = 1, "Employed" = 2, "Other" = 3)
    )
  )

  # Add age screening with different ranges per screen1 response
  routing_test$screen2 = haven::labelled(
    ifelse(routing_test$screen1 == 1, sample(16:25, n, replace=TRUE),
           ifelse(routing_test$screen1 == 2, sample(25:65, n, replace=TRUE),
                  NA_real_)),
    label="Screening: What is your age?"
  )

  # Questions only for students (screen1 == 1)
  routing_test$student_q1 = haven::labelled(
    ifelse(routing_test$screen1 == 1,
           sample(c(1:4, NA), n, replace=TRUE),
           NA_real_),
    label="Student questions: Year of study",
    labels = c("First" = 1, "Second" = 2, "Third" = 3, "Fourth+" = 4)
  )

  # Multiple response only for students in years 1-2
  routing_test$student_mr1_1 = haven::labelled(
    ifelse(routing_test$screen1 == 1 & routing_test$student_q1 %in% c(1,2),
           sample(c(0,1), n, replace=TRUE),
           NA_real_),
    label="Student services used - Library",
    labels = c("Not selected" = 0, "Selected" = 1)
  )
  routing_test$student_mr1_2 = haven::labelled(
    ifelse(routing_test$screen1 == 1 & routing_test$student_q1 %in% c(1,2),
           sample(c(0,1), n, replace=TRUE),
           NA_real_),
    label="Student services used - Career center",
    labels = c("Not selected" = 0, "Selected" = 1)
  )

  # Questions only for employed (screen1 == 2)
  routing_test$employed_q1 = haven::labelled(
    ifelse(routing_test$screen1 == 2,
           sample(c(1:5, NA), n, replace=TRUE),
           NA_real_),
    label="Employment: Years of experience",
    labels = c("<1"=1, "1-3"=2, "4-7"=3, "8-12"=4, "13+"=5)
  )

  # Grid question only for employed with 4+ years experience
  routing_test$employed_grid1_1_1 = haven::labelled(
    ifelse(routing_test$screen1 == 2 & routing_test$employed_q1 >= 3,
           sample(1:5, n, replace=TRUE),
           NA_real_),
    label="Work satisfaction - Pay - Current",
    labels = c("Very dissatisfied"=1, "Somewhat dissatisfied"=2,
               "Neither"=3, "Somewhat satisfied"=4, "Very satisfied"=5)
  )
  routing_test$employed_grid1_1_2 = haven::labelled(
    ifelse(routing_test$screen1 == 2 & routing_test$employed_q1 >= 3,
           sample(1:5, n, replace=TRUE),
           NA_real_),
    label="Work satisfaction - Pay - Previous",
    labels = c("Very dissatisfied"=1, "Somewhat dissatisfied"=2,
               "Neither"=3, "Somewhat satisfied"=4, "Very satisfied"=5)
  )

  # Nested routing based on multiple conditions
  routing_test$followup = haven::labelled(
    ifelse((routing_test$screen1 == 1 & routing_test$student_q1 >= 3) |
             (routing_test$screen1 == 2 & routing_test$employed_q1 >= 4),
           sample(c(1,2,NA), n, replace=TRUE),
           NA_real_),
    label="Follow-up interest",
    labels = c("Yes"=1, "No"=2)
  )

  return(routing_test)
}

#' get_multilingual_test_dat
#'
#' creates a dataframe with multilingual variables, labels, and values
#'
#' @param n number of rows to create
#' @param random_seed for reproducability
#'
#' @return a dataframe
#' @export
#'
#' @examples get_multilingual_test_dat()
get_multilingual_test_dat <- function(n=100, random_seed = 123) {
  set.seed(random_seed)

  multilingual_test <- data.frame(
    # Variable names in different languages
    pregunta_1 = haven::labelled(
      sample(1:5, n, replace=TRUE),
      label="¿Cómo calificaría el servicio? / How would you rate the service?",
      labels = c("Muy malo / Very poor" = 1,
                 "Malo / Poor" = 2,
                 "Regular / Fair" = 3,
                 "Bueno / Good" = 4,
                 "Muy bueno / Very good" = 5)
    ),

    # Cyrillic variable names and labels
    вопрос_1 = haven::labelled(
      sample(1:3, n, replace=TRUE),
      label="Как часто вы пользуетесь услугой? / How often do you use the service?",
      labels = c("Редко / Rarely" = 1,
                 "Иногда / Sometimes" = 2,
                 "Часто / Often" = 3)
    ),

    # Mixed script variable names
    q1_مرضی = haven::labelled(
      sample(1:5, n, replace=TRUE),
      label="کیا آپ مطمئن ہیں؟ / Are you satisfied?",
      labels = c("بالکل نہیں / Not at all" = 1,
                 "نہیں / No" = 2,
                 "شاید / Maybe" = 3,
                 "ہاں / Yes" = 4,
                 "بالکل / Absolutely" = 5)
    ),

    # Multiple response with mixed languages
    mr_選擇_1 = haven::labelled(
      sample(c(0,1), n, replace=TRUE),
      label="選擇所有適用項目 / Select all that apply - 選項 1 / Option 1",
      labels = c("未選擇 / Not selected" = 0,
                 "已選擇 / Selected" = 1)
    ),
    mr_選擇_2 = haven::labelled(
      sample(c(0,1), n, replace=TRUE),
      label="選擇所有適用項目 / Select all that apply - 選項 2 / Option 2",
      labels = c("未選擇 / Not selected" = 0,
                 "已選擇 / Selected" = 1)
    ),

    # RTL language grid
    grid_رضایت_1_1 = haven::labelled(
      sample(1:5, n, replace=TRUE),
      label="رضایت از خدمات - کیفیت - قبل / Service satisfaction - Quality - Before",
      labels = c("خیلی کم / Very low" = 1,
                 "کم / Low" = 2,
                 "متوسط / Medium" = 3,
                 "زیاد / High" = 4,
                 "خیلی زیاد / Very high" = 5)
    )
  )

  return(multilingual_test)
}

#' get_problematic_metadata_test_dat
#'
#' creates a dataframe with various problematic metadata patterns to test error handling
#'
#' @param n number of rows to create
#' @param random_seed for reproducability
#'
#' @return a dataframe
#' @export
#'
#' @examples get_problematic_metadata_test_dat()
get_problematic_metadata_test_dat <- function(n=100, random_seed = 123) {
  set.seed(random_seed)

  problematic_test <- data.frame(
    # Duplicate variable labels within question group
    q1_1 = haven::labelled(
      sample(1:5, n, replace=TRUE),
      label="Q1: Same label for multiple questions"
    ),
    q1_2 = haven::labelled(
      sample(1:5, n, replace=TRUE),
      label="Q1: Same label for multiple questions"
    ),

    # Inconsistent value labels within same scale
    scale_1 = haven::labelled(
      sample(1:5, n, replace=TRUE),
      label="Scale question 1",
      labels = c("Very poor" = 1, "Poor" = 2, "Fair" = 3, "Good" = 4, "Excellent" = 5)
    ),
    scale_2 = haven::labelled(
      sample(1:5, n, replace=TRUE),
      label="Scale question 2",
      labels = c("Very bad" = 1, "Bad" = 2, "Okay" = 3, "Good" = 4, "Very good" = 5)
    ),

    # Missing labels
    no_label = sample(1:5, n, replace=TRUE),

    # Empty labels
    empty_label = haven::labelled(
      sample(1:5, n, replace=TRUE),
      label=""
    ),

    # Inconsistent variable name patterns
    q3_a = haven::labelled(
      sample(1:5, n, replace=TRUE),
      label="Q3: First question"
    ),
    q3b = haven::labelled(
      sample(1:5, n, replace=TRUE),
      label="Q3: Second question"
    ),

    # Non-sequential question numbering
    q5_1 = haven::labelled(
      sample(1:5, n, replace=TRUE),
      label="Q5: First statement"
    ),
    q5_3 = haven::labelled(
      sample(1:5, n, replace=TRUE),
      label="Q5: Third statement"
    ),

    # Mixed data types in same question group
    mixed_1 = haven::labelled(
      sample(1:5, n, replace=TRUE),
      label="Mixed types question - Numeric"
    ),
    mixed_2 = haven::labelled(
      sample(c("A", "B", "C"), n, replace=TRUE),
      label="Mixed types question - Character"
    ),

    # Invalid characters in labels
    special_chars = haven::labelled(
      sample(1:5, n, replace=TRUE),
      label="Q7: Contains special chars: @#$%^&*"
    ),

    # Inconsistent level of detail in labels
    brief_1 = haven::labelled(
      sample(1:5, n, replace=TRUE),
      label="Brief"
    ),
    brief_2 = haven::labelled(
      sample(1:5, n, replace=TRUE),
      label="Brief - With extensive additional explanatory text that makes this label much longer than others in the group"
    )
  )

  return(problematic_test)
}

#' get_nested_metadata_test_dat
#'
#' creates a dataframe with complex nested question structures
#'
#' @param n number of rows to create
#' @param random_seed for reproducability
#'
#' @return a dataframe
#' @export
#'
#' @examples get_nested_metadata_test_dat()
get_nested_metadata_test_dat <- function(n=100, random_seed = 123) {
  set.seed(random_seed)

  # Create base data with product evaluation
  nested_test <- data.frame(
    # Loop over products
    product_count = haven::labelled(
      sample(1:3, n, replace=TRUE),
      label="How many products did you purchase?",
      labels = c("One" = 1, "Two" = 2, "Three" = 3)
    )
  )

  # Add looped questions for each potential product
  for(i in 1:3) {
    # Only fill in data for respondents who bought this many products
    product_filter <- nested_test$product_count >= i

    # Basic product details
    nested_test[[paste0("prod", i, "_type")]] <- haven::labelled(
      ifelse(product_filter,
             sample(1:3, n, replace=TRUE),
             NA_real_),
      label=paste0("Product ", i, " - Type"),
      labels = c("Electronics" = 1, "Clothing" = 2, "Food" = 3)
    )

    # Nested satisfaction grid for each product
    for(aspect in c("quality", "price", "packaging")) {
      for(timepoint in c("purchase", "now")) {
        var_name <- paste0("prod", i, "_sat_", aspect, "_", timepoint)
        nested_test[[var_name]] <- haven::labelled(
          ifelse(product_filter,
                 sample(1:5, n, replace=TRUE),
                 NA_real_),
          label=paste0("Product ", i, " - Satisfaction - ", tools::toTitleCase(aspect), " - ", tools::toTitleCase(timepoint)),
          labels = c("Very dissatisfied" = 1, "Somewhat dissatisfied" = 2,
                     "Neither" = 3, "Somewhat satisfied" = 4, "Very satisfied" = 5)
        )
      }
    }

    # Nested multiple response for each product
    for(j in 1:3) {
      var_name <- paste0("prod", i, "_features_", j)
      nested_test[[var_name]] <- haven::labelled(
        ifelse(product_filter,
               sample(c(0,1), n, replace=TRUE),
               NA_real_),
        label=paste0("Product ", i, " - Features - Option ", j),
        labels = c("Not selected" = 0, "Selected" = 1)
      )
    }
  }

  # Add complex hierarchical question (e.g., brand awareness -> consideration -> usage)
  brands <- c("A", "B", "C")

  # First level: Awareness
  for(brand in brands) {
    var_name <- paste0("brand_aware_", brand)
    nested_test[[var_name]] <- haven::labelled(
      sample(c(0,1), n, replace=TRUE),
      label=paste0("Brand awareness - ", brand),
      labels = c("Not aware" = 0, "Aware" = 1)
    )

    # Second level: Consideration (only if aware)
    var_name_cons <- paste0("brand_consider_", brand)
    nested_test[[var_name_cons]] <- haven::labelled(
      ifelse(nested_test[[paste0("brand_aware_", brand)]] == 1,
             sample(c(0,1), n, replace=TRUE),
             NA_real_),
      label=paste0("Brand consideration - ", brand),
      labels = c("Not considered" = 0, "Considered" = 1)
    )

    # Third level: Usage (only if considered)
    var_name_use <- paste0("brand_usage_", brand)
    nested_test[[var_name_use]] <- haven::labelled(
      ifelse(nested_test[[paste0("brand_consider_", brand)]] == 1,
             sample(1:4, n, replace=TRUE),
             NA_real_),
      label=paste0("Brand usage - ", brand),
      labels = c("Never" = 1, "Rarely" = 2, "Sometimes" = 3, "Frequently" = 4)
    )
  }

  return(nested_test)
}
