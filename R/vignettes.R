##### split_into_question_groups #####

dat <- get_big_test_dat(n=10)
dpdict <- create_dict(dat, prefill = TRUE)
dpdict <- dpdict[, !(names(dpdict) %in% c("old_variable_names", "old_variable_labels", "old_value_labels"))]

dpdict <- split_into_question_groups(dpdict, dat, variables_to_process = grepl("labelledmulti", dpdict$variable_names), variable_name_sep = "_", ignorelabelbeforeprefix = TRUE, prefix_sep = ": ",
                           splitbyclass = TRUE, splitbynumlabelledvalues = TRUE, splitbynoncontiguous = TRUE, splitbycommonlabel = TRUE,
                           labelsep = " - ", findlongest = TRUE, min_lcs_length = 10, min_common_strings = 3,
                           consistent_consecutive_mode = FALSE, consecutive_range = 10, variable_compare_mode = "complete",
                           noisy = 2)
