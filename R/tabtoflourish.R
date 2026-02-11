# Suppress R CMD check notes for tidyr NSE
utils::globalVariables(c("row_label", "value"))

#' Convert a surveydatar tab_result into a Flourish-ready object
#'
#' @param tab_result    A tab_result produced by \code{\link{tab}}
#' @param chart_type    Optional character; Flourish chart type. If not specified, automatically 
#'   selected based on data structure and statistic. Supported types:
#'   \itemize{
#'     \item \code{"table"} - Data table
#'     \item \code{"bar_grouped"} - Grouped bar chart
#'     \item \code{"bar_stacked"} - Stacked bar chart
#'     \item \code{"bar_stacked_prop"} - Proportional stacked bar chart
#'     \item \code{"column_grouped"} - Grouped column chart
#'     \item \code{"column_stacked"} - Stacked column chart
#'     \item \code{"column_stacked_prop"} - Proportional stacked column chart
#'     \item \code{"line"} - Line chart
#'     \item \code{"area"} - Area chart
#'     \item \code{"area_stacked"} - Stacked area chart
#'     \item \code{"donut"} - Donut/pie chart
#'   }
#' @param long          Should the data be pivoted to long format? (default TRUE)
#' @param strip_summary_rows Remove NET / Avg rows? (default TRUE for non-table charts)
#' @param strip_summary_cols Remove Total / NET columns? (default TRUE for non-table charts)
#' @param table_settings Named list of table-specific settings. See details for options.
#' @param settings Named list corresponding to flourish settings with dot notation e.g. "number_format.suffix" = "%"
#' @param label_mode Label display mode for exported labels (default "full")
#' @param ...           Future arguments
#'
#' @return An object of class "flourish_tab" containing:
#'   * data      – a data.frame to feed into flourishcharts::bind_*()
#'   * bindings  – named list mapping column names to Flourish roles
#'   * chart_type  – character chart_type hint (may be NULL)
#'
#' @details
#' This function extracts metadata from the tab_result (statistic info, summary labels, etc.)
#' at the start, then transforms the data values for Flourish visualization. The transform
#' functions work with plain data frames and don't preserve tab_result-specific attributes
#' (arrays, base_matrix, etc.) since these are not needed for visualization. Base counts are
#' always removed as Flourish cannot handle them elegantly in visualizations (base can appear
#' as either a row or column depending on the statistic). If chart_type is not specified, the
#' function automatically selects an appropriate chart type based on the statistic type and
#' data dimensions. For example, count tables with many columns (>4) will use a table chart,
#' while simpler structures use bar or column charts. Percentage statistics may use stacked
#' or grouped charts depending on the number of rows and columns.
#' 
#' For stacked and grouped charts, the transformation depends on the statistic:
#' \itemize{
#'   \item For \code{column_pct}: Data is transposed so columns become x-axis and rows become stacks/groups
#'   \item For \code{row_pct}: Data is not transposed; rows become bars and columns become stacks/groups
#' }
#' This ensures the visualization always matches user intuition: the dimension that sums to 100%
#' is shown as the primary visual element (column or bar).
#'
#' ## Table Settings
#'
#' The `table_settings` parameter accepts a named list with the following options:
#'
#' **Color Coding:**
#' * `color_mode` - Character: "none" (default) or "heatmap"
#' * `color_midpoint` - Numeric value, "mean", or "median". Midpoint for diverging color scale (default "mean")
#' * `color_palette` - Character vector of 3 colors (low, mid, high). Defaults to soft blue-white-red
#'
#' Heatmap coloring applies a gradient to each numeric column independently, 
#' with each column scaled from its minimum to maximum value.
#'
#' **Table Display:**
#' * `rows_per_page` - Integer: Number of rows per page (default 20)
#' * `show_sorting` - Logical: Enable column sorting (default TRUE)
#' * `show_search` - Logical: Enable search/filter box (default TRUE)
#' * `first_column_width` - Numeric: Width multiplier for first column (default 2)
#'
#' @export
#'
#' @examples
#' survey_obj <- create_survey_data(get_basic_test_dat(n = 50))
#' x <- tab(survey_obj, labelledcategorical, labelledordinal)
#' f_tab <- tab_to_flourish(x)
#' f_tab
#' # flourishcharts::flourish(f_tab$data, f_tab$chart_type, bindings = f_tab$bindings)
tab_to_flourish <- function(tab_result,
                            chart_type    = NULL,
                            long          = TRUE,
                            strip_summary_rows = TRUE,
                            strip_summary_cols = TRUE,
                            table_settings = NULL,
                            settings      = NULL,
                            label_mode = "full",
                            ...) {

  stopifnot(inherits(tab_result, "tab_result"))

  # Extract metadata first (before any modifications)
  stat_info <- .extract_stat_info_safe(tab_result)
  
  # Check for mixed-statistic tabs (from gluing different statistics)
  if (isTRUE(stat_info$is_mixed)) {
    message("Note: This tab contains mixed statistics (from gluing tabs with different statistics). ",
            "Flourish chart will use the first cell's statistic (", stat_info$id, ") for display settings. ",
            "Mixed-statistic tabs may not display optimally in Flourish charts.")
  }
  
  # Apply native operations if cell-based
  if (inherits(tab_result, "tab_cell_collection")) {
    # Flourish can't handle base - ensure it's removed
    # (User may have already hidden it in pipeline, but this ensures it)
    if (stat_info$base_label %in% c(tab_result$layout$row_labels, 
                                     tab_result$layout$col_labels)) {
      tab_result <- hide_base(tab_result)
    }
    
    # Strip summaries if requested
    if (strip_summary_rows || strip_summary_cols) {
      tab_result <- hide_summary(tab_result, 
                                 rows = strip_summary_rows,
                                 cols = strip_summary_cols)
    }
    
    # Materialize with class preservation
    tab_result <- .materialize_for_export(tab_result, show_base = FALSE, label_mode = label_mode)
    
  } else {
    # Data.frame-based path (existing implementation)
    # Apply layout ordering before export
    layout <- attr(tab_result, "layout")
    if (!is.null(layout) && !is.null(layout$row_order)) {
      tab_result <- tab_result[layout$row_order, , drop = FALSE]
    }
    if (!is.null(layout) && !is.null(layout$col_order)) {
      col_order_with_label <- c(1, layout$col_order + 1)
      tab_result <- tab_result[, col_order_with_label, drop = FALSE]
    }
    
    # Apply visibility filtering before export
    visibility <- attr(tab_result, "visibility")
    if (!is.null(visibility)) {
      if (!is.null(visibility$rows)) {
        tab_result <- tab_result[visibility$rows, , drop = FALSE]
      }
      if (!is.null(visibility$cols)) {
        visible_cols <- c(TRUE, visibility$cols)
        tab_result <- tab_result[, visible_cols, drop = FALSE]
      }
    }
  }

  # Determine chart type early (needed for strip_summary defaults)
  chart_type <- chart_type %||% guess_flourish_chart_type(tab_result)
  
  # Adjust strip_summary defaults for table charts
  is_table <- (chart_type == "table")
  if (is_table) {
    # For tables, default to keeping summary rows/cols (user can override)
    strip_summary_rows <- strip_summary_rows %||% FALSE
    strip_summary_cols <- strip_summary_cols %||% FALSE
  }

  # Clean data (skip for cell-based tabs - already handled by hide_summary/hide_base)
  if (inherits(tab_result, "tab_cell_collection")) {
    # Cell-based: base and summaries already correctly filtered before materialization
    # Convert to data.frame before any tidyr operations
    df <- as.data.frame(tab_result)
  } else {
    # Data.frame-based: only strip base (summaries handled during tab() creation)
    df <- .clean_tab_data(tab_result, stat_info,
                          strip_summary_rows = FALSE,
                          strip_summary_cols = FALSE)
  }

  # Transform data for chart type
  transform_result <- .transform_for_chart_type(df, chart_type, stat_info, long = long)

  # Merge default and user settings
  default_settings <- .get_default_settings(stat_info$id, chart_type)
  
  # Add table-specific settings (always apply defaults for tables, even if table_settings is NULL)
  # Use transformed data so column names match
  if (is_table) {
    # Validate and normalize table settings
    if (!is.null(table_settings)) {
      table_settings <- .validate_table_settings(table_settings)
    }
    table_flourish_settings <- .get_table_flourish_settings(table_settings, transform_result$data, stat_info)
    default_settings <- utils::modifyList(default_settings, table_flourish_settings)
  }
  
  final_settings <- if (!is.null(settings)) {
    utils::modifyList(default_settings, settings)
  } else {
    default_settings
  }

  # Return flourish_tab object
  res <- list(
    data     = transform_result$data,
    bindings = transform_result$bindings,
    chart_type = chart_type,
    settings = final_settings,
    measures = attr(tab_result, "measures"),
    measure_matrix = attr(tab_result, "measure_matrix")
  )
  class(res) <- "flourish_tab"
  res
}

#' Extract statistic information from tab result
#' @keywords internal
.extract_stat_info <- function(tab_result) {
  # Use the safe version from export_helpers
  .extract_stat_info_safe(tab_result)
}

#' Clean tab data by removing metadata rows/columns
#' @keywords internal
.clean_tab_data <- function(tab_result, stat_info, strip_summary_rows, strip_summary_cols) {
  df <- tab_result

  # Always strip base for Flourish (base counts cannot be visualized elegantly)
  # Base can appear as either a row or a column depending on statistic
  if (stat_info$base_label %in% df$row_label) {
    df <- df[df$row_label != stat_info$base_label, , drop = FALSE]
  }
  if (stat_info$base_label %in% names(df)) {
    df[[stat_info$base_label]] <- NULL
  }

  if (strip_summary_rows && !is.null(stat_info$summary_row) && stat_info$summary_row %in% df$row_label) {
    df <- df[df$row_label != stat_info$summary_row, , drop = FALSE]
  }

  if (strip_summary_cols && !is.null(stat_info$summary_col) && stat_info$summary_col %in% names(df)) {
    df[[stat_info$summary_col]] <- NULL
  }

  df
}

#' Transform data based on chart type requirements
#' @keywords internal
.transform_for_chart_type <- function(df, chart_type, stat_info, long = TRUE) {
  chart_type <- tolower(chart_type)

  # Get data dimensions

  # NEEDS FIXING
  n_rows <- nrow(df)
  n_cols <- ncol(df) - 1
  col_names <- names(df)[-1]

  # Charts that work with wide format
  if (chart_type == "table") {
    return(.transform_wide_format(df, chart_type))
  }

  # Charts that need special transformations
  transform_fn <- switch(
    chart_type,
    "bar_stacked_prop" = ,
    "bar_stacked" = ,
    "column_stacked_prop" = ,
    "column_stacked" = .transform_stacked_bar,

    "bar_grouped" = ,
    "column_grouped" = .transform_grouped_bar,

    "line" = ,
    "area" = ,
    "area_stacked" = .transform_line_area,

    "donut" = .transform_pie_donut,

    # Default transformation
    function(df, ...) .transform_default(df, long = long)
  )

  transform_fn(df, n_rows, n_cols, col_names, stat_info)
}

#' Transform for stacked bar charts
#' @keywords internal
.transform_stacked_bar <- function(df, n_rows, n_cols, col_names, stat_info) {
  # For column_pct: transpose so columns become x-axis (each column sums to 100%)
  # For row_pct: keep as-is so rows become bars (each row sums to 100%)
  
  if (stat_info$id == "row_pct") {
    # Don't transpose - rows already represent what should be bars
    list(
      data = df,
      bindings = list(
        label = "row_label",   # Y-axis (bars)
        value = col_names      # X-axis values (stacks)
      )
    )
  } else {
    # Transpose for column_pct and other stats
    # First pivot to long format
    long_df <- tidyr::pivot_longer(
      df,
      cols = -row_label,
      names_to = "label",
      values_to = "value"
    )
    
    # Then pivot wide with original rows as columns (stacks)
    wide_df <- tidyr::pivot_wider(
      long_df,
      names_from = row_label,
      values_from = value
    )
    
    # Get the stack column names (original row labels)
    stack_cols <- setdiff(names(wide_df), "label")
    
    list(
      data = wide_df,
      bindings = list(
        label = "label",       # X-axis (original columns)
        value = stack_cols     # Y-axis stacks (original rows)
      )
    )
  }
}

#' Transform for grouped bar charts
#' @keywords internal
.transform_grouped_bar <- function(df, n_rows, n_cols, col_names, stat_info) {
  # For column_pct: transpose so columns become x-axis
  # For row_pct: keep as-is so rows become bars
  
  if (stat_info$id == "row_pct") {
    # Don't transpose - rows already represent what should be bars
    list(
      data = df,
      bindings = list(
        label = "row_label",   # Y-axis (bars)
        value = col_names      # X-axis values (groups)
      )
    )
  } else {

    # Transpose for column_pct and other stats
    # First pivot to long format
    long_df <- tidyr::pivot_longer(
      df,
      cols = -row_label,
      names_to = "label",
      values_to = "value"
    )
    
    # Then pivot wide with original rows as columns (groups)
    wide_df <- tidyr::pivot_wider(
      long_df,
      names_from = row_label,
      values_from = value
    )
    
    # Get the group column names (original row labels)
    group_cols <- setdiff(names(wide_df), "label")
    
    list(
      data = wide_df,
      bindings = list(
        label = "label",       # X-axis (original columns)
        value = group_cols     # Y-axis groups (original rows)
      )
    )
  }
}

#' Transform for line/area charts
#' @keywords internal
.transform_line_area <- function(df, n_rows, n_cols, col_names, stat_info) {
  # Flourish line charts want wide format with:
  # - Time periods as rows (x-axis)
  # - Series as columns (different lines)
  # So we need to transpose: tab gives us series as rows, time as columns
  
  # First pivot to long format
  long_df <- tidyr::pivot_longer(
    df,
    cols = -row_label,
    names_to = "label",
    values_to = "value"
  )
  
  # Then pivot wide with series as columns
  wide_df <- tidyr::pivot_wider(
    long_df,
    names_from = row_label,
    values_from = value
  )
  
  # Get the series column names (everything except 'label')
  series_cols <- setdiff(names(wide_df), "label")
  
  list(
    data = wide_df,
    bindings = list(
      label = "label",       # X-axis (time periods)
      value = series_cols    # Y-axis (multiple series columns)
    )
  )
}

#' Transform for donut charts
#' @keywords internal
.transform_pie_donut <- function(df, n_rows, n_cols, col_names, stat_info) {
  if (n_cols == 1) {
    # Single column - each row is a slice
    list(
      data = df,
      bindings = list(
        label = "row_label",
        value = col_names[1]
      )
    )
  } else if (n_rows == 1) {
    # Single row - pivot to make columns as slices
    pie_df <- data.frame(
      label = col_names,
      value = as.numeric(df[1, -1])
    )
    list(
      data = pie_df,
      bindings = list(label = "label", value = "value")
    )
  } else {
    # Multiple rows and columns - use facet for multiple pies
    long_df <- tidyr::pivot_longer(
      df,
      cols = -row_label,
      names_to = "category",
      values_to = "value"
    )
    list(
      data = long_df,
      bindings = list(
        label = "category",
        value = "value",
        facet = "row_label"  # Create a pie for each row
      )
    )
  }
}

#' Transform for wide format charts
#' @keywords internal
.transform_wide_format <- function(df, chart_type) {
  # For tables, replace colons in column names (Flourish uses colons as delimiters in color rules)
  if (chart_type == "table") {
    names(df) <- gsub(": ", " - ", names(df), fixed = TRUE)
  }
  
  bindings <- switch(
    chart_type,
    "table" = list(rows_data = TRUE),
    list(label = "row_label", value = names(df)[-1])
  )
  list(data = df, bindings = bindings)
}

#' Default transformation
#' @keywords internal
.transform_default <- function(df, long = TRUE) {
  if (long) {
    long_df <- tidyr::pivot_longer(
      df,
      cols = -row_label,
      names_to = "series",
      values_to = "value"
    )
    list(
      data = long_df,
      bindings = list(label = "row_label", value = "value", facet = "series")
    )
  } else {
    list(
      data = df,
      bindings = list(label = "row_label", value = names(df)[-1])
    )
  }
}

#' Strip tab_result metadata, leaving a plain data.frame
#' @keywords internal
.as_plain_data_frame <- function(df) {
  if (!is.data.frame(df)) {
    return(df)
  }

  plain_df <- df

  attr_names_to_keep <- c("names", "row.names", "class")
  attrs <- attributes(plain_df)
  attrs <- attrs[intersect(names(attrs), attr_names_to_keep)]

  attrs$class <- "data.frame"

  attributes(plain_df) <- attrs
  plain_df
}

#' Replace NA values with empty strings for Flourish compatibility
#' @keywords internal
.replace_na_with_empty_strings <- function(df) {
  if (!is.data.frame(df)) {
    return(df)
  }

  for (col_name in names(df)) {
    column <- df[[col_name]]

    if (!anyNA(column)) {
      next
    }

    if (is.list(column)) {
      df[[col_name]] <- lapply(column, function(x) {
        if (length(x) == 1 && is.na(x)) "" else x
      })
      next
    }

    column_char <- as.character(column)
    column_char[is.na(column)] <- ""
    df[[col_name]] <- column_char
  }

  df
}

#' Generate default Flourish settings based on statistic type
#' @keywords internal
.get_default_settings <- function(stat_id, chart_type) {
  defaults <- list()

  # Value formatting based on statistic
  if (stat_id %in% c("column_pct", "row_pct") && chart_type %in% c("column_grouped", "column_stacked", "column_stacked_prop", "bar_grouped", "bar_stacked", "bar_stacked_prop")) {
    defaults$number_format$n_dec <- 1
    defaults$number_format$suffix <- "%"
    defaults$y$linear_max <- 100
    defaults$y$linear_min <- 0
  } else if (stat_id == "mean") {
    defaults$number_format$n_dec <- 1
  } else if (stat_id == "count") {
    defaults$number_format$n_dec <- 0
  }

  # Show value labels for bar/column charts
  if (chart_type %in% c("column_grouped", "column_stacked", "column_stacked_prop", "bar_grouped", "bar_stacked", "bar_stacked_prop")) {
    defaults$labels <- TRUE
  }

  # Add transparency for area charts to make overlapping series visible
  if (chart_type %in% c("area", "area_stacked")) {
    defaults$area_opacity <- 0.6
  }

  defaults
}

#' Apply Flourish settings to widget
#' @keywords internal
.apply_flourish_settings <- function(widget, settings, chart_type) {
  if (is.null(settings) || length(settings) == 0) {
    return(widget)
  }

  # Helper function to set nested values
  set_nested <- function(lst, path, value) {
    if (length(path) == 1) {
      lst[[path]] <- value
    } else {
      if (is.null(lst[[path[1]]])) {
        lst[[path[1]]] <- list()
      }
      lst[[path[1]]] <- set_nested(lst[[path[1]]], path[-1], value)
    }
    lst
  }

  # Apply each setting using dot notation
  for (setting_name in names(settings)) {
    setting_value <- settings[[setting_name]]

    # Split by dots to create nested structure
    parts <- strsplit(setting_name, "\\.")[[1]]

    # Update widget$x$state
    widget$x$state <- set_nested(widget$x$state, parts, setting_value)
  }

  widget
}

#' Create data bindings for Flourish charts
#'
#' Creates column bindings based on chart type and data format.
#'
#' @param chart_type Character string specifying the Flourish chart type
#' @param data Data frame to create bindings for
#' @param long Logical indicating whether data is in long format (default TRUE)
#'
#' @return Named list of column bindings suitable for Flourish charts
#' @keywords internal
.make_bindings <- function(chart_type, data, long = TRUE) {
  # For Sprint 1 we implement just a generic heuristic; user can override later.
  if (long) {
    list(
      x      = "row_label",
      y      = "value",
      series = "series"
    )
  } else {
    # wide table -> tabular chart_type
    list(
      rows    = "row_label"
      # Each numeric column auto-bound in Flourish for Table chart_type
    )
  }
}

#' Print method for flourish_tab objects
#'
#' Displays a summary of a flourish_tab object including the template type,
#' data bindings structure, and a preview of the data.
#'
#' @param x A flourish_tab object created by \code{\link{tab_to_flourish}}
#' @param n Integer. Number of data rows to display in preview (default 10)
#' @param ... Additional arguments passed to print methods
#' @return Invisibly returns the input object \code{x}
#' @method print flourish_tab
#' @export
#'
#' @examples
#' \dontrun{
#' # Create and print a flourish_tab object
#' f_tab <- mtcars %>% tab(gear, cyl) %>% tab_to_flourish()
#' print(f_tab)  # or just: f_tab
#' }
print.flourish_tab <- function(x, n = 10, ...) {
  cat("<flourish_tab>  chart_type:", ifelse(is.null(x$chart_type),"(unspecified)",x$chart_type), "\n")
  cat("bindings:\n")
  y <- utils::capture.output(str(x$bindings, give.attr = FALSE))
  cat("  ", paste(y, collapse = "\n  "), "\n", sep = "")

  if (!is.null(x$settings) && length(x$settings) > 0) {
    cat("settings:\n")
    z <- utils::capture.output(str(x$settings, give.attr = FALSE))
    cat("  ", paste(z, collapse = "\n  "), "\n", sep = "")
  } else {
    cat("settings: (none)\n")
  }

  cat("\ndata (first rows):\n")
  print(utils::head(x$data, n))
  invisible(x)
}

#' Preview a flourish_tab object in the browser
#'
#' Creates an interactive Flourish chart from a flourish_tab object and displays
#' it in the browser. Requires a Flourish API key and the flourishcharts package.
#'
#' @param x A flourish_tab object created by \code{\link{tab_to_flourish}}
#' @param api_key Flourish API key. Defaults to FLOURISH_API_KEY environment variable
#' @param display How to display the widget ("inline" or "viewer")
#' @param viewer Function to use for displaying the chart. Defaults to getOption("viewer")
#'   or utils::browseURL if not available
#' @param ... Additional arguments passed to htmlwidgets::saveWidget()
#'
#' @return Invisibly returns the Flourish widget object
#' @export
#'
#' @examples
#' \dontrun{
#' # Set your Flourish API key
#' Sys.setenv(FLOURISH_API_KEY = "your_api_key_here")
#'
#' # Create and preview a chart
#' data %>%
#'   tab(satisfaction, region) %>%
#'   tab_to_flourish() %>%
#'   preview_flourish()
#' }
preview_flourish <- function(x,
                             api_key    = Sys.getenv("FLOURISH_API_KEY"),
                             display    = c("inline", "viewer"),
                             viewer     = getOption("viewer", utils::browseURL),
                             ...) {

  stopifnot(inherits(x, "flourish_tab"))

  if (identical(api_key, "") || is.null(api_key)) {
    stop("Flourish API key not found.  ",
         "Set it as FLOURISH_API_KEY in your .Renviron or pass api_key =")
  }

  if (!requireNamespace("flourishcharts", quietly = TRUE))
    stop("Install the **flourishcharts** package first: install.packages('flourishcharts')")

  # 1. Choose chart type ------------------------------------------------
  chart_type <- x$chart_type

  if (is.null(chart_type)) {
    stop("No chart type stored in flourish_tab object. ",
         "Ensure tab_to_flourish() set a chart_type.")
  }

  # 2. Spawn an empty Flourish widget ----------------------------------
  widget <- flourishcharts::flourish(
    chart_type = chart_type,
    api_key    = api_key
  )

  # 3. Automatically bind data ----------------------------------------
  data_for_binding <- .replace_na_with_empty_strings(.as_plain_data_frame(x$data))
  widget <- .bind_data_auto(widget, x, chart_type, api_key, data_for_binding = data_for_binding)

  # 4. Apply settings if present
  if (!is.null(x$settings) && length(x$settings) > 0) {
    if (chart_type == "table") {
      # Use set_table_details for table charts
      widget <- .apply_table_flourish_settings(widget, x$settings)
    } else {
      # Use generic settings application for other chart types
      widget <- .apply_flourish_settings(widget, x$settings, chart_type)
    }
  }

  # 5. Display -------------------------------------------------
  display <- match.arg(display)

  if (display == "viewer" && interactive()) {
    html_path <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(widget, html_path, selfcontained = TRUE, ...)
    viewer(html_path)
    invisible(widget)
  } else {
    # Default: print widget for inline display
    print(widget)
  }

  invisible(widget)
}

#' Bind data to Flourish widget based on chart type
#' @keywords internal
.bind_data_auto <- function(widget, ft, chart_type, api_key, data_for_binding = NULL) {

  b <- ft$bindings
  data <- if (is.null(data_for_binding)) ft$data else data_for_binding

  tryCatch({
    # Special case for table (uses different parameter name)
    if (isTRUE(b$rows_data)) {
      return(.safe_bind(flourishcharts::bind_table_data, widget, rows_data = data, columns = names(data)))
    }

    # For line-bar-pie charts, we need to handle the specific binding structure
    if (chart_type %in% c("bar_grouped", "bar_stacked", "bar_stacked_prop",
                          "column_grouped", "column_stacked", "column_stacked_prop",
                          "line", "area", "area_stacked", "donut")) {

      # Build the arguments dynamically - only include non-NULL bindings
      bind_args <- list(widget, data = data)

      if (!is.null(b$label) && b$label %in% names(data)) {
        bind_args$label <- b$label
      }
      if (!is.null(b$value)) {
        # Handle both single column and multiple columns
        if (length(b$value) == 1 && b$value %in% names(data)) {
          bind_args$value <- b$value
        } else if (all(b$value %in% names(data))) {
          # Multiple columns - pass the column names directly
          bind_args$value <- b$value
        }
      }
      if (!is.null(b$facet) && b$facet %in% names(data)) {
        bind_args$facet <- b$facet
      }

      # The flourishcharts function expects specific parameter names
      return(do.call(.safe_bind, c(list(flourishcharts::bind_line_bar_pie_data), bind_args)))
    }

    # Handle other chart types
    bind_fn <- switch(
      chart_type,
      "table" = flourishcharts::bind_table_data,
      stop("Unsupported chart type: ", chart_type)
    )

    # Build arguments list from bindings
    args <- list(widget, data = data)

    # Map our generic binding names to chart-specific parameter names
    if (!is.null(b$label)) args$label <- b$label
    if (!is.null(b$value)) {
      args$value <- b$value
    }
    if (!is.null(b$x)) args$x <- b$x
    if (!is.null(b$y)) args$y <- b$y
    if (!is.null(b$name)) args$name <- b$name
    if (!is.null(b$series)) args$series <- b$series
    if (!is.null(b$color)) args$color <- b$color
    if (!is.null(b$size)) args$size <- b$size
    if (!is.null(b$shape)) args$shape <- b$shape
    if (!is.null(b$slider)) args$slider <- b$slider

    # Call the bind function with safe wrapper
    do.call(.safe_bind, c(list(bind_fn), args))

  }, error = function(e) {
    # Fallback to table on error - create a new table widget
    warning("Failed to bind data for '", chart_type, "': ", e$message,
            ". Falling back to table view.")

    # Create a new table widget
    table_widget <- flourishcharts::flourish(
      chart_type = "table",
      api_key = api_key
    )

    # Bind the data as a table
    .safe_bind(flourishcharts::bind_table_data, table_widget, rows_data = data, columns = names(data))
  })
}

#' Guess a sensible Flourish chart_type from a tab_result
#' @keywords internal
guess_flourish_chart_type <- function(tab_result) {

  stopifnot(inherits(tab_result, "tab_result"))

  # Extract info and get dimensions without materializing
  stat_info <- .extract_stat_info_safe(tab_result)

  dims <- .get_grid_dimensions(tab_result,
                               exclude_base = TRUE,
                               exclude_summary_rows = FALSE,
                               exclude_summary_cols = FALSE)
  
  n_rows <- dims$nrow
  n_cols <- dims$ncol
  
  # Get row labels for length check
  if (inherits(tab_result, "tab_cell_collection")) {
    row_labels <- tab_result$layout$row_labels
    # Filter out base and summary
    if (stat_info$base_label %in% row_labels) {
      row_labels <- row_labels[row_labels != stat_info$base_label]
    }
    if (!is.null(stat_info$summary_row) && stat_info$summary_row %in% row_labels) {
      row_labels <- row_labels[row_labels != stat_info$summary_row]
    }
  } else {
    row_labels <- tab_result$row_label
    if (stat_info$base_label %in% row_labels) {
      row_labels <- row_labels[row_labels != stat_info$base_label]
    }
    if (!is.null(stat_info$summary_row) && stat_info$summary_row %in% row_labels) {
      row_labels <- row_labels[row_labels != stat_info$summary_row]
    }
  }
  
  max_label_char <- if (length(row_labels) > 0) max(nchar(row_labels)) else 0
  stat_id <- stat_info$id

  # Check there's data
  if (n_rows == 0 || n_cols == 0){
    stop("nrow or ncol 0")
  }

  # Single cell
  if (n_rows == 1 && n_cols == 1) {
    return("table")
  }

  # Decision tree based on statistic and structure
  chart_type <- switch(
    stat_id,

    "column_pct" = {
      if (n_rows <= 3 && n_cols > 2) { # Few series, multiple columns
        "column_stacked"
      } else if (n_rows >= 2 && n_cols <= 4) { # Many series, few columns
        if (max_label_char > 20) { # bar if labels are really long
          "bar_grouped"
        } else {
          "column_grouped"
        }
      } else {
        "table"
      }
    },

    # same as column_pct, just reversed
    "row_pct" = {
      if (n_cols <= 3 && n_rows > 2) {
        "column_stacked"
      } else if (n_cols >= 2 && n_rows <= 4) {
        if (max_label_char > 20) {
          "bar_grouped"
        } else {
          "column_grouped"
        }
      } else {
        "table"
      }
    },

    "index" = {
      if (n_rows >= 2 && n_cols <= 2) {
        "column_grouped"  # Many series, max 2 columns
      } else {
        "table"
      }
    },

    "count" = {
      if (n_rows == 1 || n_cols == 1) {
        "column_grouped"
      } else if (n_rows >= 2 && n_cols <= 4) {
        "column_grouped"
      } else {
        "table"
      }
    },

    "mean" = ,
    "median" = ,
    "sd" = ,
    "cv" = {
      if (n_rows == 1 || n_cols == 1) {
        "column_grouped"       # Simple numerics as bars
      } else if (n_cols >= 4 && all(grepl("^(19|20)\\d{2}$", names(df)[-1]))) {
        "line"                 # Time series pattern detected
      } else {
        "table"                # Default for numerics
      }
    },

    # Default
    "table"
  )

  chart_type
}

#' Get default table settings
#' @keywords internal
.get_default_table_settings <- function() {
  list(
    color_mode = "none",
    color_midpoint = "mean",
    color_palette = NULL,
    rows_per_page = 20,
    show_sorting = TRUE,
    show_search = TRUE,
    first_column_width = 2
  )
}

#' Validate table settings
#' @keywords internal
.validate_table_settings <- function(user_settings) {
  # Validate color_mode if provided
  if (!is.null(user_settings$color_mode)) {
    if (!user_settings$color_mode %in% c("none", "heatmap")) {
      stop("color_mode must be 'none' or 'heatmap'")
    }
  }
  
  user_settings
}

#' Build Flourish color rules for heatmap
#' @keywords internal
.build_heatmap_color_rules <- function(df, ts) {
  # Get value columns (excluding row_label)
  value_cols <- setdiff(names(df), "row_label")
  
  if (length(value_cols) == 0) {
    return(NULL)
  }
  
  # Get palette colors (soft diverging palette by default)
  if (is.null(ts$color_palette) || length(ts$color_palette) < 3) {
    colors <- c("#D4E6F1", "#FFFFFF", "#F8D7DA")  # Soft blue-white-soft red
  } else {
    colors <- ts$color_palette[1:3]
  }
  
  rules <- character()
  
  # Apply separate scale for each column
  for (col in value_cols) {
    col_vals <- as.numeric(df[[col]])
    col_vals <- col_vals[!is.na(col_vals)]
    
    if (length(col_vals) == 0) next
    
    domain_min <- min(col_vals)
    domain_max <- max(col_vals)
    domain_mid <- if (is.character(ts$color_midpoint)) {
      if (ts$color_midpoint == "mean") mean(col_vals)
      else if (ts$color_midpoint == "median") median(col_vals)
      else (domain_min + domain_max) / 2
    } else {
      ts$color_midpoint
    }
    
    rule <- sprintf("%s >> %s >> %s : %s : %.4f >> %.4f >> %.4f",
                    colors[1], colors[2], colors[3], col,
                    domain_min, domain_mid, domain_max)
    rules <- c(rules, rule)
  }
  
  paste(rules, collapse = "\n")
}

#' Generate Flourish settings from table_settings
#' @keywords internal
.get_table_flourish_settings <- function(table_settings, df, stat_info) {
  # Start with defaults
  ts <- .get_default_table_settings()
  
  # Override with user settings (already validated if not NULL)
  if (!is.null(table_settings)) {
    for (nm in names(table_settings)) {
      ts[[nm]] <- table_settings[[nm]]
    }
  }
  
  settings <- list()
  
  # Pagination
  settings$pagination_amount <- ts$rows_per_page
  
  # Sorting (must be "all", "none", or "custom")
  settings$sorting_enabled <- if (ts$show_sorting) "all" else "none"
  
  # Search/filter (boolean)
  settings$search_enabled <- ts$show_search
  
  # Column alignment (must be "start", "center", or "end")
  settings$cell_horizontal_alignment <- "start"
  settings$cell_numeric_horizontal_alignment <- "end"
  
  # Column widths (make first column wider)
  # column_width_mode must be "auto", "equal", or "fixed"
  value_cols <- setdiff(names(df), "row_label")
  widths <- c(ts$first_column_width, rep(1, length(value_cols)))
  settings$column_width_mode <- "fixed"
  settings$column_widths <- paste(widths, collapse = ",")
  
  # Header styling
  settings$header_font_weight <- "bold"
  
  # Color coding settings (heatmap with column-based gradients)
  if (ts$color_mode == "heatmap") {
    settings$cell_fill_custom_enabled <- TRUE
    color_rules <- .build_heatmap_color_rules(df, ts)
    if (!is.null(color_rules)) {
      settings$cell_fill_custom_numeric <- color_rules
    }
  }
  
  settings
}

#' Apply table settings to Flourish widget using set_table_details
#' @keywords internal
.apply_table_flourish_settings <- function(widget, settings) {
  if (is.null(settings) || length(settings) == 0) {
    return(widget)
  }
  
  # Call set_table_details with the settings
  # Only pass non-NULL values
  args <- c(list(widget), settings[!sapply(settings, is.null)])
  
  tryCatch({
    do.call(flourishcharts::set_table_details, args)
  }, error = function(e) {
    warning("Failed to apply table settings: ", e$message)
    widget
  })
}

#' Safe wrapper for flourishcharts bind functions
#' Removes null bindings to avoid Flourish JavaScript errors
#' @keywords internal
.safe_bind <- function(bind_fn, widget, ...) {
  # Filter out NULL arguments before calling bind function
  args <- list(...)
  args <- args[!sapply(args, is.null)]

  # Call the bind function with non-NULL arguments
  result <- do.call(bind_fn, c(list(widget), args))

  # Remove null bindings that cause JavaScript errors
  if (!is.null(result$x$bindings)) {
    # Use base R instead of purrr::compact
    result$x$bindings <- result$x$bindings[!sapply(result$x$bindings, is.null)]
  }

  result
}
