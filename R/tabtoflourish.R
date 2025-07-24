
#' Convert a surveydatar tab_result into a Flourish-ready object
#'
#' @param tab           A tab_result produced by surveydatar::tab()
#' @param chart_type    Optional character; Flourish chart type
#' @param long          Should the data be pivoted to long format? (default TRUE)
#' @param strip_base    Remove the "Base (n)" row? (default TRUE)
#' @param strip_summary_rows Remove NET / Avg rows? (default TRUE)
#' @param strip_summary_cols Remove Total / NET columns? (default TRUE)
#' @param percent_scale How percentages are stored: "auto", "0-1", "0-100"
#' @param settings Named list corresponding to flourish settings with dot notation e.g. "number_format.suffix" = "%"
#' @param ...           Future arguments
#'
#' @return An object of class "flourish_tab" containing:
#'   * data      – a data.frame to feed into flourishcharts::bind_*()
#'   * bindings  – named list mapping column names to Flourish roles
#'   * chart_type  – character chart_type hint (may be NULL)
#'
#' @export
#'
#' @examples
#' f_tab <- data %>% tab(q1, gender) %>% tab_to_flourish()
#' f_tab                      # shows bindings + head(data)
#' # flourishcharts::flourish(f_tab$data, f_tab$chart_type, bindings = f_tab$bindings)
#' Main function remains clean and high-level
tab_to_flourish <- function(tab_result,
                            chart_type    = NULL,
                            long          = TRUE,
                            strip_base    = TRUE,
                            strip_summary_rows = TRUE,
                            strip_summary_cols = TRUE,
                            percent_scale = c("auto", "0-1", "0-100"),
                            settings      = NULL,
                            ...) {

  stopifnot(inherits(tab_result, "tab_result"))
  percent_scale <- match.arg(percent_scale)

  # 1. Extract metadata
  stat_info <- .extract_stat_info(tab_result)

  # 2. Clean data
  df <- .clean_tab_data(tab_result, stat_info,
                        strip_base = strip_base,
                        strip_summary_rows = strip_summary_rows,
                        strip_summary_cols = strip_summary_cols)

  # 3. Determine chart type
  chart_type <- chart_type %||% guess_flourish_chart_type(tab_result)

  # 4. Transform data for chart type
  transform_result <- .transform_for_chart_type(df, chart_type, long = long)

  # 5. Apply percent scaling
  data_out <- .apply_percent_scaling(transform_result$data, percent_scale, stat_info$id)

  # 6. Merge default and user settings
  default_settings <- .get_default_settings(stat_info$id, chart_type)
  final_settings <- if (!is.null(settings)) {
    utils::modifyList(default_settings, settings)
  } else {
    default_settings
  }

  # Return flourish_tab object
  res <- list(
    data     = data_out,
    bindings = transform_result$bindings,
    chart_type = chart_type,
    settings = final_settings
  )
  class(res) <- "flourish_tab"
  res
}

#' Extract statistic information from tab result
#' @keywords internal
.extract_stat_info <- function(tab_result) {
  stat <- attr(tab_result, "statistic")

  # Get summary labels from attributes first (what was actually used)
  actual_summary_row <- attr(tab_result, "summary_row_label")
  actual_summary_col <- attr(tab_result, "summary_col_label")

  list(
    stat = stat,
    id = stat$id,
    base_label = stat$base_label,
    summary_row = if (!is.null(actual_summary_row)) {
      actual_summary_row
    } else {
      stat$summary_row
    },
    summary_col = if (!is.null(actual_summary_col)) {
      actual_summary_col
    } else {
      stat$summary_col
    }
  )
}

#' Clean tab data by removing metadata rows/columns
#' @keywords internal
.clean_tab_data <- function(tab_result, stat_info, strip_base, strip_summary_rows, strip_summary_cols) {
  df <- tab_result

  if (strip_base && stat_info$base_label %in% df$row_label) {
    df <- df[df$row_label != stat_info$base_label, , drop = FALSE]
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
.transform_for_chart_type <- function(df, chart_type, long = TRUE) {
  chart_type <- tolower(chart_type)

  # Get data dimensions
  n_rows <- nrow(df)
  n_cols <- ncol(df) - 1
  col_names <- names(df)[-1]

  # Charts that work with wide format
  if (chart_type %in% c("table", "radar")) {
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

    "scatter" = .transform_scatter,

    "donut" = ,
    "pie" = .transform_pie_donut,

    # Default transformation
    function(df, ...) .transform_default(df, long = long)
  )

  transform_fn(df, n_rows, n_cols, col_names)
}

#' Transform for stacked bar charts
#' @keywords internal
.transform_stacked_bar <- function(df, n_rows, n_cols, col_names) {
  list(
    data = df,
    bindings = list(
      label = "row_label",   # X-axis
      value = col_names      # Y-axis values (multiple columns)
    )
  )
}

#' Transform for grouped bar charts
#' @keywords internal
.transform_grouped_bar <- function(df, n_rows, n_cols, col_names) {
  list(
    data = df,
    bindings = list(
      label = "row_label",   # X-axis categories
      value = col_names      # Y-axis values (multiple columns for groups)
    )
  )
}

#' Transform for line/area charts
#' @keywords internal
.transform_line_area <- function(df, n_rows, n_cols, col_names) {
  long_df <- tidyr::pivot_longer(
    df,
    cols = -row_label,
    names_to = "period",
    values_to = "value"
  )
  list(
    data = long_df,
    bindings = list(
      label = "period",      # X-axis
      value = "value",       # Y-axis
      facet = "row_label"    # Different lines
    )
  )
}

#' Transform for scatter plots
#' @keywords internal
.transform_scatter <- function(df, n_rows, n_cols, col_names) {
  if (n_cols == 1) {
    # Single column - use row index as X
    scatter_df <- data.frame(
      x = seq_len(n_rows),
      y = df[[2]],
      name = df$row_label
    )
    list(
      data = scatter_df,
      bindings = list(x = "x", y = "y", name = "name")
    )
  } else if (n_cols == 2) {
    # Two columns - use first as X, second as Y
    scatter_df <- data.frame(
      x = df[[2]],
      y = df[[3]],
      name = df$row_label
    )
    list(
      data = scatter_df,
      bindings = list(x = "x", y = "y", name = "name")
    )
  } else {
    # Multiple columns - create series for each column pair
    # Each column becomes a point, with row_label on x-axis
    long_df <- tidyr::pivot_longer(
      df,
      cols = -row_label,
      names_to = "series",
      values_to = "y"
    )
    # Use row labels as x-axis categories
    list(
      data = long_df,
      bindings = list(
        x = "row_label",
        y = "y",
        series = "series"  # Connect points from same column
      )
    )
  }
}

#' Transform for pie/donut charts
#' @keywords internal
.transform_pie_donut <- function(df, n_rows, n_cols, col_names) {
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
  bindings <- switch(
    chart_type,
    "table" = list(rows_data = TRUE),
    "radar" = list(name = "row_label", values = names(df)[-1]),  # Changed 'value' to 'values'
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

#' Apply percent scaling to numeric columns
#' @keywords internal
.apply_percent_scaling <- function(data, percent_scale, stat_id) {
  numeric_cols <- names(data)[sapply(data, is.numeric)]

  .detect_percent_scale <- function(df, cols) {
    rng <- range(unlist(df[cols]), na.rm = TRUE)
    if (rng[2] <= 1.001) "0-1" else "0-100"
  }

  if (percent_scale == "auto") {
    percent_scale <- .detect_percent_scale(data, numeric_cols)
  }

  if (percent_scale == "0-1" && stat_id %in% c("column_pct", "row_pct")) {
    data[numeric_cols] <- lapply(data[numeric_cols], function(x) x * 100)
  }

  data
}

#' Generate default Flourish settings based on statistic type
#' @keywords internal
.get_default_settings <- function(stat_id, chart_type) {
  defaults <- list()

  # Value formatting based on statistic
  if (stat_id %in% c("column_pct", "row_pct") && chart_type %in% c("column_grouped", "column_stacked", "bar_grouped", "bar_stacked")) {
    defaults$number_format$n_dec <- 1
    defaults$number_format$suffix <- "%"
    defaults$y$linear_max <- 100
    defaults$y$linear_min <- 0
  } else if (stat_id == "mean") {
    defaults$number_format$n_dec <- 1
  } else if (stat_id == "count") {
    defaults$number_format$n_dec <- 0
  }

  # Show value labels for smaller datasets
  if (chart_type %in% c("column_grouped", "column_stacked", "bar_grouped", "bar_stacked")) {
    defaults$labels <- TRUE
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
#' Creates appropriate column bindings based on chart type and data format.
#' This is a simple helper function that provides generic heuristics for
#' binding data columns to chart aesthetics.
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
#' @param chart_type Optional chart type to override the one in the flourish_tab object
#' @param api_key Flourish API key. Defaults to FLOURISH_API_KEY environment variable
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
                             chart_type = NULL,
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
  # Remove the guess_flourish_chart_type call since x is already transformed
  if(is.null(chart_type)){
    chart_type <- x$chart_type
  }

  if (is.null(chart_type)) {
    stop("No chart type specified and none found in flourish_tab object. ",
         "Specify chart_type or ensure tab_to_flourish() set a chart_type.")
  }

  # 2. Spawn an empty Flourish widget ----------------------------------
  widget <- flourishcharts::flourish(
    chart_type = chart_type,
    api_key    = api_key
  )

  # 3. Automatically bind data ----------------------------------------
  widget <- .bind_data_auto(widget, x, chart_type, api_key)

  # 4. Apply settings if present
  if (!is.null(x$settings)) {
    widget <- .apply_flourish_settings(widget, x$settings, chart_type)
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
.bind_data_auto <- function(widget, ft, chart_type, api_key) {

  b <- ft$bindings

  tryCatch({
    # Special case for table (uses different parameter name)
    if (isTRUE(b$rows_data)) {
      return(.safe_bind(flourishcharts::bind_table_data, widget, rows_data = ft$data, columns = names(ft$data)))
    }

    # For line-bar-pie charts, we need to handle the specific binding structure
    if (chart_type %in% c("bar_grouped", "bar_stacked", "bar_stacked_prop",
                          "column_grouped", "column_stacked", "column_stacked_prop",
                          "line", "area", "area_stacked", "donut", "pie")) {

      # Build the arguments dynamically - only include non-NULL bindings
      bind_args <- list(widget, data = ft$data)

      if (!is.null(b$label) && b$label %in% names(ft$data)) {
        bind_args$label <- b$label
      }
      if (!is.null(b$value)) {
        # Handle both single column and multiple columns
        if (length(b$value) == 1 && b$value %in% names(ft$data)) {
          bind_args$value <- b$value
        } else if (all(b$value %in% names(ft$data))) {
          # Multiple columns - pass the column names directly
          bind_args$value <- b$value
        }
      }
      if (!is.null(b$facet) && b$facet %in% names(ft$data)) {
        bind_args$facet <- b$facet
      }

      # The flourishcharts function expects specific parameter names
      return(do.call(.safe_bind, c(list(flourishcharts::bind_line_bar_pie_data), bind_args)))
    }

    # Handle other chart types
    bind_fn <- switch(
      chart_type,
      "scatter" = flourishcharts::bind_scatter_data,
      "radar" = flourishcharts::bind_radar_data,
      "table" = flourishcharts::bind_table_data,
      stop("Unsupported chart type: ", chart_type)
    )

    # Build arguments list from bindings
    args <- list(widget, data = ft$data)

    # Map our generic binding names to chart-specific parameter names
    if (!is.null(b$label)) args$label <- b$label
    if (!is.null(b$value)) {
      # For scatter, this might be 'values' for radar
      args$value <- b$value
    }
    if (!is.null(b$values)) {
      # For radar chart specifically
      args$values <- b$values
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
    .safe_bind(flourishcharts::bind_table_data, table_widget, rows_data = ft$data, columns = names(ft$data))
  })
}

#' Guess a sensible Flourish chart_type from a tab_result
#' @keywords internal
guess_flourish_chart_type <- function(tab) {

  stopifnot(inherits(tab, "tab_result"))

  # Extract info and clean data to get actual dimensions
  stat_info <- .extract_stat_info(tab)
  df <- .clean_tab_data(tab, stat_info,
                        strip_base = TRUE,
                        strip_summary_rows = TRUE,
                        strip_summary_cols = TRUE)

  # Get dimensions
  n_rows <- nrow(df)
  n_cols <- ncol(df) - 1  # Excluding row_label
  max_label_char <- max(nchar(df[,1]))
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
        "scatter"             # Default for numerics
      }
    },

    # Default
    "table"
  )

  chart_type
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
