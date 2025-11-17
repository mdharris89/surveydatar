# Tab to Reactable Conversion
#
# Functions for converting tab results to reactable tables with styling.

#' Convert a surveydatar tab_result into a reactable-ready object
#'
#' @param tab_result A tab_result produced by surveydatar::tab()
#' @param strip_summary_rows Remove NET / Avg rows? (default FALSE)
#' @param strip_summary_cols Remove Total / NET columns? (default FALSE)
#' @param show_base Include base (n) row or column? (default TRUE)
#' @param color_mode Character: "none" (default), "heatmap", "top_n", or "significance"
#' @param color_scope Character: "column" (default), "row", or "table" (for heatmap and top_n modes)
#' @param color_palette Character vector of colors for gradients (NULL uses defaults)
#' @param color_midpoint Numeric value, "mean", or "median" for diverging color scale (default "mean")
#' @param color_top_n Integer: number of top values to highlight (required when color_mode = "top_n")
#' @param sig_comparison Character: name of significance comparison to use (auto-selects if only one exists)
#' @param sig_symbol Logical: whether to append significance symbols (default TRUE)
#' @param show_tooltips Logical: whether to show hover tooltips with cell information (default TRUE)
#' @param freeze_headers Logical: freeze table headers when scrolling (default TRUE)
#' @param freeze_first_column Logical: freeze first column when scrolling (default TRUE)
#' @param enable_sorting Logical: enable column sorting (default TRUE)
#' @param pagination Integer: rows per page, or FALSE to disable pagination (default 20)
#' @param searchable Logical: enable search/filter box (default FALSE)
#' @param decimal_places Integer: number of decimal places to display (default 1)
#' @param font_family Character: CSS font-family to use for the table (default "Consolas, Monaco, 'Courier New', Courier, monospace")
#' @param font_size Character: CSS font-size for the table (e.g., "12px", "0.9em"). Default is "12px".
#' @param ... Additional arguments (reserved for future use)
#'
#' @return An object of class "reactable_tab" containing:
#'   * data      – a data.frame with numeric values
#'   * metadata  – list with stat_info, base info, summary info, significance data
#'   * settings  – list with all display and color settings
#'
#' @details
#' This function prepares a tab_result for display as an interactive HTML table using
#' the reactable package.
#'
#' ## Color Modes
#'
#' * `"none"`: No color coding (default)
#' * `"heatmap"`: Gradient coloring based on values, with scope determining the scale
#' * `"top_n"`: Highlight the top N values within the specified scope
#' * `"significance"`: Color cells based on significance test results (requires significance data)
#'
#' ## Significance Display
#'
#' When significance testing has been applied to the tab_result using `add_sig()` or
#' `add_sig_all()`, you can display results using:
#'
#' * `color_mode = "significance"`: Colors cells based on significance direction and level
#'
#' * `sig_symbol = TRUE`: Appends symbols to values
#'   - "↑*" for significantly above at 90% (p < 0.10)
#'   - "↑**" for significantly above at 95% (p < 0.05)
#'   - "↓*" for significantly below at 90%
#'   - "↓**" for significantly below at 95%
#'
#' These two options can be used independently or combined.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' r_tab <- data %>% tab(q1, gender) %>% tab_to_reactable()
#' display_reactable(r_tab)
#'
#' # With heatmap coloring
#' r_tab <- data %>%
#'   tab(satisfaction, region) %>%
#'   tab_to_reactable(color_mode = "heatmap", color_scope = "column")
#' display_reactable(r_tab)
#'
#' # With significance testing
#' r_tab <- data %>%
#'   tab(satisfaction, region) %>%
#'   add_sig(versus = "North") %>%
#'   tab_to_reactable(color_mode = "significance", sig_symbol = TRUE)
#' display_reactable(r_tab)
#' }
tab_to_reactable <- function(tab_result,
                             strip_summary_rows = FALSE,
                             strip_summary_cols = FALSE,
                             show_base = TRUE,
                             color_mode = c("none", "heatmap", "top_n", "significance"),
                             color_scope = c("column", "row", "table"),
                             color_palette = NULL,
                             color_midpoint = "mean",
                             color_top_n = NULL,
                             sig_comparison = NULL,
                             sig_symbol = TRUE,
                             show_tooltips = TRUE,
                             freeze_headers = TRUE,
                             freeze_first_column = TRUE,
                             enable_sorting = TRUE,
                             pagination = 20,
                             searchable = FALSE,
                             decimal_places = 1,
                             font_family = "Consolas, Monaco, 'Courier New', Courier, monospace",
                             font_size = "12px",
                             label_mode = "full",
                             ...) {
  
  # Validate input
  stopifnot(inherits(tab_result, "tab_result"))
  
  # Match arguments
  color_mode <- match.arg(color_mode)
  color_scope <- match.arg(color_scope)
  
  # Validate color_mode specific parameters
  if (color_mode == "top_n" && is.null(color_top_n)) {
    stop("color_top_n must be specified when color_mode = 'top_n'")
  }
  
  # Extract metadata first (before any modifications)
  metadata <- .extract_reactable_metadata(tab_result)
  
  # Store cell_store and layout for tooltip generation (if available)
  if (inherits(tab_result, "tab_cell_collection")) {
    metadata$cell_store <- tab_result$cell_store
    metadata$layout <- tab_result$layout
  }
  
  # Apply native operations if cell-based
  if (inherits(tab_result, "tab_cell_collection")) {
    # Unlike Flourish, reactable CAN show base - respect show_base parameter
    if (!show_base) {
      tab_result <- hide_base(tab_result)
    }
    
    # Strip summaries if requested
    if (strip_summary_rows || strip_summary_cols) {
      tab_result <- hide_summary(tab_result, 
                                 rows = strip_summary_rows,
                                 cols = strip_summary_cols)
    }
    
    # Materialize with class preservation
    # Pass show_base to control whether base row is added during materialization
    tab_result <- .materialize_for_export(tab_result, 
                                         show_base = show_base, 
                                         label_mode = label_mode)
    
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
  
  # Validate sig_comparison
  if (color_mode == "significance" || sig_symbol) {
    metadata$sig_comparison <- .validate_sig_comparison(metadata$significance, sig_comparison)
  }
  
  # Clean data
  df <- .clean_reactable_data(tab_result, metadata, show_base, 
                               strip_summary_rows, strip_summary_cols)
  
  # Identify special rows/columns
  special_elements <- .identify_special_elements(df, metadata, show_base)
  
  # Store settings
  settings <- list(
    color_mode = color_mode,
    color_scope = color_scope,
    color_palette = color_palette,
    color_midpoint = color_midpoint,
    color_top_n = color_top_n,
    sig_symbol = sig_symbol,
    show_tooltips = show_tooltips,
    freeze_headers = freeze_headers,
    freeze_first_column = freeze_first_column,
    enable_sorting = enable_sorting,
    pagination = pagination,
    searchable = searchable,
    decimal_places = decimal_places,
    font_family = font_family,
    font_size = font_size
  )
  
  # Return structured object
  res <- list(
    data = df,
    metadata = metadata,
    settings = settings,
    special_elements = special_elements
  )
  class(res) <- "reactable_tab"
  res
}

#' Extract metadata from tab_result for reactable display
#' @keywords internal
.extract_reactable_metadata <- function(tab_result) {
  # Use the safe version from export_helpers
  stat_info <- .extract_stat_info_safe(tab_result)
  
  list(
    stat_info = stat_info,
    base_label = stat_info$base_label,
    base_orientation = attr(tab_result, "base_orientation"),
    base_matrix = attr(tab_result, "base_matrix"),
    summary_row_label = stat_info$summary_row,
    summary_col_label = stat_info$summary_col,
    significance = attr(tab_result, "significance"),
    sig_comparison = NULL  # Will be set by validation
  )
}

#' Validate and select significance comparison
#' @keywords internal
.validate_sig_comparison <- function(significance, sig_comparison) {
  if (is.null(significance) || length(significance) == 0) {
    return(NULL)
  }
  
  if (is.null(sig_comparison)) {
    if (length(significance) == 1) {
      return(names(significance)[1])
    } else {
      stop("Multiple significance comparisons found. Please specify sig_comparison. ",
           "Available: ", paste(names(significance), collapse = ", "))
    }
  }
  
  if (!sig_comparison %in% names(significance)) {
    stop("sig_comparison '", sig_comparison, "' not found. ",
         "Available: ", paste(names(significance), collapse = ", "))
  }
  
  sig_comparison
}

#' Clean tab data for reactable display
#' @keywords internal
.clean_reactable_data <- function(tab_result, metadata, show_base, 
                                   strip_summary_rows, strip_summary_cols) {
  df <- as.data.frame(tab_result)
  
  # Remove base if requested
  if (!show_base) {
    base_label <- metadata$base_label
    if (base_label %in% df$row_label) {
      df <- df[df$row_label != base_label, , drop = FALSE]
    }
    if (base_label %in% names(df)) {
      df[[base_label]] <- NULL
    }
  }
  
  # Remove summary rows if requested
  if (strip_summary_rows && !is.null(metadata$summary_row_label)) {
    if (metadata$summary_row_label %in% df$row_label) {
      df <- df[df$row_label != metadata$summary_row_label, , drop = FALSE]
    }
  }
  
  # Remove summary columns if requested
  if (strip_summary_cols && !is.null(metadata$summary_col_label)) {
    if (metadata$summary_col_label %in% names(df)) {
      df[[metadata$summary_col_label]] <- NULL
    }
  }
  
  df
}

#' Identify special rows and columns (base, summary)
#' 
#' Uses metadata from tab to identify which rows/columns are special,
#' rather than hardcoding labels. This ensures compatibility with custom
#' summary labels (e.g., tab with custom NET labels).
#' 
#' @keywords internal
.identify_special_elements <- function(df, metadata, show_base) {
  base_rows <- character(0)
  base_cols <- character(0)
  summary_rows <- character(0)
  summary_cols <- character(0)
  
  # Identify base elements using metadata from tab
  # Base label comes from statistic$base_label (e.g., "Base (n)")
  if (show_base) {
    base_label <- metadata$base_label
    if (base_label %in% df$row_label) {
      base_rows <- base_label
    }
    if (base_label %in% names(df)) {
      base_cols <- base_label
    }
  }
  
  # Identify summary elements using metadata from tab
  # Summary labels come from the statistic (e.g., "NET", "Avg", "Total")
  if (!is.null(metadata$summary_row_label) && 
      metadata$summary_row_label %in% df$row_label) {
    summary_rows <- metadata$summary_row_label
  }
  if (!is.null(metadata$summary_col_label) && 
      metadata$summary_col_label %in% names(df)) {
    summary_cols <- metadata$summary_col_label
  }
  
  list(
    base_rows = base_rows,
    base_cols = base_cols,
    summary_rows = summary_rows,
    summary_cols = summary_cols
  )
}

#' Display a reactable_tab object as an interactive HTML table
#'
#' @param x A reactable_tab object created by \code{\link{tab_to_reactable}}
#' @param height Table height (default "auto")
#' @param width Table width (default "100%")
#' @param ... Additional arguments passed to reactable::reactable()
#'
#' @return A reactable widget or htmltools tag list
#' @export
#'
#' @examples
#' \dontrun{
#' r_tab <- data %>% tab(q1, gender) %>% tab_to_reactable()
#' display_reactable(r_tab)
#' }
display_reactable <- function(x, 
                               height = "auto",
                               width = "100%",
                               ...) {
  
  # Validate input
  stopifnot(inherits(x, "reactable_tab"))
  
  # Check for reactable package
  if (!requireNamespace("reactable", quietly = TRUE)) {
    stop("The 'reactable' package is required. Install it with: install.packages('reactable')")
  }
  
  # Check if we need to split into main + footer tables
  needs_split <- .needs_split_display(x)
  
  if (needs_split) {
    # Split data into main and footer
    split_data <- .split_data_for_display(x)
    
    # Create main table (data rows, sortable)
    main_table <- .create_main_table(x, split_data$main_data, height, width, ...)
    
    # Create footer table (base/summary rows, static)
    footer_table <- .create_footer_table(x, split_data$footer_data, width)
    
    # Combine tables with CSS styling
    .combine_tables(main_table, footer_table, x$settings$font_family, x$settings$font_size)
  } else {
    # Simple case: single table
    .create_single_table(x, height, width, ...)
  }
}

#' Check if table needs to be split into main + footer
#' @keywords internal
.needs_split_display <- function(x) {
  special <- x$special_elements
  # Need split if we have base or summary rows
  length(special$base_rows) > 0 || length(special$summary_rows) > 0
}

#' Split data into main (data rows) and footer (base/summary rows)
#' @keywords internal
.split_data_for_display <- function(x) {
  df <- x$data
  special <- x$special_elements
  
  # Identify footer rows
  footer_mask <- df$row_label %in% c(special$base_rows, special$summary_rows)
  
  # Split
  main_data <- df[!footer_mask, , drop = FALSE]
  footer_data <- df[footer_mask, , drop = FALSE]
  
  # Ensure footer rows are in correct order (summary then base)
  if (nrow(footer_data) > 0) {
    footer_order <- integer(0)
    if (length(special$summary_rows) > 0) {
      footer_order <- c(footer_order, which(footer_data$row_label %in% special$summary_rows))
    }
    if (length(special$base_rows) > 0) {
      footer_order <- c(footer_order, which(footer_data$row_label %in% special$base_rows))
    }
    footer_data <- footer_data[footer_order, , drop = FALSE]
  }
  
  list(
    main_data = main_data,
    footer_data = footer_data
  )
}

#' Create main reactable table (data rows only)
#' @keywords internal
.create_main_table <- function(x, data, height, width, ...) {
  # Build column definitions
  columns <- .build_reactable_columns(x)
  
  # Build row style function
  row_style_fn <- .build_row_style_fn(x)
  
  # Configure pagination
  pagination_setting <- if (is.numeric(x$settings$pagination) && x$settings$pagination > 0) {
    TRUE
  } else {
    FALSE
  }
  
  page_size <- if (is.numeric(x$settings$pagination)) {
    x$settings$pagination
  } else {
    20
  }
  
  # Create reactable
  reactable::reactable(
    data,
    columns = columns,
    pagination = pagination_setting,
    defaultPageSize = page_size,
    searchable = x$settings$searchable,
    highlight = TRUE,
    bordered = TRUE,
    striped = FALSE,
    sortable = x$settings$enable_sorting,
    resizable = TRUE,
    rowStyle = row_style_fn,
    height = height,
    width = width,
    class = "main-table",
    ...
  )
}

#' Create footer reactable table (base/summary rows only)
#' @keywords internal
.create_footer_table <- function(x, data, width) {
  # Build column definitions specific for footer (no sorting)
  columns <- .build_footer_columns(x, data)
  
  # Build row style function
  row_style_fn <- .build_row_style_fn(x)
  
  # Create static footer table
  reactable::reactable(
    data,
    columns = columns,
    pagination = FALSE,
    sortable = FALSE,
    searchable = FALSE,
    highlight = FALSE,
    bordered = TRUE,
    striped = FALSE,
    resizable = TRUE,
    rowStyle = row_style_fn,
    width = width,
    class = "footer-table"
  )
}

#' Build column definitions for footer table
#' @keywords internal
.build_footer_columns <- function(x, footer_data) {
  settings <- x$settings
  
  cols <- list()
  
  # First column (row_label) - twice the width of other columns, left-aligned (match main table)
  cols$row_label <- reactable::colDef(
    name = "Row Label",
    minWidth = 200,
    align = "left",
    sticky = if (settings$freeze_first_column) "left" else NULL,
    style = list(fontWeight = "500"),
    headerStyle = list(fontWeight = "bold")
  )
  
  # Data columns - use footer-specific rendering and styling, right-aligned (match main table)
  data_cols <- setdiff(names(x$data), "row_label")
  
  for (col_name in data_cols) {
    cols[[col_name]] <- reactable::colDef(
      name = col_name,
      minWidth = 100,
      align = "right",
      cell = .build_footer_cell_render_fn(x, col_name, footer_data),
      style = .build_footer_cell_style_fn(x, col_name, footer_data),
      headerStyle = list(fontWeight = "bold"),
      sortable = FALSE,
      html = TRUE  # Allow HTML rendering for tooltips
    )
  }
  
  cols
}

#' Build cell rendering function for footer table
#' @keywords internal
.build_footer_cell_render_fn <- function(x, col_name, footer_data) {
  metadata <- x$metadata
  settings <- x$settings
  special <- x$special_elements
  stat_info <- metadata$stat_info
  is_base_col <- col_name %in% special$base_cols
  is_summary_col <- col_name %in% special$summary_cols
  
  function(value, index) {
    # Handle NA
    if (is.na(value)) {
      return("")
    }
    
    # Get row label for this row in the footer
    row_label <- footer_data$row_label[index]
    
    # Check if this is a base row or summary row
    is_base_row <- row_label %in% special$base_rows
    is_summary_row <- row_label %in% special$summary_rows
    
    # Format base values as integers
    if (is_base_row || is_base_col) {
      return(.format_base_value(value))
    }
    
    # Format regular values with controlled decimal places
    formatted <- .format_value_with_decimals(value, settings$decimal_places, stat_info$stat$format_fn)
    
    # Add tooltip for data cells (not base or summary) if enabled
    if (settings$show_tooltips && !is_base_row && !is_base_col && !is_summary_row && !is_summary_col) {
      # Map footer row index to original data row index
      # Footer data is a subset of x$data, need to find the original row index
      original_index <- which(x$data$row_label == row_label)[1]
      
      if (!is.na(original_index)) {
        # Get cell specification for this position
        cell_info <- .get_cell_spec_for_position(x, original_index, col_name)
        
        if (!is.null(cell_info)) {
          # Build tooltip text
          tooltip_text <- .build_tooltip_text(
            cell_info$specification,
            stat_info$id,
            cell_info$base
          )
          
          # Wrap with tooltip
          formatted <- .wrap_with_tooltip(formatted, tooltip_text)
        }
      }
    }
    
    formatted
  }
}

#' Build cell style function for footer table
#' Footer cells should only get grey backgrounds, never color coding
#' @keywords internal
.build_footer_cell_style_fn <- function(x, col_name, footer_data) {
  special <- x$special_elements
  is_base_col <- col_name %in% special$base_cols
  is_summary_col <- col_name %in% special$summary_cols
  
  function(value, index) {
    # Get row label for this row in the footer
    row_label <- footer_data$row_label[index]
    is_base_row <- row_label %in% special$base_rows
    is_summary_row <- row_label %in% special$summary_rows
    
    style <- list()
    
    # Base column/row styling - faint grey background, no color coding
    if (is_base_col || is_base_row) {
      style$background <- "#F8F8F8"
      style$fontStyle <- "italic"
      return(style)
    }
    
    # Summary row/column styling - faint grey background, no color coding
    if (is_summary_row || is_summary_col) {
      style$background <- "#F8F8F8"
      style$fontWeight <- "bold"
      return(style)
    }
    
    style
  }
}

#' Create single table (when no split needed)
#' @keywords internal
.create_single_table <- function(x, height, width, ...) {
  columns <- .build_reactable_columns(x)
  row_style_fn <- .build_row_style_fn(x)
  
  pagination_setting <- if (is.numeric(x$settings$pagination) && x$settings$pagination > 0) {
    TRUE
  } else {
    FALSE
  }
  
  page_size <- if (is.numeric(x$settings$pagination)) {
    x$settings$pagination
  } else {
    20
  }
  
  table <- reactable::reactable(
    x$data,
    columns = columns,
    pagination = pagination_setting,
    defaultPageSize = page_size,
    searchable = x$settings$searchable,
    highlight = TRUE,
    bordered = TRUE,
    striped = FALSE,
    sortable = x$settings$enable_sorting,
    resizable = TRUE,
    rowStyle = row_style_fn,
    height = height,
    width = width,
    ...
  )
  
  # Add font and hover styling
  if (requireNamespace("htmltools", quietly = TRUE)) {
    font_css <- htmltools::tags$style(htmltools::HTML(paste0("
      .reactable {
        font-family: ", x$settings$font_family, " !important;
        font-size: ", x$settings$font_size, " !important;
      }
      .rt-thead .rt-th:hover {
        background-color: #F0F0F0 !important;
        cursor: pointer;
      }
      .cell-with-tooltip {
        position: relative;
      }
      .cell-with-tooltip::after {
        content: attr(data-tooltip);
        position: absolute;
        bottom: 50%;
        right: 100%;
        transform: translateY(50%);
        margin-right: 8px;
        padding: 8px 12px;
        background-color: #fff;
        color: #333;
        font-size: 11px;
        line-height: 1.4;
        text-align: left;
        white-space: pre-line;
        max-width: 300px;
        width: max-content;
        border-radius: 4px;
        border: 1px solid #ccc;
        box-shadow: 0 2px 8px rgba(0,0,0,0.15);
        opacity: 0;
        visibility: hidden;
        transition: opacity 0.2s, visibility 0.2s;
        pointer-events: none;
        z-index: 1000;
      }
      .cell-with-tooltip:hover::after {
        opacity: 1;
        visibility: visible;
      }
      .cell-with-tooltip::before {
        content: '';
        position: absolute;
        top: 50%;
        right: 100%;
        transform: translateY(-50%);
        margin-right: 2px;
        border: 6px solid transparent;
        border-left-color: #ccc;
        opacity: 0;
        visibility: hidden;
        transition: opacity 0.2s, visibility 0.2s;
      }
      .cell-with-tooltip:hover::before {
        opacity: 1;
        visibility: visible;
      }
      .tooltip-row {
        display: block;
        margin-bottom: 4px;
      }
      .tooltip-row:last-child {
        margin-bottom: 0;
      }
      .tooltip-label {
        font-weight: bold;
        margin-right: 6px;
        display: inline-block;
      }
      .tooltip-value {
        display: inline;
      }
    ")))

    htmltools::browsable(htmltools::tagList(font_css, table))
  } else {
    table
  }
}

#' Combine main and footer tables with CSS styling
#' @keywords internal
.combine_tables <- function(main_table, footer_table, font_family, font_size) {
  # Check for htmltools
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    warning("htmltools package not available. Returning main table only.")
    return(main_table)
  }
  
  # CSS to make tables appear connected and apply font
  css <- htmltools::tags$style(htmltools::HTML(paste0("
    .reactable-split-container {
      overflow-x: auto !important;
      overflow-y: visible !important;
    }
    .reactable-container {
      margin-bottom: 0 !important;
    }
    .main-table,
    .footer-table,
    .main-table *,
    .footer-table * {
      overflow-x: visible !important;
    }
    .main-table .ReactTable,
    .footer-table .ReactTable,
    .main-table .reactable,
    .footer-table .reactable {
      overflow: visible !important;
    }
    .main-table .rt-table,
    .footer-table .rt-table {
      font-family: ", font_family, " !important;
      font-size: ", font_size, " !important;
    }
    .main-table .rt-table {
      border-bottom: none !important;
    }
    .footer-table .rt-table {
      border-top: none !important;
      margin-top: -1px !important;
    }
    .footer-table .rt-thead {
      display: none !important;
    }
    .rt-thead .rt-th:hover {
      background-color: #F0F0F0 !important;
      cursor: pointer;
    }
    .cell-with-tooltip {
      position: relative;
    }
    .cell-with-tooltip::after {
      content: attr(data-tooltip);
      position: absolute;
      bottom: 50%;
      right: 100%;
      transform: translateY(50%);
      margin-right: 8px;
      padding: 8px 12px;
      background-color: #fff;
      color: #333;
      font-size: 11px;
      line-height: 1.4;
      text-align: left;
      white-space: pre-line;
      max-width: 300px;
      width: max-content;
      border-radius: 4px;
      border: 1px solid #ccc;
      box-shadow: 0 2px 8px rgba(0,0,0,0.15);
      opacity: 0;
      visibility: hidden;
      transition: opacity 0.2s, visibility 0.2s;
      pointer-events: none;
      z-index: 1000;
    }
    .cell-with-tooltip:hover::after {
      opacity: 1;
      visibility: visible;
    }
    .cell-with-tooltip::before {
      content: '';
      position: absolute;
      top: 50%;
      right: 100%;
      transform: translateY(-50%);
      margin-right: 2px;
      border: 6px solid transparent;
      border-left-color: #ccc;
      opacity: 0;
      visibility: hidden;
      transition: opacity 0.2s, visibility 0.2s;
    }
    .cell-with-tooltip:hover::before {
      opacity: 1;
      visibility: visible;
    }
  ")))
  
  # JavaScript to aggressively disable internal scrolling and sync widths
  sync_script <- htmltools::tags$script(htmltools::HTML("
    // Wait for both tables to be fully rendered
    setTimeout(function() {
      var mainTableContainer = document.querySelector('.main-table');
      var footerTableContainer = document.querySelector('.footer-table');
      var mainTable = document.querySelector('.main-table .rt-table');
      var footerTable = document.querySelector('.footer-table .rt-table');
      
      if (!mainTable || !footerTable) return;
      
      // Aggressively disable overflow on all internal elements
      function disableInternalScroll() {
        var mainElements = mainTableContainer.querySelectorAll('*');
        var footerElements = footerTableContainer.querySelectorAll('*');
        
        // Disable overflow on all elements inside main table
        for (var i = 0; i < mainElements.length; i++) {
          var elem = mainElements[i];
          var computed = window.getComputedStyle(elem);
          if (computed.overflowX !== 'visible') {
            elem.style.overflowX = 'visible';
            elem.style.overflowY = 'visible';
          }
        }
        
        // Disable overflow on all elements inside footer table
        for (var i = 0; i < footerElements.length; i++) {
          var elem = footerElements[i];
          var computed = window.getComputedStyle(elem);
          if (computed.overflowX !== 'visible') {
            elem.style.overflowX = 'visible';
            elem.style.overflowY = 'visible';
          }
        }
      }
      
      // Function to sync column widths from main to footer
      function syncColumnWidths() {
        var mainHeaders = mainTable.querySelectorAll('.rt-thead .rt-th');
        var footerHeaders = footerTable.querySelectorAll('.rt-thead .rt-th');
        var mainCols = mainTable.querySelectorAll('.rt-tbody .rt-tr:first-child .rt-td');
        var footerCols = footerTable.querySelectorAll('.rt-tbody .rt-tr:first-child .rt-td');
        
        // Sync header widths
        for (var i = 0; i < mainHeaders.length && i < footerHeaders.length; i++) {
          var width = mainHeaders[i].offsetWidth + 'px';
          footerHeaders[i].style.width = width;
          footerHeaders[i].style.minWidth = width;
          footerHeaders[i].style.maxWidth = width;
        }
        
        // Sync body column widths
        for (var i = 0; i < mainCols.length && i < footerCols.length; i++) {
          var width = mainCols[i].offsetWidth + 'px';
          footerCols[i].style.width = width;
          footerCols[i].style.minWidth = width;
          footerCols[i].style.maxWidth = width;
        }
      }
      
      // Initial aggressive disable
      disableInternalScroll();
      
      // Initial sync
      syncColumnWidths();
      
      // Set up MutationObserver to watch for changes
      var observer = new MutationObserver(function(mutations) {
        disableInternalScroll();  // Re-disable on any DOM changes
        syncColumnWidths();
      });
      
      // Observe both tables for any changes
      var config = { attributes: true, childList: true, subtree: true };
      observer.observe(mainTableContainer, config);
      observer.observe(footerTableContainer, config);
      
      // Periodically re-disable overflow (catch any late additions)
      setInterval(disableInternalScroll, 500);
      
      // Also sync on window resize
      window.addEventListener('resize', function() {
        disableInternalScroll();
        syncColumnWidths();
      });
    }, 100);
  "))
  
  # Combine into single container and make it browsable for RStudio Viewer
  combined <- htmltools::tagList(
    css,
    sync_script,
    htmltools::div(
      class = "reactable-split-container",
      main_table,
      footer_table
    )
  )
  
  # Make browsable so it renders in Viewer pane
  htmltools::browsable(combined)
}

#' Build column definitions for reactable
#' @keywords internal
.build_reactable_columns <- function(x) {
  settings <- x$settings
  
  cols <- list()
  
  # First column (row_label) - twice the width of other columns, left-aligned
  cols$row_label <- reactable::colDef(
    name = "Row Label",
    minWidth = 200,
    align = "left",
    sticky = if (settings$freeze_first_column) "left" else NULL,
    style = list(fontWeight = "500"),
    headerStyle = list(fontWeight = "bold")
  )
  
  # Data columns - standard width, right-aligned
  data_cols <- setdiff(names(x$data), "row_label")
  
  for (col_name in data_cols) {
    cols[[col_name]] <- reactable::colDef(
      name = col_name,
      minWidth = 100,
      align = "right",
      cell = .build_cell_render_fn(x, col_name),
      style = .build_cell_style_fn(x, col_name),
      headerStyle = list(fontWeight = "bold"),
      sortable = settings$enable_sorting,
      html = TRUE  # Allow HTML rendering for tooltips
    )
  }
  
  cols
}

#' Build cell rendering function
#' @keywords internal
.build_cell_render_fn <- function(x, col_name) {
  metadata <- x$metadata
  settings <- x$settings
  special <- x$special_elements
  stat_info <- metadata$stat_info
  is_base_col <- col_name %in% special$base_cols
  is_summary_col <- col_name %in% special$summary_cols
  
  function(value, index) {
    df <- x$data
    row_label <- df$row_label[index]
    is_base_row <- row_label %in% special$base_rows
    is_summary_row <- row_label %in% special$summary_rows
    
    # Handle NA
    if (is.na(value)) {
      return("")
    }
    
    # Format base values
    if (is_base_col || is_base_row) {
      return(.format_base_value(value))
    }
    
    # Format regular values with controlled decimal places
    formatted <- .format_value_with_decimals(value, settings$decimal_places, stat_info$stat$format_fn)
    
    # Add significance symbol if enabled
    if (settings$sig_symbol && !is.null(metadata$sig_comparison)) {
      sig_result <- .get_significance_for_cell(
        metadata$significance,
        metadata$sig_comparison,
        row_label,
        col_name
      )
      
      if (!is.null(sig_result) && sig_result$significant) {
        symbol <- .format_sig_symbol(sig_result)
        formatted <- paste0(formatted, symbol)
      }
    }
    
    # Add tooltip for data cells (not base or summary) if enabled
    if (settings$show_tooltips && !is_base_row && !is_base_col && !is_summary_row && !is_summary_col) {
      # Get cell specification for this position
      cell_info <- .get_cell_spec_for_position(x, index, col_name)
      
      if (!is.null(cell_info)) {
        # Build tooltip text
        tooltip_text <- .build_tooltip_text(
          cell_info$specification,
          stat_info$id,
          cell_info$base
        )
        
        # Wrap with tooltip
        formatted <- .wrap_with_tooltip(formatted, tooltip_text)
      }
    }
    
    formatted
  }
}

#' Build cell style function
#' @keywords internal
.build_cell_style_fn <- function(x, col_name) {
  metadata <- x$metadata
  settings <- x$settings
  special <- x$special_elements
  is_base_col <- col_name %in% special$base_cols
  is_summary_col <- col_name %in% special$summary_cols
  
  function(value, index) {
    df <- x$data
    row_label <- df$row_label[index]
    is_base_row <- row_label %in% special$base_rows
    is_summary_row <- row_label %in% special$summary_rows
    
    style <- list()
    
    # Base column/row styling - faint grey background, no color coding
    if (is_base_col || is_base_row) {
      style$background <- "#F8F8F8"
      style$fontStyle <- "italic"
      return(style)
    }
    
    # Summary row/column styling - faint grey background, no color coding
    if (is_summary_row || is_summary_col) {
      style$background <- "#F8F8F8"
      style$fontWeight <- "bold"
      return(style)  # Return early to exclude from all color coding
    }
    
    # Handle NA
    if (is.na(value)) {
      return(style)
    }
    
    # Apply color mode
    if (settings$color_mode == "heatmap") {
      color <- .calculate_heatmap_color(x, col_name, value, index)
      if (!is.null(color)) {
        style$background <- color
      }
    } else if (settings$color_mode == "top_n") {
      if (.is_top_n_value(x, col_name, value, index)) {
        palette <- settings$color_palette %||% .get_default_palette(diverging = FALSE)
        style$background <- palette[length(palette)]
      }
    } else if (settings$color_mode == "significance") {
      sig_result <- .get_significance_for_cell(
        metadata$significance,
        metadata$sig_comparison,
        row_label,
        col_name
      )
      
      if (!is.null(sig_result) && sig_result$significant) {
        style$background <- .get_significance_color(sig_result)
      }
    }
    
    style
  }
}

#' Build row style function
#' @keywords internal
.build_row_style_fn <- function(x) {
  special <- x$special_elements
  
  function(index) {
    df <- x$data
    row_label <- df$row_label[index]
    
    style <- list()
    
    # Summary rows get faint grey background and bold text
    if (row_label %in% special$summary_rows) {
      style$fontWeight <- "bold"
      style$background <- "#F8F8F8"
    }
    
    style
  }
}

#' Calculate heatmap color for a cell
#' @keywords internal
.calculate_heatmap_color <- function(x, col_name, value, index) {
  df <- x$data
  settings <- x$settings
  special <- x$special_elements
  
  # Don't color base or summary rows
  row_label <- df$row_label[index]
  if (row_label %in% c(special$base_rows, special$summary_rows)) {
    return(NULL)
  }
  
  # Don't color base or summary columns
  if (col_name %in% c(special$base_cols, special$summary_cols)) {
    return(NULL)
  }
  
  # Get values based on scope
  if (settings$color_scope == "column") {
    # All values in this column (excluding base/summary)
    values <- df[[col_name]]
    mask <- !(df$row_label %in% c(special$base_rows, special$summary_rows))
    values <- values[mask]
  } else if (settings$color_scope == "row") {
    # All values in this row (excluding row_label, base, and summary columns)
    data_cols <- setdiff(names(df), c("row_label", special$base_cols, special$summary_cols))
    values <- as.numeric(df[index, data_cols])
  } else if (settings$color_scope == "table") {
    # All values in table (excluding base/summary rows and base/summary columns)
    data_cols <- setdiff(names(df), c("row_label", special$base_cols, special$summary_cols))
    mask <- !(df$row_label %in% c(special$base_rows, special$summary_rows))
    values <- unlist(df[mask, data_cols])
  }
  
  # Remove NA values
  values <- values[!is.na(values)]
  
  if (length(values) == 0) {
    return(NULL)
  }
  
  # Get palette
  palette <- settings$color_palette %||% .get_default_palette(diverging = TRUE)
  
  # Calculate min, max, midpoint
  min_val <- min(values, na.rm = TRUE)
  max_val <- max(values, na.rm = TRUE)
  
  if (min_val == max_val) {
    return(NULL)  # No variation
  }
  
  mid_val <- if (is.character(settings$color_midpoint)) {
    if (settings$color_midpoint == "mean") {
      mean(values, na.rm = TRUE)
    } else if (settings$color_midpoint == "median") {
      median(values, na.rm = TRUE)
    } else {
      (min_val + max_val) / 2
    }
  } else {
    settings$color_midpoint
  }
  
  # Map value to color
  .interpolate_color(value, min_val, mid_val, max_val, palette)
}

#' Check if a value is in top N
#' @keywords internal
.is_top_n_value <- function(x, col_name, value, index) {
  df <- x$data
  settings <- x$settings
  special <- x$special_elements
  
  # Don't highlight base or summary rows
  row_label <- df$row_label[index]
  if (row_label %in% c(special$base_rows, special$summary_rows)) {
    return(FALSE)
  }
  
  # Don't highlight base or summary columns
  if (col_name %in% c(special$base_cols, special$summary_cols)) {
    return(FALSE)
  }
  
  n <- settings$color_top_n
  
  # Get values based on scope
  if (settings$color_scope == "column") {
    values <- df[[col_name]]
    mask <- !(df$row_label %in% c(special$base_rows, special$summary_rows))
    values <- values[mask]
  } else if (settings$color_scope == "row") {
    data_cols <- setdiff(names(df), c("row_label", special$base_cols, special$summary_cols))
    values <- as.numeric(df[index, data_cols])
  } else if (settings$color_scope == "table") {
    data_cols <- setdiff(names(df), c("row_label", special$base_cols, special$summary_cols))
    mask <- !(df$row_label %in% c(special$base_rows, special$summary_rows))
    values <- unlist(df[mask, data_cols])
  }
  
  # Remove NA
  values <- values[!is.na(values)]
  
  if (length(values) == 0) {
    return(FALSE)
  }
  
  # Get top n threshold (handle ties by using >= )
  sorted_vals <- sort(values, decreasing = TRUE)
  if (n >= length(sorted_vals)) {
    threshold <- min(sorted_vals)
  } else {
    threshold <- sorted_vals[n]
  }
  
  value >= threshold
}

#' Get significance result for a cell
#' @keywords internal
.get_significance_for_cell <- function(significance, sig_comparison, row_label, col_name) {
  if (is.null(significance) || is.null(sig_comparison)) {
    return(NULL)
  }
  
  sig_data <- significance[[sig_comparison]]
  if (is.null(sig_data)) {
    return(NULL)
  }
  
  # sig_data has $levels and $p_values matrices
  levels_matrix <- sig_data$levels
  p_values_matrix <- sig_data$p_values
  
  if (is.null(levels_matrix)) {
    return(NULL)
  }
  
  # Find row and column
  if (!row_label %in% rownames(levels_matrix)) {
    return(NULL)
  }
  if (!col_name %in% colnames(levels_matrix)) {
    return(NULL)
  }
  
  sig_level <- levels_matrix[row_label, col_name]
  p_value <- if (!is.null(p_values_matrix)) {
    p_values_matrix[row_label, col_name]
  } else {
    NA
  }
  
  # Interpret significance level
  # Possible values: "base", "same", "higher", "lower", "omnibus", ""
  if (is.na(sig_level) || sig_level == "" || sig_level == "same" || sig_level == "base") {
    return(list(significant = FALSE, direction = "none", level = NULL, p_value = p_value))
  }
  
  if (sig_level == "higher") {
    direction <- "above"
  } else if (sig_level == "lower") {
    direction <- "below"
  } else {
    # Other cases (omnibus, etc.)
    return(NULL)
  }
  
  # Determine significance level from p-value
  # Use 0.10 and 0.05 as thresholds
  level <- if (!is.na(p_value)) {
    if (p_value < 0.05) 0.05 else 0.10
  } else {
    0.05  # Default to 95% if p-value not available
  }
  
  list(
    significant = TRUE,
    direction = direction,
    level = level,
    p_value = p_value
  )
}

#' Format significance symbol
#' @keywords internal
.format_sig_symbol <- function(sig_result) {
  if (!sig_result$significant) {
    return("")
  }
  
  arrow <- if (sig_result$direction == "above") "\u2191" else "\u2193"  # ↑ or ↓
  stars <- if (sig_result$level <= 0.05) "**" else "*"
  
  paste0(arrow, stars)
}

#' Get significance color
#' @keywords internal
.get_significance_color <- function(sig_result) {
  if (!sig_result$significant) {
    return(NULL)
  }
  
  colors <- .get_significance_colors()
  
  if (sig_result$direction == "above") {
    if (sig_result$level <= 0.05) {
      colors$above_95
    } else {
      colors$above_90
    }
  } else {
    if (sig_result$level <= 0.05) {
      colors$below_95
    } else {
      colors$below_90
    }
  }
}

#' Get default significance colors
#' @keywords internal
.get_significance_colors <- function() {
  list(
    above_90 = "#E1F5E1",  # Very mild green (90% confidence)
    above_95 = "#B3E5B3",  # Milder green (95% confidence)
    below_90 = "#FFE5E5",  # Very mild red (90% confidence)
    below_95 = "#FFCCCC"   # Milder red (95% confidence)
  )
}

#' Format base value
#' @keywords internal
.format_base_value <- function(value) {
  if (is.na(value)) {
    return("")
  }
  as.character(round(value, 0))
}

#' Format value with specified decimal places
#' @keywords internal
.format_value_with_decimals <- function(value, decimal_places, format_fn) {
  if (is.na(value)) {
    return("")
  }
  
  # Round to specified decimal places
  rounded <- round(value, decimal_places)
  
  # Format as percentage if format_fn adds %
  test_output <- format_fn(50)
  if (grepl("%", test_output, fixed = TRUE)) {
    paste0(format(rounded, nsmall = decimal_places, trim = TRUE), "%")
  } else {
    format(rounded, nsmall = decimal_places, trim = TRUE)
  }
}

#' Get default color palette
#' @keywords internal
.get_default_palette <- function(diverging = FALSE) {
  if (diverging) {
    # Milder blue-white-red for heatmaps (softer, more pastel)
    c("#A8C8E4", "#D9E8F5", "#FFFFFF", "#FAD9D5", "#E8A29D")
  } else {
    # Very mild orange for top-n highlighting
    c("#FFFFFF", "#FFF9F0", "#FFF3E0", "#FFE8CC", "#FFE0B3", "#FFD699")
  }
}

#' Interpolate color based on value
#' @keywords internal
.interpolate_color <- function(value, min_val, mid_val, max_val, palette) {
  if (is.na(value)) {
    return(NULL)
  }
  
  # Ensure palette has at least 3 colors for diverging
  if (length(palette) < 3) {
    palette <- .get_default_palette(diverging = TRUE)
  }
  
  # Determine position in palette
  if (value <= mid_val) {
    # Map from min to mid (first half of palette)
    half_palette <- palette[1:(ceiling(length(palette)/2))]
    prop <- if (mid_val == min_val) {
      0.5
    } else {
      (value - min_val) / (mid_val - min_val)
    }
    prop <- max(0, min(1, prop))
    idx <- 1 + prop * (length(half_palette) - 1)
  } else {
    # Map from mid to max (second half of palette)
    half_palette <- palette[(ceiling(length(palette)/2)):length(palette)]
    prop <- if (max_val == mid_val) {
      0.5
    } else {
      (value - mid_val) / (max_val - mid_val)
    }
    prop <- max(0, min(1, prop))
    idx <- 1 + prop * (length(half_palette) - 1)
  }
  
  # Linear interpolation between two nearest colors
  idx_low <- floor(idx)
  idx_high <- ceiling(idx)
  
  if (idx_low == idx_high) {
    return(half_palette[idx_low])
  }
  
  prop <- idx - idx_low
  .blend_colors(half_palette[idx_low], half_palette[idx_high], prop)
}

#' Blend two hex colors
#' @keywords internal
.blend_colors <- function(col1, col2, prop) {
  # Convert hex to RGB
  rgb1 <- col2rgb(col1)[, 1]
  rgb2 <- col2rgb(col2)[, 1]
  
  # Blend
  rgb_blend <- rgb1 * (1 - prop) + rgb2 * prop
  
  # Convert back to hex
  rgb(rgb_blend[1], rgb_blend[2], rgb_blend[3], maxColorValue = 255)
}

#' Get cell specification for a given row/column position
#' 
#' Maps row index and column name to the underlying cell specification
#' by looking up the cell in the cell_store via the layout grid.
#' 
#' @param x A reactable_tab object
#' @param row_index Integer row index in the data frame
#' @param col_name Character column name
#' @return List with cell specification and base n, or NULL if unavailable
#' @keywords internal
.get_cell_spec_for_position <- function(x, row_index, col_name) {
  # Check if we have cell metadata available
  if (is.null(x$metadata$cell_store) || is.null(x$metadata$layout)) {
    return(NULL)
  }
  
  cell_store <- x$metadata$cell_store
  layout <- x$metadata$layout
  grid <- layout$grid
  
  if (is.null(grid)) {
    return(NULL)
  }
  
  # Map column name to column index
  # Column names in data frame exclude row_label, but grid includes all columns
  data_col_names <- setdiff(names(x$data), "row_label")
  col_index <- match(col_name, data_col_names)
  
  if (is.na(col_index)) {
    return(NULL)
  }
  
  # Check bounds
  if (row_index < 1 || row_index > nrow(grid) || 
      col_index < 1 || col_index > ncol(grid)) {
    return(NULL)
  }
  
  # Get cell ID from grid
  cell_id <- grid[row_index, col_index]
  
  if (is.na(cell_id)) {
    return(NULL)
  }
  
  # Retrieve cell from store
  cell <- get_cell(cell_store, cell_id)
  
  if (is.null(cell)) {
    return(NULL)
  }
  
  list(
    specification = cell$specification,
    base = cell$base
  )
}

#' Build tooltip text for a cell
#' 
#' Constructs a tooltip string describing what the cell represents.
#' Tailors the message based on the statistic type.
#' 
#' @param specification Cell specification list
#' @param statistic_name Character name of the statistic
#' @param base_n Numeric base count
#' @return Character tooltip text (with HTML)
#' @keywords internal
.build_tooltip_text <- function(specification, statistic_name, base_n) {
  if (is.null(specification)) {
    return("")
  }
  
  # Extract expressions
  row_expr <- specification$row_expr
  col_expr <- specification$col_expr
  base_expr <- specification$base_expr
  
  # Format row expression
  row_text <- if (!is.null(row_expr)) {
    deparse(row_expr, width.cutoff = 500L)
  } else {
    "all"
  }
  row_text <- paste(row_text, collapse = " ")
  
  # Format column expression
  col_text <- if (!is.null(col_expr)) {
    deparse(col_expr, width.cutoff = 500L)
  } else {
    "all"
  }
  col_text <- paste(col_text, collapse = " ")
  
  # Format filter expression
  # Only include if not TRUE or NULL (i.e., there's an actual filter)
  filter_text <- ""
  if (!is.null(base_expr)) {
    # Check if it's just TRUE
    is_true_filter <- FALSE
    if (is.symbol(base_expr) && as.character(base_expr) == "TRUE") {
      is_true_filter <- TRUE
    } else if (is.logical(base_expr) && length(base_expr) == 1 && base_expr == TRUE) {
      is_true_filter <- TRUE
    }
    
    if (!is_true_filter) {
      filter_part <- deparse(base_expr, width.cutoff = 500L)
      filter_text <- paste(filter_part, collapse = " ")
    }
  }
  
  # Format base n
  base_text <- if (!is.na(base_n)) {
    as.character(round(base_n))
  } else {
    "NA"
  }
  
  # Tailor message based on statistic type
  parts <- .format_tooltip_for_statistic(
    statistic_name, 
    row_text, 
    col_text, 
    filter_text, 
    base_text
  )
  
  paste(parts, collapse = "\n")
}

#' Format tooltip text based on statistic type
#' 
#' @param stat_id Character statistic identifier
#' @param row_text Character formatted row expression
#' @param col_text Character formatted column expression
#' @param filter_text Character formatted filter expression (may be empty)
#' @param base_text Character formatted base count
#' @return Character vector of tooltip lines
#' @keywords internal
.format_tooltip_for_statistic <- function(stat_id, row_text, col_text, filter_text, base_text) {
  
  # Determine statistic category and create appropriate message
  parts <- character()
  
  if (stat_id == "count") {
    # Count statistic
    parts <- c(
      paste("Statistic:", stat_id),
      paste("Count of respondents with:", row_text),
      paste("And:", col_text)
    )
    
  } else if (stat_id == "correlation") {
    # Correlation statistic
    parts <- c(
      paste("Statistic:", stat_id),
      paste("Correlation between:", row_text),
      paste("And:", col_text)
    )
    
  } else if (stat_id %in% c("mean", "median", "sd", "cv", "p25", "p75")) {
    # Value-based statistics
    stat_label <- switch(stat_id,
                         mean = "Mean",
                         median = "Median",
                         sd = "Standard deviation",
                         cv = "Coefficient of variation",
                         p25 = "25th percentile",
                         p75 = "75th percentile",
                         stat_id)
    
    parts <- c(
      paste("Statistic:", stat_id),
      paste(stat_label, "of values for respondents with:", sep = " "),
      paste("  ", row_text),
      paste("And:", col_text)
    )
    
  } else {
    # Default case for percentage and other statistics (column_pct, row_pct, index, etc.)
    parts <- c(
      paste("Statistic:", stat_id),
      paste("Showing:", row_text),
      paste("Amongst:", col_text)
    )
  }
  
  # Add filter if present
  if (nzchar(filter_text)) {
    parts <- c(parts, paste("Filtered by:", filter_text))
  }
  
  # Add base n
  parts <- c(parts, paste("Base n:", base_text))
  
  parts
}

#' Wrap cell value with custom tooltip HTML
#' 
#' Creates HTML structure with tooltip as a plain string.
#' Uses actual HTML elements for rich formatting support.
#' 
#' @param value Character formatted value to display
#' @param tooltip_text Character tooltip text (can include HTML)
#' @return HTML string
#' @keywords internal
.wrap_with_tooltip <- function(value, tooltip_text) {
  # Check if htmltools is available
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    return(value)
  }
  
  span_element <- htmltools::span(
    class = "cell-with-tooltip",
    `data-tooltip` = tooltip_text,
    value
  )
  
  as.character(span_element)
}

#' Print method for reactable_tab objects
#'
#' Displays a summary of a reactable_tab object including dimensions,
#' color settings, and a preview of the data.
#'
#' @param x A reactable_tab object created by \code{\link{tab_to_reactable}}
#' @param n Integer. Number of data rows to display in preview (default 6)
#' @param ... Additional arguments passed to print methods
#' @return Invisibly returns the input object \code{x}
#' @method print reactable_tab
#' @export
#'
#' @examples
#' \dontrun{
#' # Create and print a reactable_tab object
#' r_tab <- mtcars %>% tab(gear, cyl) %>% tab_to_reactable()
#' print(r_tab)  # or just: r_tab
#' }
print.reactable_tab <- function(x, n = 6, ...) {
  cat("<reactable_tab>\n")
  
  # Dimensions
  n_rows <- nrow(x$data)
  n_cols <- ncol(x$data) - 1  # Exclude row_label
  cat(sprintf("  Dimensions: %d rows × %d columns\n", n_rows, n_cols))
  
  # Statistic type
  stat_id <- x$metadata$stat_info$id
  cat(sprintf("  Statistic: %s\n", stat_id))
  
  # Base display
  if (length(x$special_elements$base_rows) > 0 || 
      length(x$special_elements$base_cols) > 0) {
    cat("  Base (n): shown\n")
  } else {
    cat("  Base (n): hidden\n")
  }
  
  # Color mode
  cat(sprintf("  Color mode: %s", x$settings$color_mode))
  if (x$settings$color_mode != "none") {
    cat(sprintf(" (scope: %s)", x$settings$color_scope))
  }
  cat("\n")
  
  # Significance
  if (!is.null(x$metadata$sig_comparison)) {
    cat(sprintf("  Significance: %s", x$metadata$sig_comparison))
    if (x$settings$sig_symbol) {
      cat(" (with symbols)")
    }
    cat("\n")
  }
  
  # Special elements
  if (length(x$special_elements$summary_rows) > 0 || 
      length(x$special_elements$summary_cols) > 0) {
    cat("  Summary rows/cols: included\n")
  }
  
  # Display settings
  features <- character()
  if (x$settings$freeze_first_column) features <- c(features, "frozen first column")
  if (x$settings$enable_sorting) features <- c(features, "sortable")
  if (is.numeric(x$settings$pagination) && x$settings$pagination > 0) {
    features <- c(features, sprintf("paginated (%d rows/page)", x$settings$pagination))
  }
  if (length(features) > 0) {
    cat(sprintf("  Features: %s\n", paste(features, collapse = ", ")))
  }
  
  cat("\nData preview (first", min(n, n_rows), "rows):\n")
  print(utils::head(x$data, n))
  
  cat("\nUse display_reactable(x) to view the interactive table.\n")
  
  invisible(x)
}
