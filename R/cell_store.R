# Cell Store - Core Storage for Tab Cell-Based Architecture
#
# Implements cell storage with hash table (O(1) access) and columnar storage
# (fast bulk operations). Cells have sequential IDs and are position-independent.

#' Create a new cell store
#'
#' Initializes an empty cell store with hash table and columnar storage.
#'
#' @param size_hint Expected number of cells (for pre-allocation)
#' @return A cell_store object
#' @keywords internal
new_cell_store <- function(size_hint = 1000) {
  # Create environment to hold mutable state
  store_env <- new.env(parent = emptyenv())
  
  # Store cells hash table
  store_env$cells <- new.env(hash = TRUE, size = size_hint, parent = emptyenv())
  
  # Store columnar data as environment (mutable!)
  store_env$cell_data <- new.env(parent = emptyenv())
  store_env$cell_data$cell_ids <- character(0)
  store_env$cell_data$values <- numeric(0)
  store_env$cell_data$bases <- numeric(0)
  store_env$cell_data$specifications <- list()
  store_env$cell_data$derivations <- list()
  
  # Store cell index
  store_env$cell_index <- new.env(hash = TRUE, size = size_hint, parent = emptyenv())
  
  # Counter
  store_env$next_id <- 1L
  
  class(store_env) <- "cell_store"
  return(store_env)
}

#' Generate next sequential cell ID
#'
#' @param store A cell_store object
#' @return Character cell ID (e.g., "c_000001")
#' @keywords internal
generate_cell_id <- function(store) {
  id <- sprintf("c_%06d", store$next_id)
  store$next_id <- store$next_id + 1L
  id
}

#' Add a cell to the store
#'
#' Adds a new cell with sequential ID to both hash table and columnar storage.
#'
#' @param store A cell_store object
#' @param value Numeric value of the cell
#' @param base Numeric base count for the cell
#' @param specification List with cell specification (expressions, metadata)
#' @param computation List with computation info (statistic, array_refs)
#' @param derivation NULL or list with derivation info for derived cells
#' @return The cell_id of the added cell (invisible)
#' @keywords internal
add_cell <- function(store, value, base, specification, computation, derivation = NULL) {
  # Generate unique cell ID
  cell_id <- generate_cell_id(store)
  pos <- length(store$cell_data$cell_ids) + 1
  
  # Create cell object
  cell <- list(
    cell_id = cell_id,
    value = value,
    base = base,
    specification = specification,
    computation = computation,
    derivation = derivation
  )
  
  # Add to hash table (for O(1) access)
  store$cells[[cell_id]] <- cell
  
  # Add to columnar storage (for bulk operations)
  # Must update vectors properly in R (not just assign to position)
  store$cell_data$cell_ids <- c(store$cell_data$cell_ids, cell_id)
  store$cell_data$values <- c(store$cell_data$values, value)
  store$cell_data$bases <- c(store$cell_data$bases, base)
  store$cell_data$specifications[[pos]] <- specification
  store$cell_data$derivations[[pos]] <- derivation
  
  # Update hash index
  store$cell_index[[cell_id]] <- pos
  
  invisible(cell_id)
}

#' Get a single cell by ID
#'
#' @param store A cell_store object
#' @param cell_id Character cell ID
#' @return Cell object or NULL if not found
#' @keywords internal
get_cell <- function(store, cell_id) {
  if (exists(cell_id, envir = store$cells, inherits = FALSE)) {
    store$cells[[cell_id]]
  } else {
    NULL
  }
}

#' Get multiple cells by IDs
#'
#' @param store A cell_store object
#' @param cell_ids Character vector of cell IDs
#' @return List of cell objects
#' @keywords internal
get_cells <- function(store, cell_ids) {
  lapply(cell_ids, function(id) get_cell(store, id))
}

#' Get all cell IDs in store
#'
#' @param store A cell_store object
#' @return Character vector of all cell IDs
#' @keywords internal
all_cell_ids <- function(store) {
  ls(store$cells, all.names = TRUE)
}

#' Get all values (columnar access)
#'
#' Fast access to all cell values using columnar storage.
#'
#' @param store A cell_store object
#' @return Numeric vector of all values
#' @keywords internal
get_all_values <- function(store) {
  store$cell_data$values
}

#' Get all bases (columnar access)
#'
#' @param store A cell_store object
#' @return Numeric vector of all bases
#' @keywords internal
get_all_bases <- function(store) {
  store$cell_data$bases
}

#' Filter cells by predicate
#'
#' Returns cell IDs matching a predicate function.
#' Uses columnar storage for efficiency.
#'
#' @param store A cell_store object
#' @param predicate Function taking cell object, returning TRUE/FALSE
#' @return Character vector of matching cell IDs
#' @keywords internal
filter_cells <- function(store, predicate) {
  # Iterate over positions (more efficient than hash table iteration)
  matching_positions <- integer(0)
  
  for (pos in seq_along(store$cell_data$cell_ids)) {
    # Reconstruct cell object from columnar storage
    # Use get_cell for safety since it handles the hash table properly
    cell_id <- store$cell_data$cell_ids[pos]
    cell <- get_cell(store, cell_id)
    
    if (!is.null(cell) && predicate(cell)) {
      matching_positions <- c(matching_positions, pos)
    }
  }
  
  store$cell_data$cell_ids[matching_positions]
}

#' Filter cells by value condition
#'
#' Optimized for simple value/base filtering using columnar access.
#'
#' @param store A cell_store object
#' @param value_condition Expression evaluated against values
#' @return Character vector of matching cell IDs
#' @keywords internal
filter_cells_by_value <- function(store, value_condition) {
  # Use columnar storage for fast filtering
  condition_expr <- substitute(value_condition)
  
  env <- list(value = store$cell_data$values, base = store$cell_data$bases)
  matching <- eval(condition_expr, env)
  
  store$cell_data$cell_ids[matching]
}

#' Get number of cells in store
#'
#' @param store A cell_store object
#' @return Integer count of cells
#' @keywords internal
cell_count <- function(store) {
  length(store$cell_data$cell_ids)
}

#' Check if cell ID exists in store
#'
#' @param store A cell_store object
#' @param cell_id Character cell ID
#' @return Logical
#' @keywords internal
has_cell <- function(store, cell_id) {
  exists(cell_id, envir = store$cells, inherits = FALSE)
}

#' Validate cell store integrity
#'
#' Checks that hash table and columnar storage are consistent.
#' Useful for debugging and testing.
#'
#' @param store A cell_store object
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_cell_store <- function(store) {
  # Check that hash table and columnar storage have same number of cells
  hash_count <- length(ls(store$cells, all.names = TRUE))
  columnar_count <- length(store$cell_data$cell_ids)
  
  if (hash_count != columnar_count) {
    stop("Cell store inconsistent: hash has ", hash_count, " cells, columnar has ", columnar_count)
  }
  
  # Check that all cell IDs in columnar exist in hash
  for (cell_id in store$cell_data$cell_ids) {
    if (!has_cell(store, cell_id)) {
      stop("Cell ", cell_id, " in columnar storage but not in hash table")
    }
  }
  
  # Check that cell_index matches positions
  for (i in seq_along(store$cell_data$cell_ids)) {
    cell_id <- store$cell_data$cell_ids[i]
    indexed_pos <- store$cell_index[[cell_id]]
    
    if (is.null(indexed_pos) || indexed_pos != i) {
      stop("Cell index incorrect for ", cell_id)
    }
  }
  
  TRUE
}

#' Print method for cell_store
#' @keywords internal
print.cell_store <- function(x, ...) {
  cat("Cell Store\n")
  cat("  Cells:", cell_count(x), "\n")
  cat("  Next ID:", x$next_id, "\n")
  cat("  Hash table size:", length(ls(x$cells)), "\n")
  invisible(x)
}