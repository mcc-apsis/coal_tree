#' Calculate new variables
#'
#' Calculate new variables from existing ones, using generic formulas.
#'
#' \code{...} is a list of name-value pairs with the general format
#' \preformatted{
#' "lhs" = "rhs + calculations - formula", "`lhs 2`" = "lhs / `rhs 2`"
#' }
#' where \code{lhs} are the names of new variables to be calculated and
#' \code{rhs} are the variables to calculate from. If \code{lhs} and \code{rhs}
#' are no proper \emph{identifiers}, they need to be quoted (see
#' \link[base]{Quotes} for details). When in doubt, just quote.
#'
#' If the new variables should have units, set \code{units} appropriately.
#'
#' \code{.dots} is a named list of strings denoting formulas and optionally
#' units. The general format is
#' \preformatted{
#' list("`lhs 1`" = "`rhs` / `calculation`",
#'      "`lhs 2`" = "sin(`rhs 2`)")
#' }
#'
#' Units are optionally included with the formulas in a vector like
#' \preformatted{
#' list("`lhs w/ unit`" = c("`rhs 1` + `rhs 2`", "rhs unit")
#' }
#' Units do not require quoting.
#'
#' \code{...} and \code{.dots} are processed in order, and variables already
#' calculated in the same call can be used for further calculations. Other
#' existing columns, including \code{period}, can be referenced, but this is
#' not supported and the results are considered \emph{undefined}.
#'
#' @param data A data frame.
#' @param ... Name-value pairs of calculation formulas. See details.
#' @param units Character vector of units corresponding to new variables.
#'              Must be of length equal to \code{...} or of length one (in
#'              which case all new variables receive the same unit).
#' @param na.rm If \code{TRUE} (the default), remove items calculated as
#'              \code{NA}.
#' @param only.new If \code{FALSE} (the default), add new variables to existing
#'                 ones. If \code{TRUE}, return only new variables.
#' @param variable Column name of variables. Defaults to \code{"variable"}.
#' @param unit Column name of units. Defaults to \code{"unit"}.
#' @param value Column name of values. Defaults to \code{"value"}.
#' @param .dots Used to work around non-standard evaluation. See details.
#'
#' @return A data frame.
#'
#' @examples
#' data <- inline.data.frame(c(
#'     "model;    scenario;   region;   variable;     unit;                 period;   value",
#'     "REMIND;   Baseline;   USA;      GDP|MER;      billion US$2005/yr;   2010;     12990",
#'     "REMIND;   Baseline;   USA;      Population;   million;              2010;       310.4",
#'     "REMIND;   Baseline;   USA;      PE;           EJ/yr;                2010;        91.62",
#'     "REMIND;   Baseline;   CHN;      GDP|MER;      billion US$2005/yr;   2020;      8882",
#'     "REMIND;   Baseline;   CHN;      GDP|MER;      billion US$2005/yr;   2010;      4119",
#'     "REMIND;   Baseline;   CHN;      Population;   million;              2020;      1387",
#'     "REMIND;   Baseline;   CHN;      Population;   million;              2010;      1349"))
#'
#' calc_addVariable(data, "GDPpC" = "`GDP|MER` / Population * 1e3",
#'                        "`ln GDPpC`" = "log(GDPpC)",
#'                        units = c("US$2005/cap", NA))
#' calc_addVariable_(
#'     data,
#'     list("`GDPpC`"    = c("`GDP|MER` / `Population` * 1e3", "US$/cap"),
#'          "`ln GDPpC`" = "log(`GDPpC`)")
#' )
#'
#' @author Michaja Pehl
#'
#' @export
calc_addVariable <- function(data, ..., units = NA, na.rm = TRUE,
                             only.new = FALSE, variable = variable, unit = NA,
                             value = value) {
  
  .dots    <- list(...)
  
  if (!all(is.na(units))) {
    if (length(units) == length(.dots)) {
      for (i in 1:length(.dots))
        .dots[i][[1]] <- c(.dots[i][[1]], units[i])
    } else if (1 == length(units)) {
      for (i in 1:length(.dots))
        .dots[i][[1]] <- c(.dots[i][[1]], units)
    } else
      stop("units must be of the same length as ... or of length one.")
  }
  
  variable <- deparse(substitute(variable))
  unit     <- ifelse(is.na(unit), NA, deparse(substitute(unit)))
  value    <- deparse(substitute(value))
  
  calc_addVariable_(data, .dots, na.rm, only.new, variable, unit, value)
}

#' @export
#' @rdname calc_addVariable
calc_addVariable_ <- function(data, .dots, na.rm = TRUE, only.new = FALSE,
                              variable = "variable", unit = NA,
                              value = "value") {
  
  # guardians
  if (!is.data.frame(data))
    stop("Only works with data frames")
  
  if (!is.list(.dots))
    stop("'.dots' must be a list of formula strings")
  
  .colnames <- colnames(data)
  
  if (!variable %in% .colnames)
    stop("No column '", variable, "' found'")
  
  if (!value %in% .colnames)
    stop("No column '", value, "' found'")
  
  if (is.na(unit)) {
    if ("unit" %in% .colnames)
      unit <- "unit"
  } else {
    if (!unit %in% .colnames)
      stop("No column '", unit, "' found.")
  }
  
  .units <- lapply(.dots, function(l) { l[2] }) %>%
    unlist()
  
  .dots <- lapply(.dots,
                  function(l) {
                    paste0("~", l[[1]]) %>%
                      stats::formula() %>%
                      lazyeval::interp()
                  })
  names(.dots) <- gsub("`", "", names(.dots))
  
  .dots.names <- lapply(.dots, all.vars) %>%
    unlist() %>%
    unique() %>%
    setdiff(names(.dots))
  
  # filter for variables used on rhs
  .data <- data %>%
    filter_(.dots = lazyeval::interp(~variable %in% .dots.names,
                                     variable = as.name(variable),
                                     .dots.names = .dots.names))
  
  
  # drop unit column, if necessary
  if (!is.na(unit)) {
    variables.units <- .data %>%
      select_(variable, unit) %>%
      distinct() %>%
      filter_(.dots = lazyeval::interp(~variable %in% .dots.names,
                                       variable = as.name(variable),
                                       .dots.names = .dots.names))
    
    .data <- .data %>%
      select_(.dots = lazyeval::interp(~-unit, unit = as.name(unit)))
  }
  
  # calculation
  .data <- .data %>%
    spread_(variable, value) %>%
    mutate_(.dots = .dots) %>%
    gather_(variable, value, unique(c(.dots.names, names(.dots))))
  
  # filter new variables
  if (only.new) {
    .data <- .data %>%
      filter_(.dots = lazyeval::interp(~variable %in% names(.dots),
                                       variable = as.name(variable)))
  }
  
  # filter NAs
  if (na.rm) {
    .data <- .data %>%
      filter_(.dots = lazyeval::interp(~!is.na(value),
                                       value = as.name(value)))
  }
  
  # restore unit column, if necessary
  if (!is.na(unit)) {
    
    .units <- data.frame(variable = gsub("`", "", names(.units)),
                         unit = as.character(.units))
    colnames(.units) <- c(variable, unit)
    
    .data <- inner_join(
      .data,
      rbind(variables.units, .units),
      by = variable
    )
  }
  
  
  # add unaffected variables
  if (!only.new) {
    .data <- rbind(
      data %>%
        filter_(.dots = lazyeval::interp(~!variable %in% .dots.names,
                                         variable = as.name(variable),
                                         .dots.names = .dots.names)),
      .data
    )
  }
  
  return(.data %>% select_(.dots = .colnames))
}
