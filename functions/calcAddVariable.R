#' Calculate new variable
#'
#' Calculate new variable from existing ones, using a generic formula.
#'
#' \strong{Obsolete}. This function will be removed in the near future. Use
#' \code{\link{calc_addVariable}} instead.
#'
#' @seealso \code{\link{calc_addVariable}}
#'
#' @param data A data frame with columns \code{"variable"}, \code{"unit"} and
#'             \code{"value"}.
#' @param formula An object of class formula, as returned by
#'                \code{\link[stats]{formula}}.
#' @param newUnit Character vector with the unit for the newly calculated
#'                variable.
#' @param na.act Indicates how NAs in the wide data frame should be handled.
#'               Default "no" indicates no action is takenl.
#'
#' @return A data frame with the original and the new variables.
#'
#' @examples
#' \dontshow{
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
#' calcAddVariable(data, formula("GDPpC" ~ "`GDP|MER` / Population"),
#'                 newUnit = "US$2005/cap")
#' }
#'
#' @author Anselm Schultes, Michaja Pehl
#'
#' @export
calcAddVariable <- function(data, formula, newUnit = "None", na.act = "no") {
  
  formula      <- stats::as.formula(formula)
  .dots        <- list(c(as.character(formula[3]), newUnit))
  names(.dots) <- as.character(formula[2])
  
  if ("no" == na.act) {
    na.rm <- FALSE
  } else if ("exclude" == na.act) {
    na.rm <- TRUE
  }
  
  calc_addVariable_(data, .dots, na.rm)
}