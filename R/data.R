#' California proposition 99
#'
#' A dataset containing per-capita cigarette consumption (in packs).
#' In year 1989 California imposed a Tobacco tax. The column `treated` is 1 from then on for California.
#'
#' @docType data
#' @name california_prop99
#'
#' @format A data frame with 1209 rows and 4 variables:
#' \describe{
#'   \item{State}{US state name, character string}
#'   \item{Year}{Year, integer}
#'   \item{PacksPerCapita}{per-capita cigarette consumption, numeric}
#'   \item{treated}{the treatmed indicator 0: control, 1: treated, numeric}
#' }
#' @source Abadie, Alberto, Alexis Diamond, and Jens Hainmueller.
#'  "Synthetic control methods for comparative case studies: Estimating the effect of California’s tobacco control program."
#'   Journal of the American statistical Association 105, no. 490 (2010): 493-505.
#'
#' @usage data(california_prop99)
#'
#' @examples
#' \donttest{
#' # Load tobacco sales in long panel format.
#' data("california_prop99")
#' # Transform to N*T matrix format required for synthdid,
#' # where N is the number of units and T the time periods.
#' setup <- panel.matrices(california_prop99)
#' }
#'
NULL


#' Spanish Elections and Just Transition Agreement Data
#'
#' This dataset contains provincial-level electoral results from Spain,
#' including treatment indicators associated with the “Just Transition Agreement.”
#' It is based on Bolet, Green, and González-Eguino (2024) and extended to
#' include vote share information for all major political parties.
#'
#' Each row corresponds to a province–election–party combination, reporting
#' the vote share obtained by that party. The treatment variable indicates
#' whether a province was part of the Just Transition Agreement.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{province}{Province identifier (character)}
#'   \item{election}{Election year (integer)}
#'   \item{treatment}{Treatment indicator: 1 = treated, 0 = control}
#'   \item{party}{Political party name (character), e.g. PSOE, PP, Podemos, Cs, VOX, Others}
#'   \item{votes}{Vote share for that party (numeric)}
#' }
#'
#' @source Bolet, D., F. Green, and M. González-Eguino. 2024.
#' *How to Get Coal Country to Vote for Climate Policy: The Effect of a
#' “Just Transition Agreement” on Spanish Election Results.*
#' American Political Science Review 118(3): 1344–1359.
#' Extended dataset including complete vote shares.
#'
#' @usage data(spain)
#'
#' @examples
#' \donttest{
#' data("spain")
#' head(spain)
#' }
#'
"spain"




