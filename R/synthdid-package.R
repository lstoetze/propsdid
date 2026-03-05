#' @description
#' The `propsdid` package provides methods for estimating treatment effects on 
#' *proportional outcomes* using extensions of the Synthetic Control (SC) and 
#' Synthetic Difference‑in‑Differences (SDID) estimators.  
#'
#' The package implements Synthetic Control  and Synthetic 
#' Difference‑in‑Differences ewith common-weights stimators introduced in:
#'
#' Bogatyrev, K., & Stoetzer, L.  
#' *Estimating treatment effects on proportions with synthetic controls.*  
#' OSF Preprints. https://osf.io/preprints/osf/brhd3
#'
#' It extends the original `synthdid` package that estimates treatment effects for 
#' *single‑outcome panels*. This package includes an extension to settings 
#' where **multiple proportional outcomes**.
#'
#' The method estimates a common set of unit and time weights across all 
#' outcomes, ensuring that the reconstructed counterfactual respects the 
#' compositional structure of the data, and treatment effects sum to zero.
#'
#' ## Key Features
#' * Synthetic Difference‑in‑Differences with **common weights**  
#' * Synthetic Control with **common weights**  
#'
#' ## Links
#' * GitHub repository: https://github.com/lstoetze/propsdid
#' * Paper preprint: https://osf.io/preprints/osf/brhd3
#'
#' @examples
#' \donttest{
#' Example: Estimating Electoral Effects of Transition Agreement in Spain 
#' data("spain")
#' # Create panel array
#' df_spain <- panel.array(spain,
#'                         unit = "province",
#'                         time = "election",
#'                         category = "party",
#'                         outcome = "votes",
#'                         treatment = "treatment"
#'                        )
#' # Synthethic Control with common weight 
#' est_sc_cw <- sc_estimate(df_spain$Y, df_spain$N0, df_spain$T0,
#'                          porp_dat = TRUE, method = "sc")
#' sum_est <- summary(est_sc_cw,fast = T) # fast = TRUE, Jackknife SE 
#' est_sc_cw
#' # Synthethic Difference-in-Difference with common weight 
#' est_sdid_cw <- sc_estimate(df_spain$Y, df_spain$N0, df_spain$T0, 
#'                            porp_dat = TRUE, method = "sdid")
#' sum_est <- summary(est_sdid_cw,fast = T) # fast = TRUE, Jackknife SE 
#' est_sdid_cw
#'
#' @keywords internal
"_PACKAGE"
