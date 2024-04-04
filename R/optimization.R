#' Perform Optimization
#'
#' This function conducts optimization based on specified constraints and model parameters.
#'
#' @param constr_type Type of constraint: "budget" or "goal".
#' @param constr_value Numeric value for the constraint.
#' @param alpha Numeric vector representing model coefficients.
#' @param beta Numeric vector representing model powers.
#' @param initial_spend Initial spending values for optimization.
#' @param lower_bound_spend Lower bound for spending optimization.
#' @param upper_bound_spend Upper bound for spending optimization.
#'
#' @return Numeric vector representing the optimized solution.
#'
#' @details
#' The function employs the NLOPT_LD_SLSQP algorithm for optimization.
#' For "goal" constraints, the optimization maximizes the sum of spending.
#' For "budget" constraints, the optimization minimizes the negative sum of spending.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' constr_type <- "Goal"
#' constr_value <- 5
#' alpha <- c(1, 2)
#' beta <- c(0.3, 0.4)
#' initial_spend <- c(2, 2)
#' lower_bound_spend <- c(-1000, -10000)
#' upper_bound_spend <- c(10000, 10000)
#' perform_optimization(constr_type, constr_value, alpha, beta, initial_spend, lower_bound_spend,
#'  upper_bound_spend)
#' }
#'
#' @importFrom nloptr nloptr
#' @importFrom utils capture.output
#' @importFrom stringr str_extract str_replace
#' @importFrom purrr map
#'
#'
#' @export
perform_optimization <-
  function(constr_type,
           constr_value,
           alpha,
           beta,
           initial_spend,
           lower_bound_spend,
           upper_bound_spend) {
    if (grepl("Goal", constr_type, ignore.case = TRUE)) {
      eval_f0 <- function(spend, alpha, beta, constr_value) {
        k <- spend
        return(sum(k))
      }
      eval_grad_f0 <- function(spend, alpha, beta, constr_value) {
        return(rep(1, length(spend)))
      }
      eval_g0 <- function(spend, alpha, beta, constr_value) {
        return(constr_value - sum(alpha * spend ^ beta))
      }
      eval_jac_g0 <- function(spend, alpha, beta, constr_value) {
        return(-alpha * beta * spend ^ (beta - 1))
      }
    } else {
      eval_f0 <- function(spend, alpha, beta, constr_value) {
        return(-sum(alpha * spend ^ beta))
      }
      eval_grad_f0 <- function(spend, alpha, beta, constr_value) {
        return(c(-alpha * beta * spend ^ (beta - 1)))
      }
      eval_g0 <- function(spend, alpha, beta, constr_value) {
        return(sum(spend) - constr_value)
      }
      eval_jac_g0 <- function(spend, alpha, beta, constr_value) {
        return(rep(1, length(spend)))
      }
    }

    opts <- list(
      "algorithm" = "NLOPT_LD_SLSQP",
      #NLOPT_LN_COBYLA,NLOPT_LD_SLSQP,NLOPT_LD_MMA
      "xtol_rel" = 1e-10,
      "print_level" = 3,
      "maxeval" = 1e5
      # ,
      # "check_derivatives" = TRUE,
      # "check_derivatives_print" = "all"
    )


    optim_steps <- capture.output({
      opti <- nloptr(
        x0 = initial_spend,
        eval_f = eval_f0,
        eval_grad_f = eval_grad_f0,
        eval_g_ineq = eval_g0,
        eval_jac_g_ineq = eval_jac_g0,
        lb = lower_bound_spend,
        ub = upper_bound_spend,
        opts = opts,
        alpha = alpha,
        beta = beta,
        constr_value = constr_value
      )
    })

    spend_steps <- optim_steps %>%
      stringr::str_extract("^\\tx = (.*)") %>%
      stringr::str_replace("^\\tx =", "c") %>%
      purrr::map(~ eval(parse(text = .x)))
    spend_steps <-
      spend_steps[sapply(spend_steps, function(x)
        any(!is.na(x)))]

    return(list(steps = spend_steps, solution = opti$solution))
  }
