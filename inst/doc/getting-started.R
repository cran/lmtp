## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  results = "hold"
)
require(nnls)

## -----------------------------------------------------------------------------
library(lmtp)

## -----------------------------------------------------------------------------
baseline <- c("W_1", "W_2")
trt <- c("A_1", "A_2")
time_vary <- list(c("L_11", "L_12"), 
                  c("L_21", "L_22"))
create_node_list(trt = trt, baseline = baseline, time_vary = time_vary, tau = 2)

## -----------------------------------------------------------------------------
shift <- function(data, trt) {
  (data[[trt]] - 1) * (data[[trt]] - 1 >= 1) + data[[trt]] * (data[[trt]] - 1 < 1)
}

## -----------------------------------------------------------------------------
A <- c("A_1", "A_2", "A_3", "A_4")
L <- list(c("L_1"), c("L_2"), c("L_3"), c("L_4"))
lmtp_sdr(sim_t4, A, "Y", time_vary = L, k = 0, shift = shift, folds = 5)

## ----warning = FALSE----------------------------------------------------------
if (require("twang", quietly = TRUE, attach.required = FALSE)) {
  data("iptwExWide", package = "twang")
  A <- paste0("tx", 1:3)
  W <- c("gender", "age")
  L <- list(c("use0"), c("use1"), c("use2"))
  lmtp_tmle(iptwExWide, A, "outcome", W, L, 
            shift = static_binary_on, outcome_type = "continuous",
            folds = 2, .SL_folds = 2)
}

## ----warning = FALSE----------------------------------------------------------
A <- c("A_1", "A_2", "A_3", "A_4")
L <- list(c("L_1"), c("L_2"), c("L_3"), c("L_4"))
shift <- function(data, trt) {
  (data[[trt]] - 1) * (data[[trt]] - 1 >= 1) + data[[trt]] * (data[[trt]] - 1 < 1)
}

# creating a dynamic mtp that applies the shift function 
# but also depends on history and the current time
dynamic_mtp <- function(data, trt) {
  if (trt == "A_1") {
    # if its the first time point, follow the same mtp as before
    shift(data, trt)
  } else {
    # otherwise check if the time varying covariate equals 1
    ifelse(data[[sub("A", "L", trt)]] == 1, 
           shift(data, trt), # if yes continue with the policy
           data[[trt]])      # otherwise do nothing
  }
}

lmtp_tmle(sim_t4, A, "Y", time_vary = L, k = 0, 
          shift = dynamic_mtp, folds = 2, .SL_folds = 2)

## ----warning = FALSE----------------------------------------------------------
if (require("ranger", quietly = TRUE, attach.required = FALSE) && 
    require("twang", quietly = TRUE, attach.required = FALSE)) {
  data("iptwExWide", package = "twang")
  A <- paste0("tx", 1:3)
  W <- c("gender", "age")
  L <- list(c("use0"), c("use1"), c("use2"))
  lrnrs <- c("SL.glm", "SL.ranger", "SL.glm.interaction")
  lmtp_tmle(iptwExWide, A, "outcome", W, L, shift = static_binary_on, 
            outcome_type = "continuous", learners_trt = lrnrs, 
            learners_outcome = lrnrs, folds = 2, .SL_folds = 2)
}

## -----------------------------------------------------------------------------
head(sim_cens[sim_cens$C1 == 0, ])

## -----------------------------------------------------------------------------
A <- c("A1", "A2")
L <- list(c("L1"), c("L2"))
C <- c("C1", "C2")

lmtp_tmle(sim_cens, A, "Y", time_vary = L, cens = C,
          shift = function(data, trt) data[[trt]] + 0.5, 
          folds = 2, .SL_folds = 2)

## -----------------------------------------------------------------------------
A <- c("A1", "A2")
L <- list(c("L1"), c("L2"))
C <- c("C1", "C2")

lmtp_sdr(sim_cens, A, "Y", time_vary = L, cens = C, 
         shift = NULL, folds = 2, .SL_folds = 2)

## ----warning = FALSE----------------------------------------------------------
A <- "trt"
Y <- paste0("Y.", 1:6)
C <- paste0("C.", 0:5)
W <- c("W1", "W2")

lmtp_tmle(sim_point_surv, A, Y, W, cens = C, shift = static_binary_on, 
          outcome_type = "survival", folds = 2, .SL_folds = 2)

## ----warning = FALSE----------------------------------------------------------
W <- "L0.c"
L <- list(c("L0.a", "L0.b"), c("L1.a", "L1.b"))
A <- c("A0", "A1")
C <- c("C0", "C1")
Y <- c("Y1", "Y2")

lmtp_sdr(sim_timevary_surv, A, Y, W, L, C, outcome_type = "survival", 
         shift = static_binary_on, folds = 2, .SL_folds = 2)

## -----------------------------------------------------------------------------
A <- c("A1", "A2")
L <- list(c("L1"), c("L2"))
C <- c("C1", "C2")

fit_shift <- 
  lmtp_sdr(sim_cens, A, "Y", time_vary = L, cens = C, 
           shift = function(data, trt) data[[trt]] + 0.5, 
           folds = 2, .SL_folds = 2)

fit_noshift <- 
  lmtp_sdr(sim_cens, A, "Y", time_vary = L, cens = C,
           shift = NULL, folds = 2, .SL_folds = 2)

## -----------------------------------------------------------------------------
lmtp_contrast(fit_shift, ref = fit_noshift, type = "additive")

## -----------------------------------------------------------------------------
lmtp_contrast(fit_shift, ref = fit_noshift, type = "rr")

## -----------------------------------------------------------------------------
tidy(fit_shift)

