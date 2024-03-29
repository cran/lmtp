cf_sub <- function(Task, outcome, learners, lrnr_folds, full_fits, pb) {
  out <- list()

  for (fold in seq_along(Task$folds)) {
    out[[fold]] <- future::future({
      estimate_sub(
        get_folded_data(Task$natural, Task$folds, fold),
        get_folded_data(Task$shifted[, Task$trt, drop = F], Task$folds, fold),
        outcome,
        Task$node_list$outcome, Task$cens,
        Task$risk, Task$tau, Task$outcome_type,
        learners, lrnr_folds, pb, full_fits
      )
    },
    seed = TRUE)
  }

  out <- future::value(out)

  list(
    m = recombine_outcome(out, "m", Task$folds),
    fits = lapply(out, function(x) x[["fits"]])
  )
}

estimate_sub <- function(natural, shifted, outcome, node_list, cens, risk,
                         tau, outcome_type, learners, lrnr_folds, pb, full_fits) {

  m <- matrix(nrow = nrow(natural$valid), ncol = tau)
  fits <- list()

  for (t in tau:1) {
    i  <- censored(natural$train, cens, t)$i
    jt <- censored(natural$train, cens, t)$j
    jv <- censored(natural$valid, cens, t)$j
    rt <- at_risk(natural$train, risk, t)
    rv <- at_risk(natural$valid, risk, t)

    pseudo <- paste0("tmp_lmtp_pseudo", t)
    vars <- node_list[[t]]

    if (t != tau) {
      outcome <- paste0("tmp_lmtp_pseudo", t + 1)
      outcome_type <- "continuous"
    }

    learners <- check_variation(natural$train[i & rt, ][[outcome]], learners)

    fit <- run_ensemble(
      natural$train[i & rt, ][[outcome]],
      natural$train[i & rt, vars],
      learners,
      outcome_type,
      id = natural$train[i & rt, ][["lmtp_id"]],
      lrnr_folds
    )

    if (full_fits) {
      fits[[t]] <- fit
    } else {
      fits[[t]] <- extract_sl_weights(fit)
    }

    trt_var <- names(shifted$train)[t]
    under_shift_train <- natural$train[jt & rt, vars]
    under_shift_train[[trt_var]] <- shifted$train[jt & rt, trt_var]

    under_shift_valid <- natural$valid[jv & rv, vars]
    under_shift_valid[[trt_var]] <- shifted$valid[jv & rv, trt_var]

    natural$train[jt & rt, pseudo] <- bound(SL_predict(fit, under_shift_train), 1e-05)
    m[jv & rv, t] <- bound(SL_predict(fit, under_shift_valid), 1e-05)

    natural$train[!rt, pseudo] <- 0
    m[!rv, t] <- 0

    pb()
  }

  list(m = m, fits = fits)
}
