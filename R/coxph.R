#' R6 Class
#' @importFrom survival strata
#' @export
Coxph <- R6::R6Class("Coxph",
  public = list(
    initialize = function(..., labels = NULL) {
      purrr::imap(list(...), function(.x, .y) {
        if (.y %in% names(self)) {
          self[[.y]] <- .x
        }
      })
      # Set data
      self$set_data()
      self$set_labels(labels)
      self$set_reference()
      self$set_formula()
      self$fit()
    },
    # Print
    print = function() {
      cli::cli_h1("Coxph Object")
      cli::cli_h3("Data")
      print(self$data)
      cli::cli_h3("Formula")
      print(self$formula)
      cli::cli_h3("Variables")
      purrr::iwalk(self$get_variables(), ~ cli::cli_li("{.y}: {.x}"))
      cli::cli_h2("Reference")
      purrr::iwalk(self$get_reference(), ~ cli::cli_li("{.y}: {.x}"))
      cli::cli_h2("Results")
      purrr::iwalk(self$results(), function(.x, .y) {
        cli::cli_h3(.y)
        print(.x)
      })
    },
    # Variables
    time = NULL,
    check_time = function() {
      if (is.null(self$time)) {
        rlang::abort("`time` is not set")
      }
    },
    event = NULL,
    check_event = function() {
      if (is.null(self$event)) {
        rlang::abort("`event` is not set")
      }
    },
    treatment = NULL,
    check_treatment = function() {
      if (is.null(self$treatment)) {
        rlang::abort("`treatment` is not set")
      }
    },
    covariates = NULL,
    strata = NULL,
    random_effects = NULL,
    interactions = NULL,
    get_variables = function() {
      list(
        time = self$time,
        event = self$event,
        treatment = self$treatment,
        covariates = self$covariates,
        strata = self$strata,
        interactions = self$interactions,
        random_effects = self$random_effects,
        group_by = self$group_by
      )
    },
    unpack_interactions = function(vars = self$get_variables()) {
      if (is.null(vars$interactions)) {
        res <- vars
      } else {
        vars <- unlist(unname(vars))
        vars <- unlist(strsplit(vars, ":"))
        vars <- unlist(stringr::str_split(vars, stringr::fixed("*")))
      }
      res <- unlist(vars) %>%
        unname(vars) %>%
        unique()
      return(res)
    },
    build_reference_for_interaction_term = function(x) {
      vars <- self$unpack_interactions(x)
      refs <- purrr::map(vars, function(.x) {
        if (is.factor(self$data[[.x]])) {
          levels(self$data[[.x]])[1]
        } else if (is.numeric(self$data[[.x]])) {
          .x
        } else {
          rlang::abort("Unable to determine reference for {.x}")
        }
      })
      var <- paste(vars, collapse = "/")
      ref <- paste(refs, collapse = "/")
      res <- list()
      res[[var]] <- ref
      return(res)
    },
    build_comparison_for_interaction_term = function(x) {
      x %>%
        stringr::str_split(":") %>%
        purrr::map(function(.x) {
          .s <- stringr::str_split(.x, " ")
          terms <- paste(purrr::map(.s, ~ .x[[1]][1]), collapse = "/")
          comp <- paste(purrr::map(.s, function(.y) {
            if (length(.y) > 1) {
              .y[[2]][1]
            } else {
              .y[[1]][1]
            }
          }), collapse = "/")
          paste(terms, "splitthere", comp, sep = "")
        }) %>%
        unlist()
    },
    # Checks
    check_variables = function() {
      if (!is.null(self$interactions)) {
        if (stringr::str_detect(self$interactions, stringr::fixed("*"))) {
          rlang::abort("Please specify interactions with `:` instead of `*`")
        }
      }
      vars <- self$unpack_interactions() %>%
        purrr::compact()
      if (!all(vars %in% names(self$data))) {
        missing <- vars[!vars %in% names(self$data)]
        msg <- glue::glue("`{missing}` not present in data")
        rlang::abort(msg)
      }
    },
    # Data
    data = NULL,
    group_by = NULL,
    set_data = function(data = self$data) {
      if (!is.null(data)) {
        self$data <- data
      } else {
        data <- self$data
      }
      if (is.null(self$data)) rlang::abort("No data provided - use `set_data` method to update")
      self$check_variables()
      if (is.null(self$group_by)) self$group_by <- waRRior::get_groups(data)
      data <- data %>%
        dplyr::ungroup() %>%
        dplyr::select_at(self$unpack_interactions()) %>%
        dplyr::mutate_if(is.character, factor) %>%
        dplyr::group_by_at(self$group_by)
      self$data <- data
      invisible(self)
    },
    check_data = function() {
      if (is.null(self$data)) rlang::abort("No data provided - use `set_data` method to update")
    },
    # Reference ================================================================
    #' @field Reference levels to be compared againsts
    reference = NULL,
    # ---------------------------------------------------------
    #' @description Set reference levels
    set_reference = function(data = self$data) {
      if (!is.null(data)) {
        .reference <- data %>%
          dplyr::select_at(unique(c(self$treatment, self$covariates, self$strata, self$random_effects))) %>%
          dplyr::select_if(is.factor) %>%
          droplevels() %>%
          purrr::map(~ levels(.x)[1])
        .numeric <- data %>%
          dplyr::ungroup() %>%
          dplyr::select_if(is.numeric) %>%
          purrr::map(~"")
        .interactions <- self$interactions %>%
          self$build_reference_for_interaction_term()
        self$reference <- c(.reference, .numeric, .interactions)
      }
      invisible(self)
    },
    # ---------------------------------------------------------
    #' @description Get reference levels
    get_reference = function(x = self$get_variables()) {
      self$reference[unlist(x)] %>%
        purrr::compact()
    },
    # Formula ==================================================
    #' @field Formula to be used in the model
    formula = NULL,
    # ---------------------------------------------------------
    #' @description Creates and set the formula to be used in the model
    set_formula = function() {
      self$formula <- nightowl::create_Surv_formula(
        data = self$data,
        time = self$time,
        event = self$event,
        treatment = self$treatment,
        covariates = self$covariates,
        interactions = self$interactions,
        strata = self$strata,
        random_effects = self$random_effects
      )
    },
    # ---------------------------------------------------------
    #' @description Checks if the formula is present
    check_formula = function() {
      if (is.null(self$formula)) {
        rlang::abort("No formula provided - use `set_formula` method to update")
      }
    },
    # Model ====================================================
    models = NULL,
    # ---------------------------------------------------------
    args_model = NULL,
    # ---------------------------------------------------------
    fit = function() {
      self$check_data()
      self$check_time()
      self$check_event()
      self$check_treatment()
      self$check_variables()
      self$check_formula()
      if (!dplyr::is_grouped_df(self$data)) {
        data_list <- list(Overall = self$data)
      } else {
        data_list <- waRRior::named_group_split_at(self$data, self$group_by, keep = T, verbose = T)
      }
      self$models <- purrr::map(data_list, purrr::safely(function(.data) {
        do.call(
          survival::coxph,
          c(
            list(
              data = .data,
              formula = self$formula
            ),
            self$args_model
          )
        )
      }))
    },
    # Results ------------------------------------------------------------------
    exponentiate = TRUE,
    # ---------------------------------------------------------
    log_x = TRUE,
    # ---------------------------------------------------------
    results = function(exponentiate = self$exponentiate, log_x = self$log_x) {
      variables <- self$get_variables()
      purrr::map(self$models, "result") %>%
        purrr::imap(function(.result, .group) {
          res <- .result %>%
            broom::tidy(exponentiate = exponentiate, conf.int = TRUE)
          if (nrow(res) == 0) {
            return(NULL)
          } else {
            non_interaction_term <- !stringr::str_detect(res$term, ":")
            res <- res %>%
              dplyr::mutate(term = stringr::str_replace_all(term, "`", "")) %>%
              dplyr::mutate(Subgroup = .group) %>%
              purrr::reduce(c(variables$treatment, variables$covariates), function(.in, .cov) {
                .in$term[non_interaction_term] <- stringr::str_replace(.in$term[non_interaction_term], paste0("^", Hmisc::escapeRegex(.cov)), paste0(.cov, "splitthere"))
                .in$term[!non_interaction_term] <- stringr::str_replace_all(.in$term[!non_interaction_term], paste0(Hmisc::escapeRegex(.cov)), paste0(.cov, " "))
                return(.in)
              }, .init = .)
            res$term[!non_interaction_term] <- res$term[!non_interaction_term] %>%
              self$build_comparison_for_interaction_term()
            res %>%
              {
                x <- .
                if (nrow(x) == 0) {
                  return(NULL)
                }
                x$comparison <- stringr::str_split(x$term, stringr::fixed("splitthere")) %>%
                  purrr::map(~ .x[[2]]) %>%
                  unlist()
                x$term <- stringr::str_split(x$term, stringr::fixed("splitthere")) %>%
                  purrr::map(~ .x[[1]]) %>%
                  unlist()
                x
              } %>%
              dplyr::arrange(estimate) %>%
              dplyr::mutate(term = factor(term, c(variables$treatment, unique(waRRior::pop(.$term, variables$treatment))))) %>%
              dplyr::arrange(term) %>%
              dplyr::mutate(reference = unlist(purrr::map(as.character(term), ~ self$get_reference(.x)))) %>%
              dplyr::select(Subgroup, term, reference, comparison, estimate, tidyselect::everything())
          }
        }) %>%
        purrr::compact()
    },
    # Errors -------------------------------------------------------------------
    errors = function() {
      purrr::map(self$models, "error")
    },
    # N ------------------------------------------------------------------
    N = function() {
      variables <- self$get_variables()
      data_list <- waRRior::named_group_split_at(self$data, self$group_by, keep = T, verbose = T)
      if (is.null(self$group_by)) {
        names(data_list) <- "Overall"
      }
      purrr::imap(data_list, function(.data, .subgroup) {
        res <- purrr::map(c(variables$treatment, self$covariates, self$strata), function(.var) {
          if (!is.numeric(.data[[.var]])) {
            dplyr::select_at(.data, c(.var, variables$event)) %>%
              waRRior::tally_at(c(.var, variables$event)) %>%
              dplyr::mutate(!!rlang::sym(variables$event) := as.character(!!rlang::sym(variables$event))) %>%
              dplyr::select_at(c(.var, "n", variables$event)) %>%
              dplyr::rename(comparison = .var) %>%
              dplyr::mutate(term = .var) %>%
              # dplyr::rename(`Events/N` = n) %>%
              dplyr::mutate(comparison = as.character(comparison))
          } else {
            dplyr::select_at(.data, c(.var, variables$event)) %>%
              waRRior::tally_at(c(variables$event)) %>%
              dplyr::mutate(!!rlang::sym(variables$event) := as.character(!!rlang::sym(variables$event))) %>%
              dplyr::select_at(c("n", variables$event)) %>%
              dplyr::mutate(term = .var) %>%
              # dplyr::rename(`Events/N` = n) %>%
              dplyr::mutate(comparison = "")
          }
        }) %>%
          dplyr::bind_rows()
        .N <- res %>%
          dplyr::group_by_at(c("comparison", "term")) %>%
          dplyr::summarise(N = sum(n, na.rm = T), event = !!rlang::sym(variables$event)) %>%
          dplyr::filter(event == "1") %>%
          dplyr::select(-event)
        res <- res %>%
          dplyr::inner_join(.N) %>%
          dplyr::mutate(`Events/N` = paste(n, N, sep = "/")) %>%
          waRRior::drop_columns(c("n", "N")) %>%
          dplyr::mutate(Subgroup = .subgroup) %>%
          dplyr::filter(!!rlang::sym(variables$event) == "1")
        res
      }) %>%
        dplyr::bind_rows()
    },
    # Annotation ----------------------------------------------------------------
    add_caption = TRUE,
    title = NULL,
    # ---------------------------------------------------------
    caption = function() {
      variables <- self$get_variables()
      if (self$add_caption) {
        if (is.null(variables$covariates) && is.null(variables$strata)) {
          .title <- "Univariate Cox's Proportional Hazard Model"
        } else {
          .title <- "Multivariate Cox's Proportional Hazard Model"
        }
        if (!is.null(self$title)) {
          .title <- glue::glue("{self$title}<br>{.title}")
        }
        .title
      } else {
        NULL
      }
    },
    # ---------------------------------------------------------
    add_footnote = TRUE,
    # ---------------------------------------------------------
    footnote = function() {
      variables <- self$get_variables()
      if (self$add_footnote) {
        if (is.null(variables$covariates) && is.null(variables$strata)) {
          .footnote <- "Univariate Analysis"
        } else {
          comb <- c(variables$covariates, variables$strata)
          if (is.null(self$labels)) {
            labels <- comb %>% purrr::set_names(comb)
          } else {
            labels <- self$labels
          }
          .labeled_covariates <- labels[variables$covariates]
          .labeled_covariates[is.na(.labeled_covariates)] <- variables$covariates[is.na(.labeled_covariates)]
          .labeled_strata <- labels[variables$strata]
          .labeled_strata[is.na(.labeled_strata)] <- variables$strata[is.na(.labeled_strata)]

          if (length(.labeled_covariates) == 0) .labeled_covariates <- "none"
          if (length(.labeled_strata) == 0) .labeled_strata <- "none"
          .footnote <- paste(
            stringr::str_wrap(paste("Covariates: ", paste(.labeled_covariates, collapse = "; ")), width = 200),
            stringr::str_wrap(paste("Stratified by: ", paste(.labeled_strata, collapse = "; ")), width = 200),
            sep = "\n"
          )
        }
        .footnote
      } else {
        NULL
      }
    },
    # Labels ------------------------------------------------------------------
    labels = NULL,
    # ---------------------------------------------------------
    set_labels = function(labels) {
      if (!is.null(labels)) {
        .data <- self$data
        names(.data) <- nightowl::get_labels(names(.data), labels)
        self$data <- .data
        self$get_variables() %>%
          purrr::iwalk(function(.x, .y) {
            self[[.y]] <- nightowl::get_labels(.x, labels)
          })
      }
      self$labels <- labels
    },
    # Arrange ------------------------------------------------------------------
    arrange_by = "Hazard Ratio",
    # ---------------------------------------------------------
    arrange = function(x) {
      if (!is.null(self$arrange_by)) {
        dplyr::arrange(dplyr::ungroup(x), !!!rlang::syms(self$arrange_by))
      } else {
        x
      }
    },
    # Raw ---------------------------------------------------------------------
    conf_range = NULL,
    # ---------------------------------------------------------
    label_left = "Comparison better",
    # ---------------------------------------------------------
    label_right = "Reference better",
    # ---------------------------------------------------------
    raw = function(drop = NULL, keep_only_treatment = TRUE) {
      results <- dplyr::inner_join(
        self$N(),
        dplyr::bind_rows(self$results())
      ) %>%
        dplyr::distinct()

      if (keep_only_treatment) {
        results <- results %>%
          dplyr::filter(term == self$get_variables()$treatment)
      }

      if (is.null(self$conf_range)) {
        self$conf_range <- c(
          min(results$conf.low[results$conf.low != -Inf], na.rm = T),
          max(results$conf.high[results$conf.high != Inf], na.rm = T)
        )
      }

      res <- results %>%
        dplyr::group_by_all() %>%
        dplyr::group_split() %>%
        purrr::map(function(.x) {
          tryCatch(
            {
              .p <- nightowl::add_inline_forestplot(
                x = log(.x$estimate),
                xmin = log(.x$conf.low),
                xmax = log(.x$conf.high),
                xlim = log(self$conf_range),
                xintercept = 0
              )
              .x$Visualization <- .p
              .x
            },
            error = function(e) {
              .x$Visualization <- NULL
              .x
            }
          )
        }) %>%
        dplyr::bind_rows()
      forest_label <- glue::glue("
        <div
        style= 'font-size: smaller;display:flex; flex-direction:column;align-items:center;'>
        <div>log(HR)</div>
        <div>← {self$label_left} | {self$label_right} →</div>
        </div>
      ")
      res <- res %>%
        dplyr::arrange(`estimate`) %>%
        dplyr::mutate(`conf.low` = round(`conf.low`, 2)) %>%
        dplyr::mutate(`conf.high` = round(`conf.high`, 2)) %>%
        dplyr::mutate_if(is.numeric, ~ round(.x, 3)) %>%
        dplyr::mutate(HR = glue::glue("{estimate} ({conf.low}-{conf.high})")) %>%
        dplyr::mutate(reference = dplyr::case_when(
          reference == "" ~ "↑",
          TRUE ~ reference
        )) %>%
        dplyr::mutate(comparison = dplyr::case_when(
          comparison == "" ~ "↓",
          TRUE ~ comparison
        )) %>%
        dplyr::select(
          Subgroup,
          Term = term,
          Comparison = comparison,
          !!rlang::sym(forest_label) := Visualization,
          Reference = reference,
          `Hazard Ratio` = HR,
          `p Value` = p.value,
          `Events/N` = `Events/N`,
        )
      res$`p Value` <- purrr::map_chr(res$`p Value`, ~ nightowl::format_p_value(.x))
      if (!is.null(drop)) {
        res <- waRRior::drop_columns(res, drop)
        res <- res %>%
          dplyr::select(-Subgroup) %>%
          self$arrange()
      }
      if (is.null(self$group_by)) {
      } else {
        subgroups <- stringr::str_split(res$Subgroup, " / ") %>%
          purrr::map(~ stringr::str_split(.x, "==")) %>%
          purrr::map(function(.x) {
            purrr::map(.x, function(.y) {
              .res <- list()
              .res[[.y[[1]]]] <- .y[[2]]
              tibble::as_tibble(.res)
            }) %>%
              dplyr::bind_cols()
          }) %>%
          dplyr::bind_rows()
        res <- dplyr::bind_cols(subgroups, res) %>%
          dplyr::select(-Subgroup) %>%
          self$arrange()
      }
      return(res)
    },
    # Extract ==================================================================
    #' @description Extract estimates of treament effect
    coefficients = function() {
      fits <- purrr::map(self$models, "result")
      coefs <- fits %>%
        purrr::map(purrr::safely(function(.fit) {
          .fit$coef
        })) %>%
        purrr::map("result") %>%
        purrr::compact()
      res <- purrr::reduce(coefs, ~ rbind(.x, .y, make.row.names = TRUE))
      res <- matrix(res, ncol = length(coefs[[1]]), nrow = length(fits))
      rownames(res) <- names(coefs)
      colnames(res) <- stringr::str_replace_all(names(coefs[[1]]), "`", "")
      res
    },
    se = function() {
      fits <- purrr::map(self$models, "result")
      ses <- fits %>%
        purrr::map(purrr::safely(function(.fit) {
          sqrt(diag(vcov(.fit)))
        })) %>%
        purrr::map("result") %>%
        purrr::compact()
      res <- purrr::reduce(ses, ~ rbind(.x, .y))
      res <- matrix(res, ncol = length(ses[[1]]), nrow = length(fits))
      rownames(res) <- names(ses)
      colnames(res) <- stringr::str_replace_all(names(ses[[1]]), "`", "")
      res
    },
    TE = function(term = self$get_variables()$treatment) {
      coefs <- self$coefficients()
      cols <- colnames(coefs)[stringr::str_starts(colnames(coefs), Hmisc::escapeRegex(term))]
      coefs[, cols, drop = FALSE][, 1]
    },
    # ---------------------------------------------------------
    #' @description Extract standard errors of treament effect
    seTE = function(term = self$get_variables()$treatment) {
      se <- self$se()
      cols <- colnames(se)[stringr::str_starts(colnames(se), Hmisc::escapeRegex(term))]
      se[, cols, drop = FALSE][, 1]
    },
    # Metaanalysis =============================================================
    #' @field Parameters to be passed to `meta::metagen`
    options_metagen = list(
      fixed = FALSE,
      random = TRUE,
      method.tau = "SJ",
      hakn = TRUE,
      prediction = TRUE,
      sm = "HR"
    ),
    # ---------------------------------------------------------
    #' @description Run metaanalysis
    metagen = function(title = var) {
      grouping <- self$get_variables()$group_by
      TE <- self$TE()
      seTE <- self$seTE()
      purrr::map(colnames(TE), purrr::safely(function(.x) {
        params <- list(
          TE = unname(TE[, .x]),
          seTE = unname(seTE[, .x]),
          # data = self$data
          studlab = rownames(TE)
          # title = title
        )
        meta <- do.call(meta::metagen, c(params, self$options_metagen))
        attributes(meta)$grouping <- grouping
        attributes(meta)$term <- .x
        meta
      }))
    },
    # ---------------------------------------------------------
    #' @description Extract metaanalysis results
    metagen_results = function(...) {
      self$metagen(...) %>%
        purrr::map("result")
    },
    # ---------------------------------------------------------
    #' @description Extract metaanalysis errors
    metagen_errors = function(...) {
      self$metagen(...) %>%
        purrr::map("error")
    },
    # ---------------------------------------------------------
    #' @description Summarise metaanalysis results
    metagen_summarise = function(...) {
      meta <- self$metagen_results(...)
      purrr::map(meta, function(.x) {
        tibble::tibble(
          grouping = attributes(.x)$grouping,
          term = attributes(.x)$term,
          k = .x$k,
          HR = exp(.x$TE.random),
          lower = exp(.x$lower.random),
          upper = exp(.x$upper.random),
          pvalue = .x$pval.random,
          tau = .x$tau,
          se.tau = .x$se.tau,
          lower.tau = .x$lower.tau,
          upper.tau = .x$upper.tau,
          tau2 = .x$tau2,
          se.tau2 = .x$se.tau2,
          lower.tau2 = .x$lower.tau2,
          upper.tau2 = .x$upper.tau2,
          H = .x$H,
          lower.H = .x$lower.H,
          upper.H = .x$upper.H,
          Q = .x$Q,
          df.Q = .x$df.Q,
          pval.Q = .x$pval.Q,
          method_tau = self$options_metagen$method.tau,
          I2 = .x$I2,
          lower.I2 = .x$lower.I2,
          upper.I2 = .x$upper.I2,
          Rb = .x$Rb,
          lower.Rb = .x$lower.Rb,
          upper.Rb = .x$upper.Rb,
          prediction = .x$prediction,
          seTE.predict = .x$seTE.predict,
          lower.predict = .x$lower.predict,
          upper.predict = .x$upper.predict,
          weights = list(weights(.x))
        )
      }) %>%
        dplyr::bind_rows()
    },
    # ---------------------------------------------------------
    metagen_raw = function(...) {
      meta <- self$metagen_summarise(...)
      grouping <- meta$grouping[1]
      raw <- self$raw()
      forest_column <- purrr::imap(raw, ~ if (nightowl::is_NightowlPlots(.x)) {
        return(.y)
      }) %>%
        unlist() %>%
        unname()
      raw %>%
        dplyr::mutate(term = paste0(Term, Comparison)) %>%
        waRRior::named_group_split(term, keep = F) %>%
        purrr::map(~ dplyr::arrange(.x, !!rlang::sym(grouping))) %>%
        purrr::imap(function(.x, .y) {
          obj <- .x[[forest_column]][[1]]
          .options_svg <- obj$options_svg
          .gg <- obj$plot
          .gg$layers <- NULL
          .meta <- dplyr::filter(meta, term == .y)
          polygon.x <- c(.meta$lower, .meta$HR, .meta$upper)
          polygon.x <- log(c(polygon.x, rev(polygon.x)))
          polygon.y <- c(0, 1, 0)
          polygon.y <- c(polygon.y, -(polygon.y))
          polygon <- data.frame(x = polygon.x, y = polygon.y)
          p <- .gg +
            ggplot2::geom_vline(xintercept = 0, color = picasso::roche_colors("red"), linetype = "solid", size = 1) +
            ggplot2::geom_polygon(
              data = polygon,
              ggplot2::aes(
                y = y,
                x = x
              ),
              fill = picasso::roche_colors("apple"),
              color = picasso::roche_colors("black")
            ) +
            ggplot2::ylim(c(-1.5, 2))
          p <- nightowl::Plot$new(
            plot = p,
            type = "DiamondPlot",
            resize = FALSE,
            options_svg = .options_svg
          )

          w <- .meta$weights[[1]]
          split <- stringr::str_split(rownames(w), "==")
          column <- split[[1]][1]
          values <- purrr::map(split, ~ .x[2]) %>% unlist()
          .weights <- tibble::tibble(
            !!rlang::sym(column) := values,
            Weight = paste0(round(w[, "p.random"]), "%")
            # w = w[, "w.random"]
          )
          .x <- dplyr::left_join(.x, .weights)
          tmp <- purrr::map2(.x[[forest_column]], .x$Weight, function(.obj, .w) {
            .gg <- .obj$plot
            .gg$layers[[3]]$aes_params$size <- as.numeric(stringr::str_replace(.w, "%", "")) * 0.3
            res <- nightowl::Plot$new(
              plot = .gg,
              type = "WeightPlot",
              resize = FALSE,
              options_svg = .options_svg
            )
          })
          .x[[forest_column]] <- do.call(nightowl::new_NightowlPlots, tmp)
          diamond <- tibble::tibble(TERM = "")
          diamond[[forest_column]] <- nightowl::new_NightowlPlots(p)
          footnote <- as.character(glue::glue("
              <div style = 'display:flex; flex-wrap: wrap; align-items: center;'>
                <div>Random Effects Model:</div>
                <div style='background-color: #EEEEEE; border-radius: 5px; margin: 3px; padding: 5px;'>Hazard Ratio: {round(.meta$HR, 3)} [{round(.meta$lower,2)};{round(.meta$upper,2)}]</div>
                <div style='background-color: #EEEEEE; border-radius: 5px; margin: 3px; padding: 5px;'>p-value: {nightowl::format_p_value(.meta$pvalue)}</div>
              </div>
              <div style = 'display:flex; flex-wrap: wrap; align-items: center;'>
                <div>Heterogenity:</div>
                <div style='background-color: #EEEEEE; border-radius: 5px; margin: 3px; padding: 5px;'>I<sup>2</sup>: {100*round(.meta$I2, 2)}%</div>
                <div style='background-color: #EEEEEE; border-radius: 5px; margin: 3px; padding: 5px;'>𝜏<sup>2</sup>: {round(.meta$tau, 3)}</div>
                <div style='background-color: #EEEEEE; border-radius: 5px; margin: 3px; padding: 5px;'>Q: {round(.meta$Q, 1)} (pvalue: {nightowl::format_p_value(.meta$pval.Q)})</div>
              </div>
              <div>
                <div>Prediction Interval (HR): [{round(exp(.meta$lower.predict), 2)}; {round(exp(.meta$upper.predict), 2)}]</div>
              </div>
            "))
          res <- dplyr::bind_rows(.x, diamond) %>%
            dplyr::select(-TERM)
          attributes(res)$footnote <- footnote
          return(res)
        })
    },
    # ---------------------------------------------------------
    metagen_kable = function() {
      meta <- self$metagen_raw()
      meta %>%
        purrr::map(~ nightowl::render_kable(.x, footnote = attributes(.x)$footnote))
    },
    metagen_html = function() {
      meta <- self$metagen_kable()
      meta %>%
        purrr::map(~ htmltools::HTML(.x))
    },
    metagen_output = function() {
      meta <- self$metagen_html()
      meta %>%
        shiny::div(
          style = "font-family: 'Lato', sans-serif;",
          kableExtra:::html_dependency_lightable(),
        ) %>%
        htmltools::browsable()
    },
    # Render Outputs ***********************************************************
    # Kable ====================================================================
    #' @field options to be passed to `nightowl::render_kable()`
    options_kable = list(),
    # ---------------------------------------------------------
    #' @description Renders a kable table
    #' @param drop A character vector of columns to drop
    #' @param keep_only_treatment A logical indicating whether to keep only the treatment effect estimates
    kable = function(drop = NULL, keep_only_treatment = TRUE) {
      raw <- self$raw(drop = drop, keep_only_treatment = keep_only_treatment)
      align <- rep("l", length(names(raw)))
      names(align) <- names(raw)
      print(align)
      align["Comparison"] <- "c"
      align["Reference"] <- "c"
      align <- unname(align)
      print(align)
      do.call(nightowl::render_kable, c(list(.tbl = raw, caption = self$caption(), footnote = self$footnote(), align = align), self$options_kable))
    },
    # HTML =====================================================================
    #' @description Returns output as HTML to be used in e.g. a Shiny app
    html = function(drop = NULL, keep_only_treatment = TRUE) {
      shiny::HTML(self$kable(drop = drop, keep_only_treatment = keep_only_treatment))
    },
    # Output ===================================================================
    #' @description Opens a styled HTML report in the browser
    output = function(drop = NULL, keep_only_treatment = TRUE) {
      self$html(drop = drop, keep_only_treatment = keep_only_treatment) %>%
        shiny::div(
          style = "font-family: 'Lato', sans-serif;",
          kableExtra:::html_dependency_lightable(),
        ) %>%
        htmltools::browsable()
    },
    # Reactables ===============================================================
    #' @field options to be passed to `nightowl::render_reactable()`
    options_reactables = list(defaultPageSize = 30),
    # ---------------------------------------------------------
    #' @description Renders a reactable table
    reactable = function(drop = NULL, keep_only_treatment = FALSE, ...) {
      .args <- list(...)
      .options <- c(.args, self$options_reactables[!names(self$options_reactables) %in% names(.args)])
      res <- shiny::div(
        style = "
          font-family: 'Lato', sans-serif;
          display: flex;
          flex-direction: column;
          align-content: flex-start;
          align-items: center;
        ",
        shiny::h3(self$caption()),
        do.call(nightowl::render_reactable, c(list(.tbl = self$raw(drop = drop, keep_only_treatment = keep_only_treatment)), .options)),
        shiny::div(
          style = "font-size: 0.8em;",
          glue::glue(self$footnote())
        )
      ) %>%
        htmltools::browsable()
      return(res)
    }
  )
)
# ==============================================================================
#' @export
coefficients <- function(models) {
  coefs <- models %>%
    purrr::map(purrr::safely(function(.model) {
      .model$coef
    })) %>%
    purrr::map("result") %>%
    purrr::compact()
  res <- purrr::reduce(coefs, ~ rbind(.x, .y))
  res <- matrix(res, ncol = length(coefs[[1]]), nrow = length(models))
  rownames(res) <- names(models)
  colnames(res) <- names(coefs[[1]])
  res
}
#' @export
se <- function(models) {
  ses <- models %>%
    purrr::map(purrr::safely(function(.model) {
      sqrt(diag(vcov(.model)))
    })) %>%
    purrr::map("result") %>%
    purrr::compact()
  res <- purrr::reduce(ses, ~ rbind(.x, .y))
  res <- matrix(res, ncol = length(ses[[1]]), nrow = length(models))
  rownames(res) <- names(ses)
  colnames(res) <- names(ses[[1]])
  res
}
#' @export
metagen_default_options <- function() {
  options_metagen <- list(
    fixed = FALSE,
    random = TRUE,
    method.tau = "SJ",
    hakn = TRUE,
    prediction = TRUE,
    sm = "HR"
  )
}
#' @export
metagen <- function(models, term, options = metagen_default_options()) {
  TE <- nightowl::coefficients(models = models)
  seTE <- nightowl::se(models = models)
  params <- list(
    TE = unname(TE[, term]),
    seTE = unname(seTE[, term]),
    # data = self$data
    studlab = rownames(TE)
    # title = title
  )
  meta <- do.call(meta::metagen, c(params, options))
  attributes(meta)$options <- options
  attributes(meta)$term <- term
  meta
}
#' @export
metagen_tidy <- function(meta) {
  tibble::tibble(
    grouping = attributes(meta)$grouping,
    term = attributes(meta)$term,
    k = meta$k,
    TE.random = meta$TE.random,
    TE.random.lower = meta$lower.random,
    TE.random.upper = meta$upper.random,
    `exp(TE)` = exp(meta$TE.random),
    `exp(TE).random.lower` = exp(meta$lower.random),
    `exp(TE).random.upper` = exp(meta$upper.random),
    pvalue = meta$pval.random,
    tau = meta$tau,
    se.tau = meta$se.tau,
    lower.tau = meta$lower.tau,
    upper.tau = meta$upper.tau,
    tau2 = meta$tau2,
    se.tau2 = meta$se.tau2,
    lower.tau2 = meta$lower.tau2,
    upper.tau2 = meta$upper.tau2,
    H = meta$H,
    lower.H = meta$lower.H,
    upper.H = meta$upper.H,
    Q = meta$Q,
    df.Q = meta$df.Q,
    pval.Q = meta$pval.Q,
    method_tau = meta$options$method.tau,
    I2 = meta$I2,
    lower.I2 = meta$lower.I2,
    upper.I2 = meta$upper.I2,
    Rb = meta$Rb,
    lower.Rb = meta$lower.Rb,
    upper.Rb = meta$upper.Rb,
    prediction = meta$prediction,
    seTE.predict = meta$seTE.predict,
    lower.predict = meta$lower.predict,
    upper.predict = meta$upper.predict,
    weights = list(weights(meta))
  )
}
#' @export
metagen_forest <- function(meta,
                           conf_range = NULL,
                           label_left = "Comparison Better",
                           label_right = "Reference Better",
                           arrange_by = "TE") {
  meta_df <- as.data.frame(meta)

  if (is.null(conf_range)) {
    conf_range <- c(
      min(meta_df$upper[meta_df$upper != -Inf], na.rm = T),
      max(meta_df$lower[meta_df$lower != Inf], na.rm = T)
    )
  }


  purrr::pmap(
    list(
      TE = meta_df$TE,
      lower = meta$lower,
      upper = meta$upper
    ),
    function(TE, lower, upper) {
      tryCatch(
        {
          .p <- nightowl::add_inline_forestplot(
            x = TE,
            xmin = lower,
            xmax = upper,
            xlim = conf_range,
            xintercept = 0
          )
          return(res)
        },
        error = function(e) {
          print(e)
          return(NULL)
        }
      )
    }
  )


  forest_label <- glue::glue("
        <div
        style= 'font-size: smaller;display:flex; flex-direction:column;align-items:center;'>
        <div>log(HR)</div>
        <div>← {label_left} | {label_right} →</div>
        </div>
      ")


  # # Prepare diamond
  # forest_column <- purrr::imap(res, ~if(nightowl::is_NightowlPlots(.x)) return(.y)) %>%
  #  unlist() %>%
  #  unname()
  # obj <- res[[forest_column]][[1]]
  # .options_svg <- obj$options_svg
  # .gg <- obj$plot
  # .gg$layers <- NULL
  # TE.random <- meta$TE.random
  # lower.random <- meta$lower.random
  # upper.random <- meta$upper.random
  #
  # polygon.x <- c(lower.random, TE.random, upper.random)
  # polygon.y <- c(0, 1, 0)
  # polygon.y <- c(polygon.y, -(polygon.y))
  # polygon <- data.frame(x = polygon.x, y = polygon.y)
  #
  # p <- .gg +
  #   ggplot2::geom_vline(xintercept = 0, color = picasso::roche_colors("red"), linetype = "solid", size = 1) +
  #   ggplot2::geom_polygon(
  #     data = polygon,
  #     ggplot2::aes(
  #       y = y,
  #       x = x
  #     ),
  #     fill = picasso::roche_colors("apple"),
  #     color = picasso::roche_colors("black")
  #   ) +
  #   ggplot2::ylim(c(-1.5, 2))

  #   p <- nightowl::Plot$new(
  #     plot = p,
  #     type = "DiamondPlot",
  #     resize = FALSE,
  #     options_svg = .options_svg
  #   )
  #
  #   w <- .meta$weights[[1]]
  #   split <- stringr::str_split(rownames(w), "==")
  #   column <- split[[1]][1]
  #   values <- purrr::map(split, ~.x[2]) %>% unlist()
  #  .weights <- tibble::tibble(
  #     !!rlang::sym(column) := values,
  #     Weight = paste0(round(w[, "p.random"]), "%")
  #     #w = w[, "w.random"]
  #   )
  #   .x <- dplyr::left_join(.x, .weights)
  #   tmp <- purrr::map2(.x[[forest_column]], .x$Weight, function(.obj, .w){
  #    .gg <- .obj$plot
  #     .gg$layers[[3]]$aes_params$size <- as.numeric(stringr::str_replace(.w, "%", "")) * 0.3
  #     res <- nightowl::Plot$new(
  #       plot = .gg,
  #       type = "WeightPlot",
  #       resize = FALSE,
  #       options_svg = .options_svg
  #     )
  #   })
  #   .x[[forest_column]] <- do.call(nightowl::new_NightowlPlots, tmp)
  #   diamond <- tibble::tibble(TERM = "")
  #   diamond[[forest_column]] <- nightowl::new_NightowlPlots(p)
  #   footnote = as.character(glue::glue("
  #     <div style = 'display:flex; flex-wrap: wrap; align-items: center;'>
  #       <div>Random Effects Model:</div>
  #       <div style='background-color: #EEEEEE; border-radius: 5px; margin: 3px; padding: 5px;'>Hazard Ratio: {round(.meta$HR, 3)} [{round(.meta$lower,2)};{round(.meta$upper,2)}]</div>
  #       <div style='background-color: #EEEEEE; border-radius: 5px; margin: 3px; padding: 5px;'>p-value: {nightowl::format_p_value(.meta$pvalue)}</div>
  #     </div>
  #     <div style = 'display:flex; flex-wrap: wrap; align-items: center;'>
  #       <div>Heterogenity:</div>
  #       <div style='background-color: #EEEEEE; border-radius: 5px; margin: 3px; padding: 5px;'>I<sup>2</sup>: {100*round(.meta$I2, 2)}%</div>
  #       <div style='background-color: #EEEEEE; border-radius: 5px; margin: 3px; padding: 5px;'>𝜏<sup>2</sup>: {round(.meta$tau, 3)}</div>
  #       <div style='background-color: #EEEEEE; border-radius: 5px; margin: 3px; padding: 5px;'>Q: {round(.meta$Q, 1)} (pvalue: {nightowl::format_p_value(.meta$pval.Q)})</div>
  #     </div>
  #     <div>
  #       <div>Prediction Interval (HR): [{round(exp(.meta$lower.predict), 2)}; {round(exp(.meta$upper.predict), 2)}]</div>
  #     </div>
  #   "))
  #   res <- dplyr::bind_rows(.x, diamond) %>%
  #     dplyr::select(-TERM)
  #   attributes(res)$footnote <- footnote
  #   return(res)
  # })
}
