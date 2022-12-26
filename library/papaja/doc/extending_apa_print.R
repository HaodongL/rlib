## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library("papaja")

## ----htest-example------------------------------------------------------------
t_test_example <- t.test(extra ~ group, data = sleep)

class(t_test_example)

str(t_test_example)

## ----apa-results, echo = FALSE------------------------------------------------
papaja:::init_apa_results()

## ----lm-example---------------------------------------------------------------
# Data from Dobson (1990), p. 9.
ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
weight <- c(ctl, trt)
lm_fit <- lm(weight ~ group)

lm_fit_apa <- apa_print(lm_fit)

## ----estimates----------------------------------------------------------------
lm_fit_apa$estimate

## ----statistic----------------------------------------------------------------
lm_fit_apa$statistic

## ----full-result--------------------------------------------------------------
lm_fit_apa$full_result

## ----table--------------------------------------------------------------------
lm_fit_apa$table

## ----variable-labels----------------------------------------------------------
# library("tinylabels")

letters

variable_label(letters) <- "Letters of the alphabet"
variable_label(letters)

letters

str(letters)

## ----variable-label-column----------------------------------------------------
lm_fit_apa$table$statistic

## ----apa-num------------------------------------------------------------------
x <- rnorm(3) * 1e4
apa_num(x)

apa_num(x, digits = 3, big.mark = ".", decimal.mark = ",")

apa_num(Inf)

## ----apa-p--------------------------------------------------------------------
apa_p(c(0.0001, 0.05, 0.99999))

## ----apa-df-------------------------------------------------------------------
apa_df(c(12, 12.485))

apa_df(12L)

## ----apa-interval-------------------------------------------------------------
apa_interval(rnorm(2), conf.int = 0.95, interval_type = "CI")

## ----apa-confint--------------------------------------------------------------
apa_confint(rnorm(2), conf.int = 0.95)

apa_hdint(rnorm(2), conf.int = 0.95)

## ----sanitize-terms-----------------------------------------------------------
mod_terms <- c("(Intercept)", "Factor A", "Factor B",
               "Factor A:Factor B", "scale(Factor A)")
sanitize_terms(mod_terms, standardized = TRUE)

## ----prettify-terms-----------------------------------------------------------
beautify_terms(mod_terms, standardized = TRUE)

## ----aov-fit------------------------------------------------------------------
npk_aov <- aov(yield ~ block + N * P * K, npk)
npk_aov

summary(npk_aov)

## ----apa-print-aov------------------------------------------------------------
papaja:::apa_print.aov

## ----tidy-results-------------------------------------------------------------
lm_fit <- lm(mpg ~ cyl + wt, mtcars)

# Tidy and typeset output
library("broom")
tidy_lm_fit <- tidy(lm_fit, conf.int = TRUE)
tidy_lm_fit$p.value <- apa_p(tidy_lm_fit$p.value)
tidy_lm_fit$conf.int <- unlist(apa_confint(tidy_lm_fit[, c("conf.low", "conf.high")]))

str(tidy_lm_fit)

glance_lm_fit <- glance(lm_fit)
glance_lm_fit$r.squared <- apa_num(glance_lm_fit$r.squared, gt1 = FALSE)
glance_lm_fit$p.value <- apa_p(glance_lm_fit$p.value)
glance_lm_fit$df <- apa_df(glance_lm_fit$df)
glance_lm_fit$df.residual <- apa_df(glance_lm_fit$df.residual)

str(glance_lm_fit)

## ----construct-apa-results-labels---------------------------------------------
tidy_lm_fit <- apa_num(tidy_lm_fit)

variable_labels(tidy_lm_fit) <- c(
  term = "Term"
  , estimate = "$b$"
  , statistic = paste0("$t(", glance_lm_fit$df.residual, ")")
  , p.value = "$p$"
  , conf.int = "95% CI"
)

glance_lm_fit <- apa_num(glance_lm_fit)

variable_labels(glance_lm_fit) <- c(
  r.squared = "$R^2$"
  , statistic = "$F$"
  , p.value = "$p$"
  , AIC = "$\\mathrm{AIC}$"
)

## ----glue---------------------------------------------------------------------
papaja:::construct_glue(tidy_lm_fit, "estimate")

## ----construct-apa-results----------------------------------------------------
lm_results <- glue_apa_results(
    x = tidy_lm_fit
    , est_glue = papaja:::construct_glue(tidy_lm_fit, "estimate")
    , stat_glue = papaja:::construct_glue(tidy_lm_fit, "statistic")
    , term_names = sanitize_terms(tidy_lm_fit$term)
)

lm_results

## ----amend-apa-results--------------------------------------------------------
add_glue_to_apa_results(
  .x = glance_lm_fit
  , container = lm_results
  , sublist = "modelfit"
  , est_glue = c(
      r2 = "$<<svl(r.squared)>> = <<r.squared>>$"
      , aic = ""
  )
  , stat_glue = c(
      r2 = papaja:::construct_glue(glance_lm_fit, "statistic")
      , aic = "$<<svl(AIC)>> = <<AIC>>$"
  )
)

## -----------------------------------------------------------------------------
in_paren <- TRUE
papaja:::validate(in_paren, check_class = "logical", check_length = 1)

