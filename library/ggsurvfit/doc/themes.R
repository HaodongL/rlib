## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ggsurvfit)
library(patchwork)

gg_theme_default1 <- 
  survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  ggsurvfit(theme = theme_ggsurvfit_default()) +
  labs(title = "Using `theme=` argument")

gg_theme_default2 <- 
  survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  ggsurvfit(theme = NULL) +
  theme_ggsurvfit_default() +
  labs(title = "Using ggplot `+`")

gg_theme_default1 + gg_theme_default2

## -----------------------------------------------------------------------------
survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  ggsurvfit(theme = list(theme_classic(), theme(legend.position = "top"))) +
  labs(title = "Custom Plot Theme")

## -----------------------------------------------------------------------------
survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  ggsurvfit() +
  add_risktable(
    risktable_stats = "n.risk",
    theme = theme_risktable_default()
  )

## -----------------------------------------------------------------------------
survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  ggsurvfit() +
  add_risktable(
    risktable_stats = "n.risk",
    theme = theme_risktable_boxed()
  )

## -----------------------------------------------------------------------------
survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  ggsurvfit() +
  add_risktable(risktable_stats = "n.risk") +
  add_risktable_strata_symbol()

