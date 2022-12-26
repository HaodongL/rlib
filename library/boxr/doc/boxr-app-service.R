## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

incl <- function(x){
  path <- paste0("figures/", x)
  knitr::include_graphics(path)
}

## -----------------------------------------------------------------------------
library("boxr")

## ---- out.width="100%", echo=FALSE--------------------------------------------
incl("rstudio-cloud.png")

## ---- out.width="100%", echo=FALSE--------------------------------------------
incl("interactive-four-steps.png")

## ---- out.width="100%", echo=FALSE--------------------------------------------
incl("service-config-1.png")

## ---- out.width="100%", echo=FALSE--------------------------------------------
incl("service-config-2.png")

## ---- out.width="100%", echo=FALSE--------------------------------------------
incl("service-config-3.png")

## ---- out.width="100%", echo=FALSE--------------------------------------------
incl("service-authorize.png")

