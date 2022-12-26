## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

incl <- function(x){
  path <- paste0("figures/", x)
  knitr::include_graphics(path)
}

## ---- out.width="100%", echo=FALSE--------------------------------------------
incl("box-folder-id.png")

## ---- out.width="100%", echo=FALSE--------------------------------------------
incl("box-file-id.png")

