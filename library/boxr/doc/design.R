## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library("tibble")
library("jsonlite")
library("purrr")

## -----------------------------------------------------------------------------
str(mtcars[, c("wt", "mpg")])
str(mtcars[, "mpg"])

## -----------------------------------------------------------------------------
str(mtcars[, "mpg", drop = FALSE])

## -----------------------------------------------------------------------------
str(as_tibble(mtcars)[, "mpg"])

## -----------------------------------------------------------------------------
content <- 
  fromJSON(
    '{
      "entries": [
        {
          "id": "12345",
          "etag": "1",
          "type": "file",
          "sequence_id": "3",
          "name": "Contract.pdf",
          "sha1": "85136C79CBF9FE36BB9D05D0639C70C265C18D37",
          "file_version": {
            "id": "12345",
            "type": "file_version",
            "sha1": "134b65991ed521fcfe4724b7d814ab8ded5185dc"
          }
        }
      ],
      "limit": 1000,
      "offset": 2000,
      "order": [
        {
          "by": "type",
          "direction": "ASC"
        }
      ],
      "total_count": 5000
    }',
    simplifyVector = FALSE
  )

## -----------------------------------------------------------------------------
str(content)

## -----------------------------------------------------------------------------
# we could imagine this as a function that would contain all our parsing rules
parse_entry <- function(entry) {
  
  # if we import tidyselect, we can use functions like `ends_with()`
  entry <- purrr::map_at(entry, c("etag", "sequence_id"), as.numeric)
  entry <- purrr::map_if(entry, is.list, parse_entry)
  
  entry
}

entries <-
  content$entries %>%
  map(parse_entry)

str(entries)

## -----------------------------------------------------------------------------
boxr:::stack_rows_df(entries)

## -----------------------------------------------------------------------------
boxr:::stack_rows_tbl(entries)

## ----eval=FALSE---------------------------------------------------------------
#  library("boxr")
#  
#  box_auth()

## ----eval=FALSE---------------------------------------------------------------
#  # this works for Ian's Box account - no-one else
#  dir_id <- "123053109701"
#  
#  # returns a httr response object
#  box_api_response <- function(verb, endpoint) {
#  
#    response <-
#      httr::RETRY(
#        verb,
#        glue::glue("https://api.box.com/2.0/{endpoint}"),
#        boxr:::get_token(),
#        terminate_on = boxr:::box_terminal_http_codes()
#      )
#  
#    response
#  }
#  
#  response <- box_api_response("GET", glue::glue("folders/{dir_id}/items/"))
#  
#  response

## -----------------------------------------------------------------------------
box_content <- function(response, task = NULL) {
  
  httr::stop_for_status(response, task = task)

  text <- httr::content(response, as = "text", encoding = "UTF-8")
  
  # we may want to deviate from the defaults
  content <- jsonlite::fromJSON(text, simplifyDataFrame = FALSE)
  
  content
}

## ----eval=FALSE---------------------------------------------------------------
#  content <- box_content(response, task = "get directory listing")
#  
#  str(content)

## ----eval=FALSE---------------------------------------------------------------
#  box_parse_entries <- function(entries) {
#    purrr::map(entries, parse_entry)
#  }
#  
#  parsed <- box_parse_entries(content$entries)
#  
#  str(parsed)

## ----eval=FALSE---------------------------------------------------------------
#  tbl <- boxr:::stack_rows_tbl(parsed)
#  
#  tbl

## ----eval=FALSE---------------------------------------------------------------
#  box_dir_info <- function(dir_id) {
#  
#    response <- box_api_response("GET", glue::glue("folders/{dir_id}/items/"))
#  
#    entries <- box_content(response, task = "get directory listing")[["entries"]]
#  
#    # The above is an oversimplification. In actuality, these two functions
#    # would be combined into one function that would take care of the pagination,
#    # something like:
#    #
#    # entries <-
#    #  box_api_entries(
#    #    "GET",
#    #     endpoint = glue::glue("folders/{dir_id}/items/"),
#    #     task = "get directory listing"
#    #  )
#    #
#    # box_api_entries() would call box_api_response() and box_content()
#  
#    parsed <- box_parse_entries(entries)
#  
#    stacked <- boxr:::stack_rows_tbl(parsed)
#  
#    # not doing anything here, but box_version_history() changes some columns
#    wrangled <- stacked
#  
#    wrangled
#  }
#  
#  box_dir_info(dir_id)

## -----------------------------------------------------------------------------
boxr:::string_side_effects()

