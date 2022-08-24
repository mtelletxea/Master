##
## FUNCIONES MODIFICADAS DEL PAQUETE rredlist
## Fecha: 03/08/2022
## 

#### Funcines escritas ####
ct <- function(l) Filter(Negate(is.null), l)

rredlist_ua <- function() {
  versions <- c(
    paste0("r-curl/", utils::packageVersion("curl")),
    paste0("crul/", utils::packageVersion("crul")),
    sprintf("rOpenSci(rredlist/%s)", utils::packageVersion("rredlist"))
  )
  paste0(versions, collapse = " ")
}

rr_GET <- function(path, key, ...){
  cli <- crul::HttpClient$new(
    url = file.path(rr_base(), path),
    opts = list(useragent = rredlist_ua())
  )
  temp <- cli$get(query = list(token = check_key(key)), ...)
  temp$raise_for_status()
  x <- temp$parse("UTF-8")
  err_catcher(x)
  return(x)
}

err_catcher <- function(x) {
  xx <- jsonlite::fromJSON(x)
  if (any(vapply(c("message", "error"), function(z) z %in% names(xx),
                 logical(1)))) {
    stop(xx[[1]], call. = FALSE)
  }
}

rl_parse <- function(x, parse) {
  jsonlite::fromJSON(x, parse)
}

check_key <- function(x){
  tmp <- if (is.null(x)) Sys.getenv("IUCN_REDLIST_KEY", "") else x
  if (tmp == "") {
    getOption("iucn_redlist_key", stop("need an API key for Red List data",
                                       call. = FALSE))
  } else {
    tmp
  }
}

rr_base <- function() "https://apiv3.iucnredlist.org/api/v3"

space <- function(x) gsub("\\s", "%20", x)

assert_is <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

assert_n <- function(x, n) {
  if (!is.null(x)) {
    if (!length(x) == n) {
      stop(deparse(substitute(x)), " must be length ", n, call. = FALSE)
    }
  }
}

assert_not_na <- function(x) {
  if (!is.null(x)) {
    if (any(is.na(x))) {
      stop(deparse(substitute(x)), " must not be NA", call. = FALSE)
    }
  }
}

nir <- function(path_name, path_id, name = NULL, id = NULL, region = NULL) {
  
  # only one of name OR id
  stopifnot(xor(!is.null(name), !is.null(id)))
  
  # check types
  assert_is(name, 'character')
  assert_is(id, c('integer', 'numeric'))
  assert_is(region, 'character')
  
  # can't be NA
  assert_not_na(name)
  assert_not_na(id)
  assert_not_na(region)
  
  # check lengths - only length 1 allowed for all
  assert_n(name, 1)
  assert_n(id, 1)
  assert_n(region, 1)
  
  # construct path
  path <- if (!is.null(name)) {
    file.path(path_name, space(name))
  } else {
    file.path(path_id, id)
  }
  if (!is.null(region)) {
    path <- file.path(path, "region", space(region))
  }
  
  return(path)
}

#' Get species
#'
#' @export
#' @param page (integer/numeric) Page to get. Default: 0. you can 
#' get up to 10,000 records per page. Paging is required because
#' it's too much burden on a server to just "get all the data"
#' in one request
#' @param region (character) Region of interest.
#' @param all (logical) to get all results or not. Default: `FALSE`. 
#' this means we do the paging internally for you. result is a list
#' of results, so you have to bind them together yourself into 
#' a data.frame, see example
#' @param quiet (logical) give progress for download or not. 
#' Default: `FALSE` (that is, give progress). ignored if 
#' `all = FALSE` 
#' @template all
#' @examples \dontrun{
#' rl_sp(page = 3)
#' 
#' # get all results
#' out <- rl_sp(region = "europe", all = TRUE)
#' length(out)
#' vapply(out, "[[", 1, "count")
#' all_df <- do.call(rbind, lapply(out, "[[", "result"))
#' head(all_df)
#' NROW(all_df)
#' }
library(rredlist)

rl_sp_region <- function(page = 0, region = NULL, key = NULL, parse = TRUE, all = FALSE, 
                  quiet = FALSE, ...) {
  
  assert_is(parse, 'logical')
  assert_is(all, 'logical')
  
  res <- rl_sp_region_(page, region, key, all, quiet, ...)
  if (all) lapply(res, rl_parse, parse = parse) else rl_parse(res, parse)
}

#' @export
#' @rdname rl_sp_region
rl_sp_region_ <- function(page, region = NULL, key = NULL, all = FALSE, quiet = FALSE, ...) {
  assert_is(key, 'character')
  assert_is(region, 'character')
  assert_is(page, c('integer', 'numeric'))
  assert_n(page, 1)
  
  if (all) {
    out <- list()
    done <- FALSE
    i <- 0
    page <- 0
    while (!done) {
      if (!quiet) cat(".")
      i <- i + 1
      tmp <- rr_GET(file.path("species/region", region, "page", page), key, ...)
      if (jsonlite::fromJSON(tmp, FALSE)$count == 0) {
        done <- TRUE
      } else {
        out[[i]] <- tmp
        page <- page + 1
      }
    }
    if (!quiet) cat("\n")
    return(out)
  } else {
    rr_GET(file.path("species/region", region, "page", page), key, ...)
  }
}

#### Guardar funciones en un workspaces ####
save(assert_is, assert_n, assert_not_na, check_key, ct, err_catcher, nir, rl_parse, rl_sp_region, rl_sp_region_, rr_base, rr_GET, rredlist_ua, space, file = "./Workspaces/species_region_functions.RData")
