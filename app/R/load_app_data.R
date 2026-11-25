load_all_app_data <- function(force_reload = FALSE, verbose = TRUE) {
  if (!exists("EQ_cache", envir = .GlobalEnv, inherits = FALSE)) {
    assign("EQ_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  
  EQ_cache <- get("EQ_cache", envir = .GlobalEnv)
  
  if (isTRUE(EQ_cache$loaded) && !isTRUE(force_reload)) {
    if (isTRUE(verbose)) message("EQ_cache already loaded; skipping reload.")
    return(invisible(TRUE))
  }
  
  say <- function(...) if (isTRUE(verbose)) message(...)
  
  unpack_config_to_cache <- function(config_df, cache_env) {
    if (is.null(config_df) || nrow(config_df) == 0) return(invisible(FALSE))
    
    get_cfg <- function(name) {
      vals <- config_df$config_value[config_df$config_name == name]
      if (length(vals) == 0) return(NULL)
      vals
    }
    
    if (!is.null(get_cfg("max_year"))) assign("max_year", as.integer(get_cfg("max_year")[1]), envir = cache_env)
    if (!is.null(get_cfg("update.tmdls"))) assign("update.tmdls", get_cfg("update.tmdls")[1], envir = cache_env)
    if (!is.null(get_cfg("act_agencies"))) assign("act_agencies", get_cfg("act_agencies"), envir = cache_env)
    if (!is.null(get_cfg("categories"))) assign("categories", get_cfg("categories"), envir = cache_env)
    if (!is.null(get_cfg("years_list"))) assign("years_list", as.integer(get_cfg("years_list")), envir = cache_env)
    
    metric_names <- c(
      "orig.distinct.n", "orig.dups.n", "orig.dups.removed", "orig.n",
      "orig.noauid.n", "orig.nopoll.n", "orig.noauidpoll.n",
      "max.dups", "min.dups", "filt.df.n"
    )
    
    for (nm in metric_names) {
      vals <- get_cfg(nm)
      if (!is.null(vals)) {
        num <- suppressWarnings(as.numeric(vals[1]))
        if (!is.na(num)) {
          if (abs(num - round(num)) < .Machine$double.eps^0.5) {
            assign(nm, as.integer(round(num)), envir = cache_env)
          } else {
            assign(nm, num, envir = cache_env)
          }
        } else {
          assign(nm, vals[1], envir = cache_env)
        }
      }
    }
    
    invisible(TRUE)
  }
  
  assign_objects_to_cache <- function(obj, cache_env) {
    for (nm in names(obj)) {
      assign(nm, obj[[nm]], envir = cache_env)
    }
    invisible(TRUE)
  }
  
  validate_required <- function(cache_env) {
    required <- c("filt.df", "app_config")
    missing <- required[!vapply(required, exists, logical(1), envir = cache_env, inherits = FALSE)]
    if (length(missing) > 0) {
      stop("Missing required object(s) after load: ", paste(missing, collapse = ", "))
    }
    invisible(TRUE)
  }
  
  rm(list = ls(envir = EQ_cache, all.names = TRUE), envir = EQ_cache)
  
  load_from_db <- function() {
    say("Attempting DB load...")
    
    if (!requireNamespace("DBI", quietly = TRUE)) stop("Package 'DBI' is required")
    if (!requireNamespace("RPostgres", quietly = TRUE)) stop("Package 'RPostgres' is required")
    
    pwd <- Sys.getenv("PGPASSWORD_TMDL")
    if (pwd == "") stop("PGPASSWORD_TMDL is not set or is empty")
    
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      host = "pgdb1.dmap-stage.aws.epa.gov",
      port = 5432,
      dbname = "tmdldb",
      user = "tmdl_user",
      password = pwd
    )
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    obj <- list(
      addparameters_filter_pg   = DBI::dbReadTable(con, "addparameters_filter_pg"),
      addparameters_filter_poll = DBI::dbReadTable(con, "addparameters_filter_poll"),
      filt.df                   = DBI::dbReadTable(con, "filt.df"),
      parameters                = DBI::dbReadTable(con, "parameters"),
      pollutants_groups         = DBI::dbReadTable(con, "pollutants_groups"),
      states_regions            = DBI::dbReadTable(con, "states_regions"),
      app_config                = DBI::dbReadTable(con, "app_config")
    )
    
    say("DB loaded objects: ", paste(names(obj), collapse = ", "))
    assign_objects_to_cache(obj, EQ_cache)
    unpack_config_to_cache(obj$app_config, EQ_cache)
    validate_required(EQ_cache)
    
    assign("loaded", TRUE, envir = EQ_cache)
    assign("source", "db", envir = EQ_cache)
    assign("EQ_cache", EQ_cache, envir = .GlobalEnv)
    say("Successfully loaded data from DB")
    invisible(TRUE)
  }
  
  load_from_local <- function() {
    say("Attempting local load...")
    
    rdata_file <- Sys.getenv("APP_RDATA_FILE", unset = "app/data/EQ_data.Rdata")
    if (!file.exists(rdata_file)) stop("Local RData file not found: ", rdata_file)
    
    e <- new.env(parent = emptyenv())
    load(rdata_file, envir = e)
    obj <- as.list(e)
    
    say("Local loaded objects: ", paste(names(obj), collapse = ", "))
    assign_objects_to_cache(obj, EQ_cache)
    unpack_config_to_cache(obj$app_config, EQ_cache)
    validate_required(EQ_cache)
    
    assign("loaded", TRUE, envir = EQ_cache)
    assign("source", "local", envir = EQ_cache)
    assign("EQ_cache", EQ_cache, envir = .GlobalEnv)
    say("Successfully loaded data from local file")
    invisible(TRUE)
  }
  
  tryCatch(
    load_from_db(),
    error = function(db_err) {
      say("DB load failed; falling back to local. Reason: ", db_err$message)
      load_from_local()
    }
  )
}