library(DBI)
library(RPostgres)
library(dplyr)

refresh_data <- function(
    addparameters_filter_pg,
    addparameters_filter_poll,
    filt.df,
    parameters,
    pollutants_groups,
    states_regions,
    act_agencies,
    categories,
    max_year,
    update_tmdls,
    years_list,
    cleaning_metrics
) {
  # ---- helpers ----
  as_df_safe <- function(x) {
    if (inherits(x, "data.table")) {
      return(as.data.frame(x))
    }
    as.data.frame(x)
  }
  
  make_config_rows <- function(name, values) {
    data.frame(
      config_name = name,
      config_value = as.character(values),
      stringsAsFactors = FALSE
    )
  }
  
  # ---- env vars ----
  pwd <- Sys.getenv("PGPASSWORD_TMDL")
  if (pwd == "") stop("PGPASSWORD_TMDL is not set or is empty")
  
  # ---- normalize inputs ----
  addparameters_filter_pg   <- as_df_safe(addparameters_filter_pg)
  addparameters_filter_poll  <- as_df_safe(addparameters_filter_poll)
  filt.df                    <- as_df_safe(filt.df)
  parameters                 <- as_df_safe(parameters)
  pollutants_groups          <- as_df_safe(pollutants_groups)
  states_regions             <- as_df_safe(states_regions)
  
  act_agencies <- unlist(act_agencies)
  categories   <- unlist(categories)
  years_list   <- unlist(years_list)
  
  # ---- config table ----
  config_df <- bind_rows(
    make_config_rows("max_year", max_year),
    make_config_rows("update.tmdls", update_tmdls),
    make_config_rows("act_agencies", act_agencies),
    make_config_rows("categories", categories),
    make_config_rows("years_list", years_list),
    make_config_rows("orig.distinct.n", cleaning_metrics$orig.distinct.n),
    make_config_rows("orig.dups.n", cleaning_metrics$orig.dups.n),
    make_config_rows("orig.dups.removed", cleaning_metrics$orig.dups.removed),
    make_config_rows("orig.n", cleaning_metrics$orig.n),
    make_config_rows("orig.noauid.n", cleaning_metrics$orig.noauid.n),
    make_config_rows("orig.nopoll.n", cleaning_metrics$orig.nopoll.n),
    make_config_rows("orig.noauidpoll.n", cleaning_metrics$orig.noauidpoll.n),
    make_config_rows("max.dups", cleaning_metrics$max.dups),
    make_config_rows("min.dups", cleaning_metrics$min.dups),
    make_config_rows("filt.df.n", cleaning_metrics$filt.df.n)
  )
  
  # ---- connect ----
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = "pgdb1.dmap-stage.aws.epa.gov",
    port = 5432,
    dbname = "tmdldb",
    user = "tmdl_user",
    password = pwd
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # ---- DB write in a transaction ----
  DBI::dbBegin(con)
  
  tryCatch({
    DBI::dbWriteTable(con, "addparameters_filter_pg", addparameters_filter_pg, overwrite = TRUE)
    DBI::dbWriteTable(con, "addparameters_filter_poll", addparameters_filter_poll, overwrite = TRUE)
    DBI::dbWriteTable(con, "filt.df", filt.df, overwrite = TRUE)
    DBI::dbWriteTable(con, "parameters", parameters, overwrite = TRUE)
    DBI::dbWriteTable(con, "pollutants_groups", pollutants_groups, overwrite = TRUE)
    DBI::dbWriteTable(con, "states_regions", states_regions, overwrite = TRUE)
    DBI::dbWriteTable(con, "app_config", config_df, overwrite = TRUE)
    
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Database refresh failed: ", e$message)
  })
  
  invisible(TRUE)
}

cleaning_metrics <- list(
  orig.distinct.n = orig.distinct.n,
  orig.dups.n = orig.dups.n,
  orig.dups.removed = orig.dups.removed,
  orig.n = orig.n,
  orig.noauid.n = orig.noauid.n,
  orig.nopoll.n = orig.nopoll.n,
  orig.noauidpoll.n = orig.noauidpoll.n,
  max.dups = max.dups,
  min.dups = min.dups,
  filt.df.n = filt.df.n
)

refresh_data(
  addparameters_filter_pg = addparameters_filter_pg,
  addparameters_filter_poll = addparameters_filter_poll,
  filt.df = filt.df,
  parameters = parameters,
  pollutants_groups = pollutants_groups,
  states_regions = states_regions,
  act_agencies = act_agencies,
  categories = categories,
  max_year = max_year,
  update_tmdls = update_tmdls,
  years_list = years_list,
  cleaning_metrics = cleaning_metrics
)