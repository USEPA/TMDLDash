build_data_cleaning_content <- function(metrics) {
  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE)
  
  getv <- function(x, default = NA) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) return(default)
    x[1]
  }
  
  # pull values safely
  update_tmdls <- getv(metrics$update.tmdls, "")
  orig_n <- getv(metrics$orig.n, 0)
  orig_dups_n <- getv(metrics$orig.dups.n, 0)
  orig_dups_removed <- getv(metrics$orig.dups.removed, 0)
  orig_nopoll_n <- getv(metrics$orig.nopoll.n, 0)
  orig_noauid_n <- getv(metrics$orig.noauid.n, 0)
  orig_noauidpoll_n <- getv(metrics$orig.noauidpoll.n, 0)
  filt_df_n <- getv(metrics$filt.df.n, 0)
  min_dups <- getv(metrics$min.dups, 0)
  max_dups <- getv(metrics$max.dups, 0)
  orig_distinct_n <- getv(metrics$orig.distinct.n, 0)
  
  num <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 0 || is.na(x)) 0 else x
  }
  
  # ---- top text (before diagram) ----
  html_top <- HTML(paste0(
    "<h2>Data Source</h2>",
    "<p>The data used in this dashboard are reported to EPA by states, tribes, and territories in ATTAINS. ",
    "This is public information, which is available through Expert Query web services, ATTAINS web services and ATTAINS geospatial services. ",
    "Expert Query web services are used to access and update ATTAINS data for TMDL Dashboard. It is crucial to be aware of certain complexities inherent in the data. ",
    "Some of these complexities arise from multiple tables that have many-to-many relationships, resulting in what may appear to be duplicates when looking at a subset of data columns. ",
    "Other complexities arise from incomplete legacy data that were imported into the current ATTAINS database.</p>",
    
    "<p>This tool handles these complexities by employing a thorough data cleaning process (described in detail below) which removes exact duplicates, ",
    "removes incomplete records, and condenses the original data set to one row per unique combination of assessment unit identifier, pollutant, and action identifier. ",
    "This prevents double counting in situations where the same assessment unit identifier/pollutant/action identifier may be associated with more than one use or addressed parameter.</p>",
    
    "<p>Additionally, users are cautioned against direct comparisons between states due to differences in data entry, TMDL development, and assessment methodologies.</p>",
    
    "<h2>Data Import</h2>",
    "<p>Expert Query are updated on a weekly basis. The data set for this dashboard is refreshed weekly to ensure that the most recent TMDL data are displayed. ",
    "The Expert Query data set currently used by the dashboard is from the Expert Query refresh on <b>",
    update_tmdls,
    "</b>. It contains <b>",
    fmt(orig_n),
    "</b> rows.</p>",
    
    "<h2>Data Cleaning</h2>",
    "<p>The following columns were retained from the Expert Query TMDL data set for analysis: ",
    "region, state, fiscalYearEstablished, pollutant, pollutantGroup, addressedParameter, actionId, actionName, assessmentUnitId, assessmentUnitName, and planSummaryLink. ",
    "Once these columns were selected, any duplicate columns were removed. <b>",
    fmt(orig_dups_n),
    "</b> duplicate records were found, leading to the removal of <b>",
    fmt(orig_dups_removed),
    "</b> rows as the number of repeats per unique record ranged from <b>",
    fmt(min_dups),
    "</b> to <b>",
    fmt(max_dups),
    "</b>.</p>",
    
    "<p>Any records missing pollutant or assessmentUnitId were also removed. There were <b>",
    fmt(orig_nopoll_n),
    "</b> rows missing pollutant and <b>",
    fmt(orig_noauid_n),
    "</b> rows missing assessment unit identifier. Some rows were missing both.</p>",
    
    "<p>Finally, the data set was grouped by unique combination of actionId, assessmentUnitId, and pollutant and a new column, addressedParameters, was created to list all applicable addressedParameter values in one column. ",
    "This yielded a data set with <b>",
    fmt(filt_df_n),
    "</b> rows with each row representing a unique combination of assessmentUnitIdentifier, pollutant, and actionId. ",
    "This clean data set is behind all of the data summaries and visualizations in the dashboard.</p>"
  ))
  
  # ---- diagram ----
  raw_data <- num(orig_n)
  unique_rows <- num(orig_distinct_n)
  complete_rows <- num(orig_distinct_n) - num(orig_noauidpoll_n)
  clean_rows <- num(filt_df_n)
  
  if (raw_data <= 0) raw_data <- 1
  
  width_b <- 8 * unique_rows / raw_data
  width_c <- 8 * complete_rows / raw_data
  width_d <- 8 * clean_rows / raw_data
  
  height_b <- 4 * unique_rows / raw_data
  height_c <- 4 * complete_rows / raw_data
  height_d <- 4 * clean_rows / raw_data
  
  # prevent tiny/invalid boxes
  width_b <- max(2, width_b)
  width_c <- max(2, width_c)
  width_d <- max(2, width_d)
  height_b <- max(2, height_b)
  height_c <- max(2, height_c)
  height_d <- max(2, height_d)
  
  diagram <- DiagrammeR::grViz(sprintf("
digraph flowchart {
  node [fontname = Helvetica, fontsize = 22, shape = box, style = filled, fillcolor = lightblue]
  edge [fontname = Helvetica, fontsize = 22]

  A [label = 'Raw Data\\nRecords: %s', width = 8, height = 4]
  B [label = 'Unique Rows\\nRecords: %s', width = %.2f, height = %.2f]
  C [label = 'Complete Rows\\nRecords: %s', width = %.2f, height = %.2f]
  D [label = 'Clean Data \\nRecords: %s', width = %.2f, height = %.2f]

  A -> B [label = '   duplicate rows removed']
  B -> C [label = '   rows missing pollutant or assesmentUnitIdentifier removed']
  C -> D [label = '   consolidate addressedParameters for one row per actionId/assessmentUnitIdentifier/pollutant']
}
",
fmt(raw_data),
fmt(unique_rows), width_b, height_b,
fmt(complete_rows), width_c, height_c,
fmt(clean_rows), width_d, height_d
  ))
  
  html_bottom <- HTML(paste0(
    "<h2>Questions?</h2>",
    "<p>For questions about the data cleaning process or Expert Query in general, please contact the ATTAINS team at ",
    "<a href='mailto:ATTAINS@epa.gov'>ATTAINS@epa.gov</a>.</p>"
  ))
  
  list(
    html_top = html_top,
    diagram = diagram,
    html_bottom = html_bottom
  )
}