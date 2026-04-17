required_packages <- c(
  "DBI",
  "duckdb",
  "data.table",
  "readxl",
  "ggplot2",
  "networkD3",
  "htmlwidgets"
)

check_required_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      paste0(
        "Missing packages: ",
        paste(missing, collapse = ", "),
        ".\nInstall them with:\ninstall.packages(c(",
        paste(sprintf('"%s"', missing), collapse = ", "),
        "))"
      ),
      call. = FALSE
    )
  }
}

check_required_packages(required_packages)

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(data.table)
  library(readxl)
  library(ggplot2)
  library(networkD3)
  library(htmlwidgets)
})

config <- list(
  transactions_csv = Sys.getenv("TRANSACTIONS_CSV", unset = "data/transacoes.csv"),
  dictionary_xlsx = Sys.getenv("DICTIONARY_XLSX", unset = "data/bu_dictionary.xlsx"),
  output_dir = Sys.getenv("OUTPUT_DIR", unset = "outputs"),
  cut_date = as.Date(Sys.getenv("CUT_DATE", unset = "2023-12-17")),
  k_min = 2L,
  k_max = 10L,
  final_k = 4L,
  random_seed = 123L
)

dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)

required_transaction_columns <- c(
  "_data",
  "bilhete_anonimizado",
  "faixa_horaria",
  "linha_embarque"
)

clustering_vars <- c(
  "DOM_TRIPS_MEAN",
  "DOM_TRIPS_SD",
  "DOM_FIRST_HOUR_MEDIAN",
  "DOM_FIRST_HOUR_SD",
  "DOM_ACTIVE_SPAN_MEDIAN",
  "DOM_ACTIVE_SPAN_SD",
  "DOM_UNIQUE_LINES_MEDIAN"
)

assert_file_exists <- function(path, label) {
  if (!file.exists(path)) {
    stop(sprintf("%s not found: %s", label, path), call. = FALSE)
  }
}

assert_required_columns <- function(columns_present, columns_required, object_name) {
  missing <- setdiff(columns_required, columns_present)
  if (length(missing) > 0) {
    stop(
      sprintf(
        "Missing required columns in %s: %s",
        object_name,
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

impute_median_in_place <- function(dt, vars) {
  for (var in vars) {
    if (anyNA(dt[[var]])) {
      med <- median(dt[[var]], na.rm = TRUE)
      dt[is.na(get(var)), (var) := med]
    }
  }
  dt
}

prepare_feature_matrix <- function(dt, vars) {
  dt <- copy(dt)
  dt <- impute_median_in_place(dt, vars)
  dt <- dt[complete.cases(dt[, ..vars])]
  x <- scale(as.matrix(dt[, ..vars]))
  list(data = dt, matrix = x)
}

compute_elbow <- function(x, k_min, k_max, seed) {
  set.seed(seed)
  k_values <- k_min:k_max
  wss <- vapply(
    k_values,
    FUN.VALUE = numeric(1),
    FUN = function(k) {
      kmeans(x, centers = k, nstart = 10)$tot.withinss
    }
  )
  data.table(k = k_values, wss = wss)
}

save_elbow_plot <- function(elbow_dt, final_k, output_path) {
  plot <- ggplot(elbow_dt, aes(x = k, y = wss)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_vline(xintercept = final_k, linetype = "dashed", color = "red") +
    scale_x_continuous(breaks = elbow_dt$k) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Elbow Method for K-Means",
      x = "Number of clusters (k)",
      y = "Within-cluster sum of squares (WSS)"
    )

  ggsave(
    filename = output_path,
    plot = plot,
    width = 8,
    height = 5,
    dpi = 300
  )
}

load_dictionary <- function(path) {
  assert_file_exists(path, "Dictionary file")

  raw <- read_excel(path)
  raw <- as.data.table(raw)

  if (ncol(raw) < 3) {
    stop("Dictionary file must contain at least 3 columns.", call. = FALSE)
  }

  dictionary <- copy(raw[, 1:3])
  setnames(dictionary, c("code", "classification", "card_type"))
  dictionary[, code := as.character(code)]

  dictionary
}

build_profile_table <- function(con, transactions_csv, cut_date) {
  assert_file_exists(transactions_csv, "Transactions CSV")

  dbExecute(con, "DROP VIEW IF EXISTS tr")
  dbExecute(
    con,
    sprintf(
      "CREATE VIEW tr AS SELECT * FROM read_csv_auto('%s')",
      normalizePath(transactions_csv, winslash = "/", mustWork = TRUE)
    )
  )

  transaction_columns <- dbGetQuery(con, "SELECT * FROM tr LIMIT 0")
  assert_required_columns(
    columns_present = names(transaction_columns),
    columns_required = required_transaction_columns,
    object_name = "transactions CSV"
  )

  dbExecute(con, "DROP VIEW IF EXISTS tr_dt")
  dbExecute(
    con,
    sprintf(
      "
      CREATE VIEW tr_dt AS
      SELECT
        *,
        CAST(STRPTIME(CAST(_data AS VARCHAR), '%%Y%%m%%d') AS DATE) AS date_dt,
        CASE
          WHEN CAST(STRPTIME(CAST(_data AS VARCHAR), '%%Y%%m%%d') AS DATE) < DATE '%s' THEN 'pre'
          ELSE 'post'
        END AS period
      FROM tr
      WHERE _data IS NOT NULL
      ",
      cut_date
    )
  )

  dbExecute(
    con,
    "
    CREATE OR REPLACE VIEW daily_features AS
    SELECT
      bilhete_anonimizado AS ticket_id,
      period,
      date_dt,
      COUNT(*) AS n_validations,
      MIN(faixa_horaria) AS first_hour,
      MAX(faixa_horaria) AS last_hour,
      MAX(faixa_horaria) - MIN(faixa_horaria) AS active_span,
      COUNT(DISTINCT linha_embarque) AS unique_lines_day
    FROM tr_dt
    GROUP BY ticket_id, period, date_dt
    "
  )

  dbExecute(
    con,
    "
    CREATE OR REPLACE VIEW profile AS
    SELECT
      ticket_id,
      period,
      COUNT(*) AS active_sundays,
      AVG(n_validations) AS DOM_TRIPS_MEAN,
      STDDEV_POP(n_validations) AS DOM_TRIPS_SD,
      MEDIAN(first_hour) AS DOM_FIRST_HOUR_MEDIAN,
      STDDEV_POP(first_hour) AS DOM_FIRST_HOUR_SD,
      MEDIAN(active_span) AS DOM_ACTIVE_SPAN_MEDIAN,
      STDDEV_POP(active_span) AS DOM_ACTIVE_SPAN_SD,
      MEDIAN(unique_lines_day) AS DOM_UNIQUE_LINES_MEDIAN
    FROM daily_features
    GROUP BY ticket_id, period
    "
  )

  as.data.table(dbGetQuery(con, "SELECT * FROM profile"))
}

build_sankey_html <- function(df, vars, final_k, seed, output_path) {
  transition_data <- copy(df)
  transition_data <- transition_data[period %in% c("pre", "post")]
  transition_data <- impute_median_in_place(transition_data, vars)
  transition_data <- transition_data[complete.cases(transition_data[, ..vars])]

  if (nrow(transition_data) == 0) {
    warning("No complete records available for transition analysis.")
    return(invisible(NULL))
  }

  x_all <- scale(as.matrix(transition_data[, ..vars]))

  set.seed(seed)
  km_all <- kmeans(
    x_all,
    centers = final_k,
    nstart = 20,
    iter.max = 300
  )

  transition_data[, cluster := km_all$cluster]

  wide <- dcast(
    transition_data[, .(ticket_id, period, cluster)],
    ticket_id ~ period,
    value.var = "cluster"
  )

  wide <- wide[!is.na(pre) & !is.na(post)]

  if (nrow(wide) == 0) {
    warning("No users with both pre and post periods were found. Sankey was not generated.")
    return(invisible(NULL))
  }

  links_dt <- wide[, .(value = .N), by = .(pre, post)]

  pre_labels <- paste0("PRE: ", sort(unique(links_dt$pre)))
  post_labels <- paste0("POST: ", sort(unique(links_dt$post)))

  nodes <- data.table(name = c(pre_labels, post_labels))

  pre_map <- setNames(seq_along(pre_labels) - 1L, pre_labels)
  post_map <- setNames(length(pre_labels) + seq_along(post_labels) - 1L, post_labels)

  links <- data.table(
    source = unname(pre_map[paste0("PRE: ", links_dt$pre)]),
    target = unname(post_map[paste0("POST: ", links_dt$post)]),
    value = links_dt$value
  )

  sankey <- sankeyNetwork(
    Links = as.data.frame(links),
    Nodes = as.data.frame(nodes),
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "name",
    fontSize = 12,
    nodeWidth = 30
  )

  saveWidget(sankey, file = output_path, selfcontained = TRUE)
}

run_pipeline <- function(config) {
  dictionary <- load_dictionary(config$dictionary_xlsx)

  con <- dbConnect(duckdb::duckdb())

  on.exit({
    try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)
  }, add = TRUE)

  profile_df <- build_profile_table(
    con = con,
    transactions_csv = config$transactions_csv,
    cut_date = config$cut_date
  )

  profile_df[, ticket_id := as.character(ticket_id)]

  profile_df <- merge(
    profile_df,
    dictionary,
    by.x = "ticket_id",
    by.y = "code",
    all.x = TRUE
  )

  post_df <- profile_df[period == "post"]
  prepared <- prepare_feature_matrix(post_df, clustering_vars)

  if (nrow(prepared$data) == 0) {
    stop("No complete records available for post-period clustering.", call. = FALSE)
  }

  elbow_dt <- compute_elbow(
    x = prepared$matrix,
    k_min = config$k_min,
    k_max = config$k_max,
    seed = config$random_seed
  )

  save_elbow_plot(
    elbow_dt = elbow_dt,
    final_k = config$final_k,
    output_path = file.path(config$output_dir, "elbow_plot.png")
  )

  set.seed(config$random_seed)
  km <- kmeans(
    prepared$matrix,
    centers = config$final_k,
    nstart = 20,
    iter.max = 300
  )

  clustered_post <- copy(prepared$data)
  clustered_post[, cluster_kmeans := km$cluster]

  cluster_profile <- clustered_post[
    ,
    lapply(.SD, median, na.rm = TRUE),
    by = cluster_kmeans,
    .SDcols = clustering_vars
  ]

  fwrite(clustered_post, file.path(config$output_dir, "post_period_clusters_k4.csv"))
  fwrite(cluster_profile, file.path(config$output_dir, "cluster_profile_medians.csv"))
  fwrite(elbow_dt, file.path(config$output_dir, "elbow_wss.csv"))

  build_sankey_html(
    df = profile_df,
    vars = clustering_vars,
    final_k = config$final_k,
    seed = config$random_seed,
    output_path = file.path(config$output_dir, "pre_post_cluster_transition_sankey.html")
  )

  cat("\nPipeline completed successfully.\n")
  cat(sprintf("Outputs saved to: %s\n", normalizePath(config$output_dir, winslash = "/")))

  invisible(list(
    profile_df = profile_df,
    clustered_post = clustered_post,
    cluster_profile = cluster_profile,
    elbow = elbow_dt
  ))
}

run_pipeline(config)
