args <- commandArgs(trailingOnly = TRUE)

parse_arg <- function(prefix, default_value){
  hit <- args[startsWith(args, paste0(prefix, "="))]
  if(length(hit) == 0) return(default_value)
  sub(paste0("^", prefix, "="), "", hit[1])
}

to_bool <- function(x){
  x <- tolower(trimws(as.character(x)))
  x %in% c("1", "true", "t", "yes", "y")
}

db_path <- parse_arg("db", file.path("data", "designplot.sqlite"))
keep_runs <- suppressWarnings(as.integer(parse_arg("keep_runs", "30")))
apply_clean <- to_bool(parse_arg("apply", "false"))
do_vacuum <- to_bool(parse_arg("vacuum", "false"))

if(is.na(keep_runs) || keep_runs < 1) stop("keep_runs 必须是 >=1 的整数")
if(!file.exists(db_path)) stop(paste0("数据库文件不存在: ", db_path))

source("designplot.R", local = TRUE)

count_rows <- function(con, table_name){
  if(!(table_name %in% DBI::dbListTables(con))) return(NA_integer_)
  q <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", quoteSqliteIdentifier(table_name)))
  as.integer(q$n[1])
}

backup_db <- function(path){
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup <- sub("\\.sqlite$", paste0("_backup_", stamp, ".sqlite"), path)
  if(identical(backup, path)) backup <- paste0(path, ".backup_", stamp)
  ok <- file.copy(path, backup, overwrite = FALSE)
  if(!isTRUE(ok)) stop("数据库备份失败")
  backup
}

con <- connectDesignplotDb(db_path)
on.exit(DBI::dbDisconnect(con), add = TRUE)
initDesignplotDb(con)

tables <- DBI::dbListTables(con)
required <- c("plan_runs", "plan_slots", "plant_assignments")
missing <- setdiff(required, tables)
if(length(missing) > 0) stop(paste0("缺少历史表: ", paste(missing, collapse = ", ")))

before_runs <- count_rows(con, "plan_runs")
before_slots <- count_rows(con, "plan_slots")
before_assign <- count_rows(con, "plant_assignments")

runs_df <- DBI::dbGetQuery(con, "SELECT plan_id, created_at FROM plan_runs ORDER BY created_at DESC, plan_id DESC")
keep_ids <- if(is.data.frame(runs_df) && nrow(runs_df) > 0) as.character(utils::head(runs_df$plan_id, keep_runs)) else character(0)
drop_ids <- if(is.data.frame(runs_df) && nrow(runs_df) > keep_runs) as.character(runs_df$plan_id[(keep_runs + 1):nrow(runs_df)]) else character(0)

cat("DB=", db_path, "\n", sep = "")
cat("模式=", if(apply_clean) "APPLY" else "DRY-RUN", "\n", sep = "")
cat("plan_runs 总数=", before_runs, "；保留=", length(keep_ids), "；待清理=", length(drop_ids), "\n", sep = "")

if(length(drop_ids) == 0){
  cat("无需清理：历史记录未超过 keep_runs。\n")
  quit(save = "no", status = 0)
}

if(!apply_clean){
  cat("示例待清理 plan_id（前10）：", paste(utils::head(drop_ids, 10), collapse = ", "), "\n", sep = "")
  cat("提示：执行清理请加 apply=true；如需收缩文件加 vacuum=true。\n")
  quit(save = "no", status = 0)
}

backup_path <- backup_db(db_path)
cat("已备份到: ", backup_path, "\n", sep = "")

drop_ids_sql <- paste(vapply(drop_ids, function(x) as.character(DBI::dbQuoteString(con, x)), character(1)), collapse = ",")

DBI::dbWithTransaction(con, {
  DBI::dbExecute(con, paste0("DELETE FROM plant_assignments WHERE plan_id IN (", drop_ids_sql, ")"))
  DBI::dbExecute(con, paste0("DELETE FROM plan_slots WHERE plan_id IN (", drop_ids_sql, ")"))
  DBI::dbExecute(con, paste0("DELETE FROM plan_runs WHERE plan_id IN (", drop_ids_sql, ")"))
})

if(do_vacuum){
  DBI::dbExecute(con, "VACUUM")
}

after_runs <- count_rows(con, "plan_runs")
after_slots <- count_rows(con, "plan_slots")
after_assign <- count_rows(con, "plant_assignments")

cat("清理完成。\n")
cat("plan_runs: ", before_runs, " -> ", after_runs, "\n", sep = "")
cat("plan_slots: ", before_slots, " -> ", after_slots, "\n", sep = "")
cat("plant_assignments: ", before_assign, " -> ", after_assign, "\n", sep = "")
