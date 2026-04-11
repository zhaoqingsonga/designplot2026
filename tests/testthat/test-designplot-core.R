library(testthat)

# 避免 source designplot.R 时自动启动 Shiny
SOURCED_AS_LIB <- TRUE
source(file.path("..", "..", "designplot.R"))

test_that("designPlot 默认参数输出稳定", {
  res <- designPlot()

  expect_true(is.matrix(res))
  expect_equal(ncol(res), 10 + STAT_COL_COUNT)  # 默认 y=10
  expect_equal(tail(colnames(res), STAT_COL_COUNT), STAT_COL_NAMES)
})

test_that("design_from_left=FALSE 时镜像逻辑稳定", {
  left  <- designPlot(design_from_left = TRUE)
  right <- designPlot(design_from_left = FALSE)

  # 镜像后内容可不同，但结构应一致
  expect_equal(dim(left), dim(right))
  expect_equal(colnames(left), colnames(right))
})

test_that("plant_from_left 改变蛇形编号方向但不改变结构", {
  a <- designPlot(plant_from_left = TRUE)
  b <- designPlot(plant_from_left = FALSE)

  expect_equal(dim(a), dim(b))
  expect_equal(colnames(a), colnames(b))
  expect_false(identical(a, b))
})

test_that("p_a=NULL 路径可运行", {
  res <- designPlot(p_a = NULL)

  expect_true(is.matrix(res))
  expect_equal(tail(colnames(res), STAT_COL_COUNT), STAT_COL_NAMES)
})

test_that("modibridges 支持压缩语法", {
  expect_equal(modibridges("6/3,10"), c(6, 6, 6, 10))
  expect_equal(modibridges("5"), 5)
})

test_that("modibridges 非法输入会报错", {
  expect_error(modibridges("6/a"), "格式错误")
  expect_error(modibridges(""), "不能为空")
})

test_that("getWaterLane 非法 token 会报错", {
  expect_error(getWaterLane("w/x/w"), "田间布局格式错误")
  expect_error(getWaterLane(""), "田间布局不能为空")
})

test_that("parseParamTxt 遇到不支持键会报错", {
  tf <- tempfile(fileext = ".txt")
  writeLines(c("bad_key=1"), tf)
  expect_error(parseParamTxt(tf), "参数键不支持")
})

test_that("designPlot blocks 为0时报错", {
  expect_error(designPlot(blocks = 0), "blocks 必须是大于0的整数")
})

test_that("designPlot 对参数边界进行防御式校验", {
  expect_error(designPlot(y = 0), "总列数必须是大于0的整数")
  expect_error(designPlot(subg = 0), "分组大小必须是大于0的整数")
  expect_error(designPlot(ww = -1), "必须为非负数")
  expect_error(designPlot(protected_columns = c(999)), "保护列索引必须是")
  expect_error(designPlot(protected_blocks = c(999)), "横向保护行必须是")
  expect_error(designPlot(p_a = c(10, 5, 1, 2)), "起始位置必须小于终止位置")
  expect_error(designPlot(p_a = c(10, 20, 5, 1)), "起始行不能大于终止行")
})

test_that("designPlot 支持无横向保护行", {
  res <- designPlot(protected_blocks = numeric(0))
  expect_true(is.matrix(res))
  expect_true(nrow(res) > 0)
})

test_that("parseDesignInputParams 可正确解析合法输入", {
  parsed <- parseDesignInputParams("1,3,5", "23,25,3,5")
  expect_equal(parsed$protected_blocks, c(1, 3, 5))
  expect_equal(parsed$p_a, c(23, 25, 3, 5))
})

test_that("parseDesignInputParams 支持空横向保护行", {
  parsed <- parseDesignInputParams("", "")
  expect_equal(length(parsed$protected_blocks), 0)
  expect_null(parsed$p_a)
})

test_that("parseDesignInputParams 对非法输入报错", {
  expect_error(parseDesignInputParams("a,2", ""), "横向保护行必须为数字")
  expect_error(parseDesignInputParams("1", "1,2,3"), "参数个数必须是")
})

test_that("parsePlantingColumnRange 可正确解析并校验列范围", {
  parsed <- parsePlantingColumnRange("2", "4", 6)
  expect_equal(parsed$start_col, 2)
  expect_equal(parsed$end_col, 4)

  defaulted <- parsePlantingColumnRange("", "", 6)
  expect_equal(defaulted$start_col, 1)
  expect_equal(defaulted$end_col, 6)

  defaulted_na <- parsePlantingColumnRange(1, NA, 6)
  expect_equal(defaulted_na$start_col, 1)
  expect_equal(defaulted_na$end_col, 6)

  expect_error(parsePlantingColumnRange("5", "2", 6), "起始列不能大于终止列")
  expect_error(parsePlantingColumnRange("0", "2", 6), "起始列必须在 1 到 6 之间")
})

test_that("sanitizePlantingColumnRange 会对空值和越界值自动纠偏", {
  sanitized_blank <- sanitizePlantingColumnRange(1, NA, 6)
  expect_equal(sanitized_blank$start_col, 1)
  expect_equal(sanitized_blank$end_col, 6)

  sanitized_overflow <- sanitizePlantingColumnRange(2, 99, 6)
  expect_equal(sanitized_overflow$start_col, 2)
  expect_equal(sanitized_overflow$end_col, 6)
})

test_that("parsePlantingStartRow 和 sanitizePlantingStartRow 可正确处理起始行号", {
  expect_equal(parsePlantingStartRow("3"), 3)
  expect_equal(parsePlantingStartRow(NA), 1)
  expect_error(parsePlantingStartRow("0"), "种植起始行号必须为大于等于 1 的整数")

  expect_equal(sanitizePlantingStartRow(NA), 1)
  expect_equal(sanitizePlantingStartRow("4.8"), 4)
})

test_that("parsePlantingCoordinateRange 支持 n,n / n, / ,n / n 规则", {
  parsed_default <- parsePlantingCoordinateRange("", "", 12, 8)
  expect_equal(parsed_default$start_row, 1)
  expect_equal(parsed_default$start_col, 1)
  expect_equal(parsed_default$end_row, 12)
  expect_equal(parsed_default$end_col, 8)

  parsed_full <- parsePlantingCoordinateRange("2,3", "10,7", 12, 8)
  expect_equal(parsed_full$start_row, 2)
  expect_equal(parsed_full$start_col, 3)
  expect_equal(parsed_full$end_row, 10)
  expect_equal(parsed_full$end_col, 7)

  parsed_row_only <- parsePlantingCoordinateRange("4", "9", 12, 8)
  expect_equal(parsed_row_only$start_row, 4)
  expect_equal(parsed_row_only$start_col, 1)
  expect_equal(parsed_row_only$end_row, 9)
  expect_equal(parsed_row_only$end_col, 8)

  parsed_col_only <- parsePlantingCoordinateRange(",3", ",6", 12, 8)
  expect_equal(parsed_col_only$start_row, 1)
  expect_equal(parsed_col_only$start_col, 3)
  expect_equal(parsed_col_only$end_row, 12)
  expect_equal(parsed_col_only$end_col, 6)

  expect_error(parsePlantingCoordinateRange("2,3,4", "", 12, 8), "格式错误")
  expect_error(parsePlantingCoordinateRange("0,1", "", 12, 8), "起始行")
  expect_error(parsePlantingCoordinateRange("", "2,9", 12, 8), "终止列")
})

test_that("makelistWithPos 和 plant 支持指定种植列范围", {
  plan_matrix <- designPlot(
    blocks = 2,
    y = 6,
    bridges = c(6, 6),
    protected_blocks = c(1),
    water_columns = integer(),
    lane_columns = integer(),
    protected_columns = integer(),
    p_a = NULL
  )
  seed_df <- data.frame(num = c(1, 2), name = c("材料A", "材料B"), re = c(2, 2))
  seed_list <- makelist(seed_df)

  mapped <- makelistWithPos(seed_df, plan_matrix, start_col = 2, end_col = 4)
  planted <- plant(plan_matrix, seed_list, start_col = 2, end_col = 4)
  mapped_non_na <- mapped[!is.na(mapped$field_col), , drop = FALSE]

  expect_gt(nrow(mapped_non_na), 0)
  expect_true(all(mapped_non_na$field_col >= 2 & mapped_non_na$field_col <= 4))
  planted_data <- as.matrix(planted[, 1:(ncol(planted) - STAT_COL_COUNT), drop = FALSE])
  planted_match <- matrix(grepl("材料", planted_data), nrow = nrow(planted_data))
  planted_positions <- which(planted_match, arr.ind = TRUE)
  expect_true(nrow(planted_positions) > 0)
  expect_true(all(planted_positions[, 2] >= 2 & planted_positions[, 2] <= 4))
})

test_that("makelistWithPos 和 plant 支持指定种植起始行号", {
  plan_matrix <- designPlot(
    blocks = 4,
    y = 6,
    bridges = c(6, 6, 6, 6),
    protected_blocks = c(1),
    water_columns = integer(),
    lane_columns = integer(),
    protected_columns = integer(),
    p_a = NULL
  )
  seed_df <- data.frame(num = c(1, 2), name = c("材料A", "材料B"), re = c(2, 2))
  seed_list <- makelist(seed_df)

  mapped <- makelistWithPos(seed_df, plan_matrix, start_row = 3, start_col = 1, end_col = 6)
  planted <- plant(plan_matrix, seed_list, start_row = 3, start_col = 1, end_col = 6)
  mapped_non_na <- mapped[!is.na(mapped$field_row), , drop = FALSE]

  expect_gt(nrow(mapped_non_na), 0)
  expect_true(all(mapped_non_na$field_row >= 3))
  planted_data <- as.matrix(planted[, 1:(ncol(planted) - STAT_COL_COUNT), drop = FALSE])
  planted_match <- matrix(grepl("材料", planted_data), nrow = nrow(planted_data))
  planted_positions <- which(planted_match, arr.ind = TRUE)
  planted_row_no <- suppressWarnings(as.numeric(planted[planted_positions[, 1], ncol(planted) - STAT_COL_OFFSET_ROWNO]))
  expect_true(all(planted_row_no >= 3))
})

test_that("plant 支持同时按起止行列限制种植区域", {
  plan_matrix <- designPlot(
    blocks = 4,
    y = 8,
    bridges = c(6, 6, 6, 6),
    protected_blocks = c(1),
    water_columns = integer(),
    lane_columns = integer(),
    protected_columns = integer(),
    p_a = NULL
  )
  seed_df <- data.frame(num = c(1), name = c("材料A"), re = c(20))
  seed_list <- makelist(seed_df)

  planted <- plant(plan_matrix, seed_list, start_row = 3, start_col = 2, end_row = 6, end_col = 5)
  planted_data <- as.matrix(planted[, 1:(ncol(planted) - STAT_COL_COUNT), drop = FALSE])
  planted_match <- matrix(grepl("材料", planted_data), nrow = nrow(planted_data))
  planted_positions <- which(planted_match, arr.ind = TRUE)

  expect_true(nrow(planted_positions) > 0)
  planted_row_no <- suppressWarnings(as.numeric(planted[planted_positions[, 1], ncol(planted) - STAT_COL_OFFSET_ROWNO]))
  expect_true(all(planted_row_no >= 3 & planted_row_no <= 6))
  expect_true(all(planted_positions[, 2] >= 2 & planted_positions[, 2] <= 5))
})

test_that("fillUnplantedWithMaterial 仅替换区域内纯数字未种位", {
  base_matrix <- matrix(c(
    1, 2, -8,
    3, 4, 5
  ), nrow = 2, byrow = TRUE)
  stat_cols <- cbind(c(0.5, 0.6), c(0, 0), c(2, 1), c(1, 2))
  base_matrix <- cbind(base_matrix, stat_cols)
  colnames(base_matrix) <- c(paste0("V", 1:3), STAT_COL_NAMES)

  planted_matrix <- base_matrix
  planted_matrix[1, 1] <- "材料甲|1|1"
  planted_matrix[1, 2] <- "2"
  planted_matrix[2, 1] <- "3"
  planted_matrix[2, 2] <- "材料乙|1|4"
  planted_matrix[2, 3] <- "5"

  filled <- fillUnplantedWithMaterial(planted_matrix, "补种X", start_row = 1, start_col = 2, end_row = 2, end_col = 3)
  updated <- filled$matrix

  expect_equal(filled$filled_count, 2L)
  expect_true(grepl("^补种X\\|1\\|2$", as.character(updated[1, 2])))
  expect_true(grepl("^补种X\\|1\\|5$", as.character(updated[2, 3])))
  # 非纯数字或范围外不应被替换
  expect_equal(as.character(updated[1, 1]), "材料甲|1|1")
  expect_equal(as.character(updated[2, 1]), "3")
  expect_equal(as.character(updated[2, 2]), "材料乙|1|4")
})

test_that("种植容量不足时只会部分填入矩阵，assignment 提取不会写入 NA seq_no", {
  plan_matrix <- matrix(c(
    1, 2, -8,
    3, 4, 5
  ), nrow = 2, byrow = TRUE)
  stat_cols <- cbind(c(0.5, 0.6), c(0, 0), c(2, 1), c(1, 2))
  plan_matrix <- cbind(plan_matrix, stat_cols)
  colnames(plan_matrix) <- c(paste0("V", 1:3), STAT_COL_NAMES)

  seed_df <- data.frame(
    num = c("A", "B", "C"),
    name = c("材料A", "材料B", "材料C"),
    re = c(2, 2, 2),
    stringsAsFactors = FALSE
  )
  planted <- plant(plan_matrix, makelist(seed_df), start_row = 1, start_col = 1, end_row = 1, end_col = 2)
  assignments <- extractPlantAssignments(planted, plan_matrix, "测试试验")

  expect_true(nrow(assignments) > 0)
  expect_true(all(!is.na(assignments$seq_no)))
  expect_true(all(assignments$seq_no > 0))
})

test_that("重构后默认参数结果与基准一致", {
  baseline_path <- file.path("..", "fixtures", "baseline-default.rds")

  skip_if_not(file.exists(baseline_path), "baseline-default.rds 不存在，请先生成基准文件")
  baseline <- readRDS(baseline_path)
  current  <- designPlot()

  expect_identical(current, baseline)
})

test_that("SQLite 持久化可保存规划位点与种植分配", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  plan_matrix <- designPlot(blocks = 2, y = 4, bridges = c(6, 6), protected_blocks = c(1), water_columns = integer(), lane_columns = integer(), protected_columns = integer(), p_a = NULL)
  seed_df <- data.frame(num = c(1, 2), name = c("材料A", "材料B"), re = c(2, 2))
  seed_list <- makelist(seed_df)
  planted_matrix <- plant(plan_matrix, seed_list)
  db_path <- tempfile(fileext = ".sqlite")

  plan_result <- savePlanToSqlite(
    plan_matrix = plan_matrix,
    experiment_name = "test_experiment",
    db_path = db_path,
    plan_id = "plan_test_001",
    metadata = list(field_layout = "4", bridge_layout = "6,6", row_gap = 0, group_rows = 1)
  )
  assignment_result <- saveAssignmentsToSqlite(
    plan_id = plan_result$plan_id,
    planted_matrix = planted_matrix,
    plan_matrix = plan_matrix,
    experiment_name = "test_experiment",
    db_path = db_path
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  slots <- DBI::dbReadTable(con, "plan_slots")
  assignments <- DBI::dbReadTable(con, "plant_assignments")

  expect_equal(plan_result$plan_id, "plan_test_001")
  expect_gt(nrow(slots), 0)
  expect_true(all(c("seq_no", "field_row_no", "field_col_no") %in% names(slots)))
  expect_equal(assignment_result$assignment_count, nrow(assignments))
  expect_true(all(c("experiment_name", "material_name", "material_subrow_no") %in% names(assignments)))
})

test_that("旧种植策略会把种植结果写回所选地块表", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".sqlite")
  base_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
  base_matrix <- cbind(base_matrix, c(0, 0), c(10, 10), c(1, 2), c(1, 2))
  colnames(base_matrix) <- c(paste0("V", 1:2), STAT_COL_NAMES)

  savePlantTable("测试地块A", base_matrix, db_path)

  seed_df <- data.frame(num = c("1", "2"), name = c("材料A", "材料B"), re = c(1, 1), stringsAsFactors = FALSE)
  planted_matrix <- plant(base_matrix, makelist(seed_df), start_row = 1, start_col = 1, end_col = 2)

  plan_result <- savePlanToSqlite(
    plan_matrix = base_matrix,
    experiment_name = "试验A",
    db_path = db_path,
    plan_id = "plan_preview_guard",
    metadata = list(field_layout = "2", bridge_layout = "10,10", row_gap = 0, group_rows = 1)
  )
  saveAssignmentsToSqlite(
    plan_id = plan_result$plan_id,
    planted_matrix = planted_matrix,
    plan_matrix = base_matrix,
    experiment_name = "试验A",
    db_path = db_path
  )
  savePlantTable("测试地块A", planted_matrix, db_path)

  reloaded_base <- readPlantTable("测试地块A.plant", db_path)
  assignments <- readTableFromSqlite("plant_assignments", db_path)

  expect_equal(as.character(reloaded_base), as.character(planted_matrix))
  expect_equal(nrow(assignments), 2)
  expect_true(all(c("material_name", "field_row_no", "field_col_no") %in% names(assignments)))
})

test_that("可根据种植结果生成标准 .sow 播种表", {
  base_matrix <- matrix(c(
    1, 2,
    3, -8
  ), nrow = 2, byrow = TRUE)
  base_matrix <- cbind(base_matrix, c(0.5, 0.6), c(0, 0), c(5, 6), c(1, 2))
  colnames(base_matrix) <- c(paste0("V", 1:2), STAT_COL_NAMES)
  planted_matrix <- base_matrix
  planted_matrix[1, 1] <- "材料甲|1|1"
  planted_matrix[2, 1] <- "材料乙|1|3"

  sow_df <- buildSowTable("测试地块A", base_matrix, planted_matrix)

  expect_equal(names(sow_df), c("Location", "ID", "Y", "X", "XiaoQuChangDu", "GuoDaoKuanDu", "HangJv", "XiaoQuLiShu"))
  expect_equal(nrow(sow_df), 3)
  expect_equal(as.character(sow_df$Location), c("测试地块A", "测试地块A", "测试地块A"))
  expect_equal(as.character(sow_df$ID), c("材料甲", "2", "材料乙"))
  expect_equal(as.integer(sow_df$Y), c(5, 5, 6))
  expect_equal(as.integer(sow_df$X), c(1, 2, 1))
  expect_equal(as.numeric(sow_df$XiaoQuChangDu), c(50, 50, 60))
  expect_equal(as.numeric(sow_df$GuoDaoKuanDu), c(100, 100, 100))
  expect_equal(as.numeric(sow_df$HangJv), c(40, 40, 40))
  expect_equal(as.numeric(sow_df$XiaoQuLiShu), c(6.5, 6.5, 7.8))
})

test_that("播种表生成不会遗漏仅在种植矩阵出现的条目", {
  base_matrix <- matrix(c(
    NA, NA,
    3, NA
  ), nrow = 2, byrow = TRUE)
  base_matrix <- cbind(base_matrix, c(0.5, 0.6), c(0, 0), c(8, 9), c(1, 2))
  colnames(base_matrix) <- c(paste0("V", 1:2), STAT_COL_NAMES)

  planted_matrix <- base_matrix
  planted_matrix[1, 1] <- "材料甲|1|1"
  planted_matrix[2, 1] <- "材料乙|1|3"

  sow_df <- buildSowTable("测试地块A", base_matrix, planted_matrix)

  expect_equal(nrow(sow_df), 2)
  expect_equal(as.character(sow_df$ID), c("材料甲", "材料乙"))
  expect_equal(as.integer(sow_df$Y), c(8, 9))
  expect_equal(as.integer(sow_df$X), c(1, 1))
})

test_that(".sow 播种表可按地块名持久化读写", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".sqlite")
  sow_df <- data.frame(
    Location = c("测试地块A"),
    ID = c("S1"),
    Y = c(8L),
    X = c(3L),
    XiaoQuChangDu = c(50),
    GuoDaoKuanDu = c(100),
    HangJv = c(40),
    XiaoQuLiShu = c(6.5),
    stringsAsFactors = FALSE
  )

  save_result <- saveSowTable("测试地块A", sow_df, db_path)
  loaded <- readSowTable("测试地块A.sow", db_path)

  expect_equal(save_result$table_name, "测试地块A.sow")
  expect_equal(nrow(loaded), 1)
  expect_equal(as.character(loaded$Location), "测试地块A")
  expect_equal(as.character(loaded$ID), "S1")
  expect_equal(as.integer(loaded$Y), 8)
  expect_equal(as.integer(loaded$X), 3)
})

test_that("可枚举多个地块的 .sow 播种表", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".sqlite")
  saveSowTable("地块A", data.frame(Location = "地块A", ID = "1", Y = 1L, X = 1L, XiaoQuChangDu = 50, GuoDaoKuanDu = 100, HangJv = 40, XiaoQuLiShu = 6.5), db_path)
  saveSowTable("地块B", data.frame(Location = "地块B", ID = "2", Y = 2L, X = 2L, XiaoQuChangDu = 60, GuoDaoKuanDu = 100, HangJv = 40, XiaoQuLiShu = 7.8), db_path)

  sow_tables <- listSowTables(db_path)

  expect_equal(sort(as.character(sow_tables$table_name)), c("地块A.sow", "地块B.sow"))
  expect_equal(sort(as.character(sow_tables$field_name)), c("地块A", "地块B"))
})

test_that("同一试验同一地块默认不可重复种植（可覆盖）", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".sqlite")
  planting_df <- data.frame(
    fieldid = c("F1", "F2"),
    id = c("1", "2"),
    stageid = c("S1", "S2"),
    name = c("X", "Y"),
    former_stageid = c("FX", "FY"),
    source = c("SRC-X", "SRC-Y"),
    code = c("CX", "CY"),
    rp = c("1", "1"),
    rows = c(1, 2),
    line_number = c("1", "2"),
    stringsAsFactors = FALSE
  )

  saved <- saveExperimentWithRecords("试验防重A", planting_df, db_path)
  exp_id <- saved$experiment_id
  plant_table <- "地块A.plant"

  expect_false(hasExperimentPlantRun(exp_id, plant_table, db_path))

  saveExperimentPlantRun(
    experiment_id = exp_id,
    plant_table_name = plant_table,
    sow_table_name = "地块A.sow",
    plan_id = "plan_1",
    db_path = db_path,
    overwrite = FALSE
  )
  expect_true(hasExperimentPlantRun(exp_id, plant_table, db_path))

  expect_error(
    saveExperimentPlantRun(
      experiment_id = exp_id,
      plant_table_name = plant_table,
      sow_table_name = "地块A.sow",
      plan_id = "plan_2",
      db_path = db_path,
      overwrite = FALSE
    ),
    "已在当前地块执行过种植"
  )

  expect_silent(
    saveExperimentPlantRun(
      experiment_id = exp_id,
      plant_table_name = plant_table,
      sow_table_name = "地块A.sow",
      plan_id = "plan_2",
      db_path = db_path,
      overwrite = TRUE
    )
  )
})

test_that("重置地块后可自动解除该地块防重复锁", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".sqlite")
  planting_df <- data.frame(
    fieldid = c("F1"),
    id = c("1"),
    stageid = c("S1"),
    name = c("X"),
    former_stageid = c("FX"),
    source = c("SRC-X"),
    code = c("CX"),
    rp = c("1"),
    rows = c(1),
    line_number = c("1"),
    stringsAsFactors = FALSE
  )

  saved <- saveExperimentWithRecords("试验防重B", planting_df, db_path)
  exp_id <- saved$experiment_id
  plant_table <- "地块B.plant"

  saveExperimentPlantRun(
    experiment_id = exp_id,
    plant_table_name = plant_table,
    sow_table_name = "地块B.sow",
    plan_id = "plan_1",
    db_path = db_path,
    overwrite = FALSE
  )
  expect_true(hasExperimentPlantRun(exp_id, plant_table, db_path))

  cleared <- clearExperimentPlantRunByTable(plant_table, db_path)
  expect_gte(cleared, 1)
  expect_false(hasExperimentPlantRun(exp_id, plant_table, db_path))
})

test_that("computeSowProgress 正确统计已种与纯数字未种", {
  base_matrix <- matrix(c(
    1, 2,
    3, 0
  ), nrow = 2, byrow = TRUE)
  stat_cols <- cbind(c(0.5, 0.6), c(0, 0), c(2, 1), c(1, 2))
  base_matrix <- cbind(base_matrix, stat_cols)
  colnames(base_matrix) <- c(paste0("V", 1:2), STAT_COL_NAMES)

  sow_df <- data.frame(
    Location = c("A", "A", "A", "A"),
    ID = c("材料甲", "2", "材料乙", "3"),
    Y = c(1, 1, 2, 2),
    X = c(1, 2, 1, 2),
    stringsAsFactors = FALSE
  )

  progress <- computeSowProgress(base_matrix, sow_df)
  expect_equal(progress$expected_count, 3)
  expect_equal(progress$planted_count, 2)
  expect_equal(progress$placeholder_count, 2)
  expect_equal(progress$status, "under")
})

test_that("computeSowProgress 在超种时返回 over", {
  base_matrix <- matrix(c(
    1, 0,
    2, 0
  ), nrow = 2, byrow = TRUE)
  stat_cols <- cbind(c(0.5, 0.6), c(0, 0), c(2, 1), c(1, 2))
  base_matrix <- cbind(base_matrix, stat_cols)
  colnames(base_matrix) <- c(paste0("V", 1:2), STAT_COL_NAMES)

  sow_df <- data.frame(
    Location = c("A", "A", "A"),
    ID = c("材料甲", "材料乙", "材料丙"),
    Y = c(1, 2, 2),
    X = c(1, 1, 2),
    stringsAsFactors = FALSE
  )

  progress <- computeSowProgress(base_matrix, sow_df)
  expect_equal(progress$expected_count, 2)
  expect_equal(progress$planted_count, 3)
  expect_equal(progress$placeholder_count, 0)
  expect_equal(progress$status, "over")
})

test_that("computeSowProgress 在满种时返回 ok", {
  base_matrix <- matrix(c(
    1, 1,
    0, 2
  ), nrow = 2, byrow = TRUE)
  stat_cols <- cbind(c(0.5, 0.6), c(0, 0), c(2, 1), c(1, 2))
  base_matrix <- cbind(base_matrix, stat_cols)
  colnames(base_matrix) <- c(paste0("V", 1:2), STAT_COL_NAMES)

  sow_df <- data.frame(
    Location = c("A", "A", "A"),
    ID = c("材料甲", "材料乙", "材料丙"),
    Y = c(1, 1, 2),
    X = c(1, 2, 2),
    stringsAsFactors = FALSE
  )

  progress <- computeSowProgress(base_matrix, sow_df)
  expect_equal(progress$expected_count, 3)
  expect_equal(progress$planted_count, 3)
  expect_equal(progress$placeholder_count, 0)
  expect_equal(progress$status, "ok")
})

test_that("导入试验允许 former_stageid 缺失并默认置空", {
  planting_df <- data.frame(
    fieldid = c("F1", "F1"),
    id = c("1", "2"),
    stageid = c("S1", "S2"),
    name = c("A", "B"),
    source = c("SRC1", "SRC2"),
    code = c("C1", "C2"),
    rp = c("1", "1"),
    rows = c(2, 3),
    line_number = c("1", "2"),
    stringsAsFactors = FALSE
  )

  normalized <- normalizePlantingDataFrame(planting_df)
  expect_true("former_stageid" %in% names(normalized))
  expect_equal(as.character(normalized$former_stageid), c("", ""))
  expect_equal(as.character(normalized$source), c("SRC1", "SRC2"))
})

test_that("重置地块可同步清空对应播种表", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".sqlite")
  field_name <- "测试地块清空"
  plant_table <- paste0(field_name, ".plant")
  sow_table <- paste0(field_name, ".sow")

  base_matrix <- matrix(c(
    1, 2,
    3, 4
  ), nrow = 2, byrow = TRUE)
  stat_cols <- cbind(c(0.5, 0.6), c(0, 0), c(2, 1), c(1, 2))
  plan_matrix <- cbind(base_matrix, stat_cols)
  colnames(plan_matrix) <- c(paste0("V", 1:2), STAT_COL_NAMES)

  savePlantTable(field_name, plan_matrix, db_path)

  sow_data <- data.frame(
    Location = c(field_name),
    ID = c("材料甲"),
    Y = c(1),
    X = c(1),
    XiaoQuChangDu = c(50),
    GuoDaoKuanDu = c(100),
    HangJv = c(40),
    XiaoQuLiShu = c(6.5),
    stringsAsFactors = FALSE
  )
  saveSowTable(field_name, sow_data, db_path)

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(sow_table %in% DBI::dbListTables(con))

  dropped <- clearSowTableByPlantTable(plant_table, db_path)
  expect_true(isTRUE(dropped))
  expect_false(sow_table %in% DBI::dbListTables(con))
})

test_that("field_models 支持增删改查", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".sqlite")
  model_name <- "地块模型A"
  model_data <- list(
    field_len = 200,
    no_plant = "23,25,3,5",
    field_layout = "w/8/w",
    strip_width = "10,6/3,10",
    protect_strip = "1",
    cross_path_width = 1,
    row_gap = 0.5,
    group_rows = 2,
    plant_start_row = 1,
    plant_start_col = 1,
    plant_end_col = 8,
    plan_left = TRUE,
    plant_left = TRUE
  )

  saveFieldModel(model_name, model_data, db_path)
  listed <- listFieldModels(db_path)
  expect_gt(nrow(listed), 0)
  expect_true(model_name %in% listed$field_name)

  fetched <- getFieldModel(model_name, db_path)
  expect_equal(fetched$field_name, model_name)
  expect_equal(as.numeric(fetched$field_len), 200)

  saveFieldModel(model_name, modifyList(model_data, list(field_len = 250)), db_path)
  fetched2 <- getFieldModel(model_name, db_path)
  expect_equal(as.numeric(fetched2$field_len), 250)

  deleteFieldModel(model_name, db_path)
  expect_null(getFieldModel(model_name, db_path))
})

test_that("试验管理可保存 experiments 与 experiment_records 并汇总 rows", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".sqlite")
  planting_df <- data.frame(
    fieldid = c("F1", "F1", "F2"),
    id = c("1", "2", "3"),
    stageid = c("S1", "S1", "S2"),
    name = c("A", "B", "C"),
    former_stageid = c("FS0", "FS0", "FS1"),
    source = c("SRC-A", "SRC-A", "SRC-B"),
    code = c("CA", "CB", "CC"),
    rp = c("1", "1", "2"),
    rows = c(2, 3, 5),
    line_number = c(11, 12, 13),
    stringsAsFactors = FALSE
  )

  result <- saveExperimentWithRecords(
    experiment_name = "试验A",
    planting_df = planting_df,
    db_path = db_path
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  experiments <- DBI::dbReadTable(con, "experiments")
  records <- DBI::dbReadTable(con, "experiment_records")

  expect_true(nzchar(result$experiment_id))
  expect_equal(result$record_count, 3)
  expect_equal(as.numeric(result$total_rows), 10)
  expect_equal(nrow(experiments), 1)
  expect_equal(as.character(experiments$experiment_id[1]), result$experiment_id)
  expect_equal(as.numeric(experiments$total_rows[1]), 10)
  expect_equal(nrow(records), 3)
  expect_true(all(c("experiment_id", "fieldid", "former_stageid", "source", "rows", "line_number") %in% names(records)))
  expect_equal(as.character(records$former_stageid), c("FS0", "FS0", "FS1"))
  expect_equal(as.character(records$source), c("SRC-A", "SRC-A", "SRC-B"))
  expect_type(as.character(records$line_number), "character")
})

test_that("可从 Excel 的 planting 表导入试验并写入两张表", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("openxlsx")

  db_path <- tempfile(fileext = ".sqlite")
  excel_path <- tempfile(fileext = ".xlsx")

  planting_df <- data.frame(
    fieldid = c("F10", "F10"),
    id = c("101", "102"),
    stageid = c("S1", "S2"),
    name = c("材料甲", "材料乙"),
    former_stageid = c("FS10", "FS11"),
    source = c("SRC-X", "SRC-Y"),
    code = c("A01", "A02"),
    rp = c("1", "1"),
    rows = c(4, 6),
    line_number = c(1, 2),
    stringsAsFactors = FALSE
  )
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "planting")
  openxlsx::writeData(wb, "planting", planting_df)
  openxlsx::saveWorkbook(wb, excel_path, overwrite = TRUE)

  result <- importExperimentFromPlantingExcel(
    excel_path = excel_path,
    experiment_name = "导入试验A",
    db_path = db_path,
    sheet = "planting"
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  experiments <- DBI::dbReadTable(con, "experiments")
  records <- DBI::dbReadTable(con, "experiment_records")

  expect_true(nzchar(result$experiment_id))
  expect_equal(as.character(experiments$experiment_name[1]), "导入试验A")
  expect_equal(as.numeric(experiments$total_rows[1]), 10)
  expect_equal(nrow(records), 2)
  expect_true(all(as.character(records$experiment_id) == result$experiment_id))
  expect_true(all(as.character(records$line_number) %in% c("1", "2")))
  expect_equal(as.character(records$former_stageid), c("FS10", "FS11"))
  expect_equal(as.character(records$source), c("SRC-X", "SRC-Y"))
})

test_that("试验管理支持重命名与级联删除", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".sqlite")
  planting_df <- data.frame(
    fieldid = c("F1", "F2"),
    id = c("1", "2"),
    stageid = c("S1", "S2"),
    name = c("X", "Y"),
    former_stageid = c("FSA", "FSB"),
    source = c("SRC-1", "SRC-2"),
    code = c("CX", "CY"),
    rp = c("1", "1"),
    rows = c(1, 2),
    line_number = c(1, 2),
    stringsAsFactors = FALSE
  )

  saved <- saveExperimentWithRecords("原试验名", planting_df, db_path)
  exp_id <- saved$experiment_id

  rename_affected <- renameExperiment(exp_id, "新试验名", db_path)
  expect_equal(rename_affected, 1)

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  experiments <- DBI::dbReadTable(con, "experiments")
  expect_equal(as.character(experiments$experiment_name[1]), "新试验名")

  delete_affected <- deleteExperiment(exp_id, db_path)
  expect_gte(delete_affected, 1)

  experiments_after <- DBI::dbReadTable(con, "experiments")
  records_after <- DBI::dbReadTable(con, "experiment_records")
  expect_equal(nrow(experiments_after), 0)
  expect_equal(nrow(records_after), 0)
})

test_that("可保存并读取地块名.plant种植地块表", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".sqlite")
  plan_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
  field_name <- "测试地块A"

  saved <- savePlantTable(field_name, plan_matrix, db_path)
  expect_equal(saved$table_name, "测试地块A.plant")

  tables <- listPlantTables(db_path)
  expect_true(any(as.character(tables$table_name) == "测试地块A.plant"))

  loaded <- readPlantTable("测试地块A.plant", db_path)
  expect_equal(as.numeric(loaded), as.numeric(plan_matrix))
  expect_equal(dim(loaded), dim(plan_matrix))
})

test_that("导入 planting 数据会先按 fieldid 排序", {
  unsorted_df <- data.frame(
    fieldid = c("F3", "F1", "F2"),
    id = c("3", "1", "2"),
    stageid = c("S1", "S1", "S1"),
    name = c("C", "A", "B"),
    former_stageid = c("FS3", "FS1", "FS2"),
    source = c("S3", "S1", "S2"),
    code = c("C3", "A1", "B2"),
    rp = c("1", "1", "1"),
    rows = c(3, 1, 2),
    line_number = c("30", "10", "20"),
    stringsAsFactors = FALSE
  )

  normalized <- normalizePlantingDataFrame(unsorted_df)
  expect_equal(as.character(normalized$fieldid), c("F1", "F2", "F3"))
})
