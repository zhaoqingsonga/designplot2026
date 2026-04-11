library(shiny)
library(DT)
library(openxlsx)
library(plotly)
library(ggplot2)
library(scales)

# ============ 加载常量定义（拆分文件） ============
constants_candidates <- c(file.path("R", "constants.R"), file.path("..", "..", "R", "constants.R"))
constants_path <- constants_candidates[file.exists(constants_candidates)][1]
if (is.na(constants_path)) stop("无法找到常量文件: R/constants.R")
source(constants_path, local = TRUE)

# ============ 加载解析函数（拆分文件） ============
parsers_candidates <- c(file.path("R", "parsers.R"), file.path("..", "..", "R", "parsers.R"))
parsers_path <- parsers_candidates[file.exists(parsers_candidates)][1]
if (is.na(parsers_path)) stop("无法找到解析函数文件: R/parsers.R")
source(parsers_path, local = TRUE)

# ============ 加载核心算法函数（拆分文件） ============
core_candidates <- c(file.path("R", "core_design.R"), file.path("..", "..", "R", "core_design.R"))
core_path <- core_candidates[file.exists(core_candidates)][1]
if (is.na(core_path)) stop("无法找到核心函数文件: R/core_design.R")
source(core_path, local = TRUE)

# ============ 加载 SQLite 持久化函数（拆分文件） ============
sqlite_candidates <- c(file.path("R", "sqlite_persistence.R"), file.path("..", "..", "R", "sqlite_persistence.R"))
sqlite_path <- sqlite_candidates[file.exists(sqlite_candidates)][1]
if (is.na(sqlite_path)) stop("无法找到 SQLite 持久化文件: R/sqlite_persistence.R")
source(sqlite_path, local = TRUE)

# ============ 将数据写入带条件格式的Excel文件 ============
writeFormattedXlsx<-function(data, file, ref_data=NULL, color_planted=TRUE, highlight_positive=FALSE){
  wb<-createWorkbook()
  addWorksheet(wb, "Sheet1")
  writeData(wb, 1, data)
  # 仅对真正的地块数据区着色，避免把右侧统计列一并染色
  data_cols <- max(1, ncol(data) - STAT_COL_COUNT)
  if(is.null(ref_data)){
    # 数值型数据：使用条件格式（bgFill）
    for(st in EXCEL_COND_STYLES){
      if(st$code == 0){
        conditionalFormatting(wb, 1,
                              cols=1:data_cols,
                              rows=2:(nrow(data)+1),
                              rule=if(isTRUE(highlight_positive)) ">0" else "==0",
                              style=st$style)
      }else{
        conditionalFormatting(wb, 1,
                              cols=1:data_cols,
                              rows=2:(nrow(data)+1),
                              rule=paste0("==", st$code),
                              style=st$style)
      }
    }
  }else{
    # 种植列表等字符型数据：根据ref_data的编码值直接着色（fgFill）
    for(st in EXCEL_CELL_STYLES){
      if(st$code == 0){
        if(isTRUE(color_planted)){
          # 设计表/简表：可种植格着黄
          positions<-which(ref_data[,1:data_cols, drop=FALSE] > 0, arr.ind=TRUE)
        }else{
          # 种植列表：只给"未种/空白"格着黄（ref_data>0 但 data 中仍是数字）
          plantable_mask <- ref_data[,1:data_cols, drop=FALSE] > 0
          data_subset <- data[,1:data_cols, drop=FALSE]
          is_numeric_mask <- !is.na(suppressWarnings(as.numeric(as.matrix(data_subset))))
          unplanted_mask <- plantable_mask & is_numeric_mask
          positions<-which(unplanted_mask, arr.ind=TRUE)
        }
      }else{
        positions<-which(ref_data[,1:data_cols, drop=FALSE]==st$code, arr.ind=TRUE)
      }
      if(nrow(positions)>0){
        addStyle(
          wb, 1,
          style = st$style,
          rows = positions[, 1] + 1,
          cols = positions[, 2],
          gridExpand = FALSE,
          stack = TRUE
        )
      }
    }
  }
  saveWorkbook(wb, file, overwrite=TRUE)
}

# ============ 加载 Shiny UI/Server 组装模块 ============
app_ui_candidates <- c(file.path("R", "app_ui.R"), file.path("..", "..", "R", "app_ui.R"))
app_ui_path <- app_ui_candidates[file.exists(app_ui_candidates)][1]
if (is.na(app_ui_path)) stop("无法找到 UI 组装文件: R/app_ui.R")
source(app_ui_path, local = TRUE)

app_server_candidates <- c(file.path("R", "app_server.R"), file.path("..", "..", "R", "app_server.R"))
app_server_path <- app_server_candidates[file.exists(app_server_candidates)][1]
if (is.na(app_server_path)) stop("无法找到 Server 组装文件: R/app_server.R")
source(app_server_path, local = TRUE)

# ============ 定义UI界面 ============
ui <- buildDesignplotUI()

# ============ 定义服务器逻辑 ============
server <- function(input, output) {
  buildDesignplotServer(input, output)
}
# ============ 启动应用 ============
# 当被其他脚本 source 时（如 field_plot_by_r.R），跳过启动 Shiny
# Run the app ----
if(!exists("SOURCED_AS_LIB")){
  shinyApp(ui = ui, server = server)
}
