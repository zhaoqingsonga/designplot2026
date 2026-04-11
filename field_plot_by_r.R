# ============ 脚本模式运行田间规划（非 Shiny） ============
# 依赖：自动 source designplot.R 加载函数和常量（不启动 Shiny）
# 用法：修改下方参数后，在 RStudio 中 source 本文件即可生成 Excel

library(openxlsx)

# 加载 designplot.R 中的函数和常量（SOURCED_AS_LIB 阻止启动 Shiny）
SOURCED_AS_LIB <- TRUE
source("designplot.R", encoding = "UTF-8")

# ============ 参数配置（按实际情况修改） ============
# 工作目录和材料文件路径
work_dir <- "."  # 修改为实际数据所在目录
seeds_file <- "材料列表.xlsx"  # 修改为实际材料文件名
param_file <- "常规地块1参数.txt"  # 修改为实际参数文件路径

setwd(work_dir)
seeds_sum<-read.xlsx(seeds_file, 1)

# ============ 调入参数 ============
params <- parseParamTxt(param_file)

field_len <- if(!is.null(params$field_len)) params$field_len else 200
no_plant <- if(!is.null(params$no_plant)) params$no_plant else NULL
field_layout <- if(!is.null(params$field_layout)) params$field_layout else "120"
strip_width <- if(!is.null(params$strip_width)) params$strip_width else "4,3/80"
protect_strip <- if(!is.null(params$protect_strip)) params$protect_strip else NULL
cross_path_width <- if(!is.null(params$cross_path_width)) params$cross_path_width else 1
row_gap <- if(!is.null(params$row_gap)) params$row_gap else 0
group_rows <- if(!is.null(params$group_rows)) params$group_rows else 2
plant_start_row <- if(!is.null(params$plant_start_row)) params$plant_start_row else 1
plant_start_col <- if(!is.null(params$plant_start_col)) params$plant_start_col else 1
plant_end_col <- if(!is.null(params$plant_end_col)) params$plant_end_col else NULL
plan_left <- if(!is.null(params$plan_left)) params$plan_left else TRUE
plant_left <- if(!is.null(params$plant_left)) params$plant_left else TRUE

to_num_vec <- function(x){
  if(is.null(x)) return(NULL)
  if(is.numeric(x)) return(x)
  xv <- trimws(unlist(strsplit(as.character(x), ",")))
  xv <- xv[nchar(xv)>0]
  if(length(xv)==0) return(NULL)
  suppressWarnings(as.numeric(xv))
}

w_c <- getWaterLane(field_layout)
bridges <- modibridges(strip_width)
protect_strip <- to_num_vec(protect_strip)
no_plant <- to_num_vec(no_plant)
mys<-designPlot(blocks=length(bridges),
                y=w_c$total_cols,
                bridges=bridges,
                ww=cross_path_width,
                w=row_gap,
                protected_columns=w_c$protect_cols,
                protected_blocks=protect_strip,
                water_columns=w_c$water_cols,
                lane_columns=w_c$lane_cols,
                p_a=no_plant,
                design_from_left=plan_left,
                plant_from_left=plant_left,
                subg=group_rows
                )
simplemys<-selectedCol(mys)
plant_col_range <- parsePlantingColumnRange(plant_start_col, plant_end_col, w_c$total_cols)
plant_row_start <- parsePlantingStartRow(plant_start_row)
seeds_list<-makelistWithPos(seeds_sum, mys, start_row = plant_row_start, start_col = plant_col_range$start_col, end_col = plant_col_range$end_col)
planted<-plant(mys, seeds_list, start_row = plant_row_start, start_col = plant_col_range$start_col, end_col = plant_col_range$end_col)
sta<-getSta(mys)

experiment_name <- defaultExperimentName(param_file = param_file)
sqlite_plan_result <- savePlanToSqlite(
  plan_matrix = mys,
  experiment_name = experiment_name,
  metadata = list(
    source_param_file = param_file,
    field_length = field_len,
    field_layout = field_layout,
    bridge_layout = strip_width,
    row_gap = row_gap,
    group_rows = group_rows,
    design_from_left = plan_left,
    plant_from_left = plant_left
  )
)
saveAssignmentsToSqlite(
  plan_id = sqlite_plan_result$plan_id,
  planted_matrix = planted,
  plan_matrix = mys,
  experiment_name = experiment_name
)

# 复用 designplot.R 的着色策略到指定工作表
applyDesignplotColoring <- function(wb, sheet, data, ref_data=NULL, color_planted=TRUE){
  data_cols <- max(1, ncol(data) - STAT_COL_COUNT)
  if(is.null(ref_data)){
    for(st in EXCEL_COND_STYLES){
      if(st$code == 0){
        conditionalFormatting(wb, sheet,
                              cols=1:data_cols,
                              rows=2:(nrow(data)+1),
                              rule="==0",
                              style=st$style)
      }else{
        conditionalFormatting(wb, sheet,
                              cols=1:data_cols,
                              rows=2:(nrow(data)+1),
                              rule=paste0("==", st$code),
                              style=st$style)
      }
    }
  }else{
    for(st in EXCEL_CELL_STYLES){
      if(st$code == 0){
        if(isTRUE(color_planted)){
          positions<-which(ref_data[,1:data_cols, drop=FALSE] > 0, arr.ind=TRUE)
        }else{
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
        for(k in 1:nrow(positions)){
          addStyle(wb, sheet, style=st$style,
                   rows=positions[k,1]+1, cols=positions[k,2])
        }
      }
    }
  }
}

wb<-createWorkbook()
    addWorksheet(wb, "设计表")
    addWorksheet(wb, "设计简表")
    addWorksheet(wb, "材料列表（按重复）")
    addWorksheet(wb, "材料列表（按行）")
    addWorksheet(wb, "种植列表")
    addWorksheet(wb, "行数统计")
    writeData(wb, 1, mys)
    writeData(wb, 2, simplemys)
    writeData(wb, 3, seeds_sum)
    writeData(wb, 4, seeds_list)
    writeData(wb, 5, planted)
    writeData(wb, 6, sta)
    
    # 复刻 designplot.R 的导出着色逻辑
    applyDesignplotColoring(wb, 1, mys)
    applyDesignplotColoring(wb, 2, simplemys)
    applyDesignplotColoring(wb, 5, planted, ref_data=mys, color_planted=FALSE)
    
    openXL(wb)
    
    
