buildDesignplotUI <- function(){
  fluidPage(
    navbarPage("田间设计",
               tabPanel("田间规划",
                        sidebarLayout(
                          sidebarPanel(
                            tags$div(
                              style = "background:#f8fafc;border:1px solid #dbe4ee;border-radius:10px;padding:12px 12px 8px 12px;margin-bottom:10px;",
                              tags$div("地块管理", style = "font-size:16px;font-weight:600;color:#1f2937;margin-bottom:8px;"),
                              selectInput("field_model_select", "当前地块", choices = c("常规地块1" = "常规地块1"), selected = "常规地块1", width = "100%"),
                              fluidRow(
                                column(4, actionButton("saveFieldModel", "保存参数", class = "btn-primary", width = "100%")),
                                column(4, actionButton("addFieldModel", "增加参数", class = "btn-success", width = "100%")),
                                column(4, actionButton("deleteFieldModel", "删除参数", class = "btn-danger", width = "100%"))
                              ),
                              tags$div(style = "height:8px;"),
                              actionButton("generatePlantField", "创建地块", class = "btn-info", width = "100%"),
                              tags$div("提示：设计表会实时更新；\"保存\"仅保存地块模板参数，\"创建地块\"才会写入可用于种植的地块表。", style = "font-size:12px;color:#6b7280;margin-top:8px;line-height:1.6;")
                            ),
                            tags$hr(),
                            tags$div(
                              style = "background:#ffffff;border:1px solid #dbe4ee;border-radius:10px;padding:10px 12px;margin-bottom:10px;",
                              tags$div("基础参数", style = "font-size:15px;font-weight:600;color:#1f2937;margin-bottom:6px;"),
                              tags$p("建议先设置以下参数，再查看右侧设计表。", style = "margin:0 0 6px 0;color:#6b7280;font-size:12px;"),
                              numericInput("f_l", h5("规划地长(m)"), value = NA),
                              textInput("get_water_columns", h5("田间纵向布局"), value="", placeholder = "示例：w/8/w"),
                              tags$p("规则：w=水沟；r=纵向观察道（2r=1.5米）；p=保护行（可加数字）；斜杠间数字代表水沟间行数。", style = "margin-top:-6px;color:#6b7280;font-size:12px;line-height:1.6;"),
                              textInput("bridges", h5("条带宽度设置"), value="", placeholder = "示例：10,6/3,10"),
                              tags$p("规则：\"/\"前为长度，\"/\"后为重复数。", style = "margin-top:-6px;color:#6b7280;font-size:12px;")
                            ),
                            tags$div(
                              style = "background:#ffffff;border:1px solid #dbe4ee;border-radius:10px;padding:10px 12px;margin-bottom:10px;",
                              tags$div("通道与间隔", style = "font-size:15px;font-weight:600;color:#1f2937;margin-bottom:6px;"),
                              numericInput("ww", h5("田间横向观察道宽(m)"), value = NA, min = 0),
                              numericInput("w", h5("材料间隔距离(m)"), value = NA, min = 0),
                              numericInput("subg", h5("材料按组种植（组内材料不隔断）"), value = NA, min = 1),
                              tags$div(style = "height:6px;"),
                              tags$div("种植位置设置", style = "font-size:14px;font-weight:600;color:#1f2937;margin-bottom:4px;"),
                              textInput("plant_start_pos", h5("种植起始位置（行,列）"), value = "", placeholder = "示例：1,1 / 1, / ,3 / 1"),
                              textInput("plant_end_pos", h5("种植终止位置（行,列；留空=最后）"), value = "", placeholder = "示例：10,8 / 10, / ,8 / 10"),
                              tags$p("规则：格式 n,n（行,列）；起始留空默认 1,1；终止留空默认到最后。只控行可写 n 或 n,；只控列请写 ,n。", style = "margin-top:-6px;color:#6b7280;font-size:12px;line-height:1.6;")
                            ),
                            tags$details(
                              tags$summary("高级参数（可选）"),
                              tags$div(style = "margin-top:8px;"),
                              textInput("p_a", h5("不可种植区（可选）"), value="", placeholder = "示例：23,25,3,5,30,40,7,10"),
                              tags$p("规则：每4个数一组，依次为起始位置m、终止位置m、起始行、终止行。", style = "margin-top:-6px;color:#6b7280;font-size:12px;"),
                              textInput("protected_blocks",h5("横向保护行([-99])"),value="")
                            ),
                            tags$details(
                              tags$summary("编码图例（点击展开）"),
                              tags$div(style = "margin-top:8px;"),
                              p("表中",
                                span("正整数", style = "background-color:yellow"),
                                "代表可种植行，",
                                span("-77", style = "background-color:lightseagreen"),
                                "代表水沟，",
                                span("-11", style = "background-color:gray"),
                                "代表横向观察道，",
                                span("-111", style = "background-color:darkorange"),
                                "代表纵向观察道，",
                                span("-1", style = "background-color:silver"),
                                "代表材料间隔，",
                                span("-9", style = "background-color:green"),
                                "代表纵向保护行，",
                                span("-99", style = "background-color:seagreen"),
                                "代表横向保护行，",
                                span("-8", style = "background-color:red"),
                                "代表不能种植地块，",
                                span("-6", style = "background-color:limegreen"),
                                "代表分组后补充保护行。")
                            ),
                            radioButtons("design_from_left", h5("是否从左侧规划"), choices = list("是" = TRUE, "否" = FALSE), selected = TRUE),
                            radioButtons("plant_from_left", h5("是否从左侧排种"), choices = list("是" = TRUE, "否" = FALSE), selected = TRUE),
                            tags$p(
                              "说明：『从左侧规划』管地块排号方向；『从左侧排种』管材料从哪边开始种。两个开关互不影响。",
                              style = "margin:-6px 0 8px 0;color:#6b7280;font-size:12px;line-height:1.6;"
                            ),
                            tags$hr()
                          , width = 3),
                          mainPanel(
                            h3("设计表"), textOutput("selected_var"), downloadButton("downloadData", "下载设计表（xlsx）"),
                            h3("设计简表"), DT::dataTableOutput("simpleMydata"), downloadButton("simpleDownloadData", "下载设计简表（xlsx）"),
                            h3("行数统计"), DT::dataTableOutput("sta")
                          , width = 9)
                        )
               ),
               tabPanel("种植试验",
                        uiOutput("experimentStatusSummaryUi"),
                        fluidRow(
                          column(4,
                            # Step 1: 导入试验（蓝左边框）
                            tags$div(
                              style = "background:#ffffff;border:1px solid #dbe4ee;border-left:4px solid #3b82f6;border-radius:10px;padding:12px;margin-bottom:12px;",
                              fluidRow(
                                column(1, tags$span("1", style = "background:#3b82f6;color:#fff;border-radius:50%;width:22px;height:22px;display:inline-flex;align-items:center;justify-content:center;font-weight:700;font-size:13px;")),
                                column(11, tags$span("导入试验", style = "font-size:15px;font-weight:600;color:#1f2937;margin-left:6px;"))
                              ),
                              tags$p("上传含 planting 工作表的 Excel 文件以导入试验。", style = "margin:4px 0 8px 0;color:#6b7280;font-size:12px;"),
                              fileInput("experimentImportFile", "上传试验Excel（含 planting 工作表）", multiple = FALSE,
                                        accept = c(".xlsx", ".xls", "application/vnd.ms-excel", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
                              textInput("experimentImportName", "试验名称（默认=上传文件名）", value = ""),
                              actionButton("importExperimentPlanting", "导入试验", class = "btn-primary", width = "100%")
                            ),
                            # Step 2: 种植配置（白+阴影卡片）
                            tags$div(
                              style = "background:#ffffff;border:1px solid #dbe4ee;border-radius:10px;padding:12px;margin-bottom:12px;box-shadow:0 2px 8px rgba(15,23,42,0.06);",
                              fluidRow(
                                column(1, tags$span("2", style = "background:#10b981;color:#fff;border-radius:50%;width:22px;height:22px;display:inline-flex;align-items:center;justify-content:center;font-weight:700;font-size:13px;")),
                                column(11, tags$span("种植配置", style = "font-size:15px;font-weight:600;color:#1f2937;margin-left:6px;"))
                              ),
                              tags$div(style = "height:4px;"),
                              selectInput("experimentPlantId", "选择试验名称", choices = c("暂无试验" = ""), selected = ""),
                              selectInput("experimentPlantTableSelect", "选择种植地块", choices = c("暂无地块" = ""), selected = ""),
                              textInput("experimentPlantStartPos", "种植起始位置（行,列）", value = "1,1", placeholder = "示例：1,1 / 1, / ,3 / 1"),
                              textInput("experimentPlantEndPos", "种植终止位置（行,列；留空=最后）", value = "", placeholder = "示例：10,8 / 10, / ,8 / 10"),
                              uiOutput("experimentPlantSummaryUi")
                            ),
                            # Step 3: 区域补种（浅黄左边框）
                            tags$div(
                              style = "background:#fffbeb;border:1px solid #fcd34d;border-left:4px solid #f59e0b;border-radius:10px;padding:12px;margin-bottom:12px;",
                              fluidRow(
                                column(1, tags$span("3", style = "background:#f59e0b;color:#fff;border-radius:50%;width:22px;height:22px;display:inline-flex;align-items:center;justify-content:center;font-weight:700;font-size:13px;")),
                                column(11, tags$span("区域补种", style = "font-size:15px;font-weight:600;color:#1f2937;margin-left:6px;"))
                              ),
                              tags$div(style = "height:4px;"),
                              textInput("experimentFillMaterial", "区域补种材料（仅填未种位）", value = "", placeholder = "示例：补种材料A"),
                              uiOutput("experimentFillSummaryUi"),
                              actionButton("runExperimentFillUnplanted", "执行区域补种（仅未种）", class = "btn-warning", width = "100%")
                            )
                          ),
                          column(8,
                            # 执行控制卡（蓝左边框）
                            tags$div(
                              style = "background:#ffffff;border:1px solid #dbe4ee;border-left:4px solid #3b82f6;border-radius:12px;padding:14px;margin-bottom:12px;box-shadow:0 2px 8px rgba(15,23,42,0.06);",
                              fluidRow(
                                column(12, tags$span("执行控制", style = "font-size:16px;font-weight:600;color:#1f2937;"))
                              ),
                              tags$div(style = "height:8px;"),
                              checkboxInput("allowExperimentReplant", "允许覆盖重种（默认关闭）", value = FALSE),
                              tags$p("默认会阻止同一 experiment_id 在同一地块重复种植，避免误操作。", style = "margin:0 0 8px 0;color:#6b7280;font-size:12px;line-height:1.6;"),
                              uiOutput("runExperimentPlantingUi")
                            ),
                            # 试验名称表
                            tags$div(
                              style = "background:#ffffff;border:1px solid #dbe4ee;border-radius:12px;padding:14px;margin-bottom:12px;box-shadow:0 2px 8px rgba(15,23,42,0.06);",
                              fluidRow(
                                column(12, tags$span("试验名称表", style = "font-size:16px;font-weight:600;color:#1f2937;"))
                              ),
                              tags$div(style = "height:4px;"),
                              selectInput("sqliteFilterExperimentId", "筛选 experiment_id（行内删除按钮可直接删除该试验）", choices = c("全部试验" = ""), selected = "", width = "100%"),
                              DT::dataTableOutput("sqliteExperiments"),
                              downloadButton("downloadExperimentsCsv", "下载试验名称表（csv）")
                            ),
                            # 试验记录表
                            tags$div(
                              style = "background:#ffffff;border:1px solid #dbe4ee;border-radius:12px;padding:14px;box-shadow:0 2px 8px rgba(15,23,42,0.06);",
                              h3("试验记录表（experiment_records）", style = "margin-top:0;margin-bottom:10px;"),
                              DT::dataTableOutput("sqliteExperimentRecords"),
                              downloadButton("downloadExperimentRecordsCsv", "下载试验记录表（csv）")
                            )
                          )
                        )
               ),
               tabPanel("种植展示",
                        fluidRow(column(12,
                                        tags$div(style = "background:#ffffff;border:1px solid #dbe4ee;border-radius:12px;padding:14px 14px 10px 14px;box-shadow:0 2px 8px rgba(15,23,42,0.06);",
                                                 h3("种植地块预览", style = "margin-top:0;margin-bottom:10px;"),
                                                 selectInput("plant_table_select", "选择种植地块", choices = c("暂无地块" = ""), selected = "", width = "100%"),
                                                 DT::dataTableOutput("selectedPlantPlotPreview"),
                                                 downloadButton("plantPreviewDownloadData", "下载种植展示（xlsx）")
                                        )))),
               tabPanel("播种列表",
                        mainPanel(
                          h3("播种列表"),
                          actionButton("refreshSowList", "刷新播种列表", class = "btn-default"),
                          tags$div(style = "height:8px;"),
                          selectInput("sow_table_select", "选择播种地块", choices = c("暂无播种表" = ""), selected = "", width = "100%"),
                          uiOutput("sowIntegrityUi"),
                          DT::dataTableOutput("mydf"),
                          downloadButton("downloadSowListXlsx", "下载播种列表（xlsx）")
                        )),
               tabPanel("田间布局图",
                        fluidRow(column(12,
                                        tags$div(style = "background:#ffffff;border:1px solid #dbe4ee;border-radius:12px;padding:14px 14px 10px 14px;box-shadow:0 2px 8px rgba(15,23,42,0.06);",
                                                  h3("田间布局总览", style = "margin-top:0;margin-bottom:10px;"),
                                                  selectInput("layout_field_select", "选择地块", choices = c("暂无地块" = ""), selected = "", width = "100%"),
                                                  uiOutput("fieldLayoutError"),
                                                   tags$p("提示：补种材料会在原实验色块上叠加虚线边框显示（不改变原实验颜色）。", style = "margin:0 0 8px 0;color:#6b7280;font-size:12px;"),
                                                   plotOutput("fieldLayoutPlot", height = "600px"),
                                                   downloadButton("downloadFieldLayoutPlot", "下载布局图（PNG）")
                                         ))))
    )
  )
}
