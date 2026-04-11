args <- commandArgs(trailingOnly = TRUE)

parse_arg <- function(prefix, default_value){
  hit <- args[startsWith(args, paste0(prefix, "="))]
  if(length(hit) == 0) return(default_value)
  sub(paste0("^", prefix, "="), "", hit[1])
}

host <- "127.0.0.1"
port <- suppressWarnings(as.integer(parse_arg("port", "3838")))
if(is.na(port) || port < 1 || port > 65535) stop("port 必须是 1-65535 的整数")

cat(sprintf("启动本地模式: http://%s:%d\n", host, port))
cat("提示：本模式仅本机可访问，不对局域网开放。\n")

SOURCED_AS_LIB <- TRUE
source("designplot.R", local = TRUE)

shiny::runApp(
  shiny::shinyApp(ui = ui, server = server),
  host = host,
  port = port,
  launch.browser = FALSE
)
