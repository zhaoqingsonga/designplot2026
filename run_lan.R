args <- commandArgs(trailingOnly = TRUE)

parse_arg <- function(prefix, default_value) {
  hit <- args[startsWith(args, paste0(prefix, "="))]
  if (length(hit) == 0) {
    return(default_value)
  }
  sub(paste0("^", prefix, "="), "", hit[1])
}

host <- parse_arg("host", "0.0.0.0")
port <- suppressWarnings(as.integer(parse_arg("port", "3939")))
if (is.na(port) || port < 1 || port > 65535) stop("port 必须是 1-65535 的整数")

cat(sprintf("启动局域网模式: http://%s:%d\n", host, port))
cat("提示：同一局域网设备可用 你的IP:端口 访问（如 192.168.1.10:3939）\n")

SOURCED_AS_LIB <- TRUE
source("designplot.R", local = TRUE)

shiny::runApp(
  shiny::shinyApp(ui = ui, server = server),
  host = host,
  port = port,
  launch.browser = FALSE
)
