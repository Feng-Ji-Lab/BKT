check_data <- function(data) {
  # 检查 data 列数与 resources 行数是否匹配
  if (ncol(data$data) != nrow(data$resources)) {
    stop("data and resource sizes must match")
  }
  
  # 检查所有 starts + lengths - 1 是否小于等于 data 的列数
  if (!all(data$starts + data$lengths - 1 <= ncol(data$data))) {
    stop("data lengths don't match its shape")
  }
}
