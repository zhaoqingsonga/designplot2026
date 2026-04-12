test_that("删除地块功能正常工作", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  # 创建临时数据库
  db_path <- tempfile(fileext = ".sqlite")
  field_name <- "测试删除地块"

  # 创建测试矩阵
  test_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  colnames(test_matrix) <- c("V1", "V2")

  # 1. 保存地块
  save_result <- savePlantTable(field_name, test_matrix, db_path)
  expect_equal(save_result$table_name, paste0(field_name, ".plant"))

  # 2. 验证地块表已创建
  tables <- listPlantTables(db_path)
  expect_true(any(tables$field_name == field_name))

  # 3. 删除地块
  expect_true(deletePlantTable(field_name, db_path))

  # 4. 验证地块表已删除
  tables_after <- listPlantTables(db_path)
  expect_false(any(tables_after$field_name == field_name))

  # 5. 尝试删除不存在的地块应该报错
  expect_error(deletePlantTable(field_name, db_path), "种植地块表不存在")
})

test_that("deletePlantTable 参数验证正确", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  db_path <- tempfile(fileext = ".sqlite")

  # 测试空地块名
  expect_error(deletePlantTable("", db_path), "地块名不能为空")
  expect_error(deletePlantTable(NULL, db_path), "地块名不能为空")
  expect_error(deletePlantTable("   ", db_path), "地块名不能为空")
})