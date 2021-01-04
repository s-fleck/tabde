context("generate_dummy")


test_that("generate_dummy works as expected", {
  parse_sql_type("varchar(10)")
  generate_dummy_col("varchar(10)")

  generate_dummy_df(table_design = gvrail:::table_design$gvrail_app_movements_plaus)

})
