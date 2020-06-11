test_that("multiplication works", {
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 5, B = 100)
  expect_s3_class(fit, "blblm")
  co <- coef(fit)
  n<-nrow(mtcars)
  m=5
  expect_equal(length(co), 4)
  fit1 <- blblm_cluster(mpg ~ wt * hp, data = mtcars, m = 5, B = 100,3)
  expect_s3_class(fit1, "blblm")
  co1 <- coef(fit1)
  n<-nrow(mtcars)
  m=5
  expect_equal(length(co1), 4)
  fit2 <- blbglm(vs ~ wt * hp, data = mtcars, m = 5, B = 100)
  expect_s3_class(fit2, "blbglm")
  co2 <- coef(fit2)
  n<-nrow(mtcars)
  m=5
  expect_equal(length(co2), 4)
  fit3 <- blbglm_cluster(vs ~ wt * hp, data = mtcars, m = 5, B = 100,3)
  expect_s3_class(fit3, "blbglm")
  co3 <- coef(fit3)
  n<-nrow(mtcars)
  m=5
  expect_equal(length(co3), 4)
})
