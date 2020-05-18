library(testthat)
#  throws_error("non-numeric argument"))
# Passes
# expect_that(10, equals(10))
# # Also passes
# expect_that(10, equals(10 + 1e-7))
# # Fails
# expect_that(10, equals(10 + 1e-6))
# # Definitely fails!
# expect_that(10, equals(11))
# # Passes
# expect_that(10, is_identical_to(10))
# # Fails
# expect_that(10, is_identical_to(10 + 1e-10))
# # Fails
# expect_that(c("one" = 1, "two" = 2),
# equals(1:2))
# # Passes
# expect_that(c("one" = 1, "two" = 2),
# is_equivalent_to(1:2))
# model <- lm(mpg ~ wt, data = mtcars)
# # Passes
# expect_that(model, is_a("lm"))
# # Fails
# expect_that(model, is_a("glm"))
# string <- "Testing is fun!"
# # Passes
# expect_that(string, matches("Testing"))
# # Fails, match is case-sensitive
# expect_that(string, matches("testing"))
# # Passes, match can be a regular expression
# expect_that(string, matches("t.+ting"))
# a <- list(1:10, letters)
# # Passes
# expect_that(str(a), prints_text("List of 2"))
# # Passes
# expect_that(str(a),
# prints_text(fixed("int [1:10]"))
# # Passes
# expect_that(library(mgcv),
# shows_message("This is mgcv"))
# # Passes
# expect_that(log(-1), gives_warning())
# expect_that(log(-1),
# gives_warning("NaNs produced"))
# # Fails
# expect_that(log(0), gives_warning())
# # Fails
# expect_that(1 / 2, throws_error())
# # Passes
# expect_that(1 / "a", throws_error())
# # But better to be explicit
# expect_that(1 / "a",
# expect_that(x, is_true()) expect_true(x)
# expect_that(x, is_false()) expect_false(x)
# expect_that(x, is_a(y)) expect_is(x, y)
# expect_that(x, equals(y)) expect_equal(x, y)
# expect_that(x, is_equivalent_to(y)) expect_equivalent(x, y)
# expect_that(x, is_identical_to(y)) expect_identical(x, y)
# expect_that(x, matches(y)) expect_matches(x, y)
# expect_that(x, prints_text(y)) expect_output(x, y)
# expect_that(x, shows_message(y)) expect_message(x, y)
# expect_that(x, gives_warning(y)) expect_warning(x, y)
# expect_that(x, throws_error(y)) expect_error(x, y)
# is_true <- function() {
# function(x) {
# expectation(
# identical(x, TRUE),
# "isn't true"
# )
# }
# }
# test_that("floor_date works for different units", {
# base <- as.POSIXct("2009-08-03 12:01:59.23", tz = "UTC")
# is_time <- function(x) equals(as.POSIXct(x, tz = "UTC"))
# floor_base <- function(unit) floor_date(base, unit)
# expect_that(floor_base("second"), is_time("2009-08-03 12:01:59"))
# expect_that(floor_base("minute"), is_time("2009-08-03 12:01:00"))
# expect_that(floor_base("hour"), is_time("2009-08-03 12:00:00"))
# expect_that(floor_base("day"), is_time("2009-08-03 00:00:00"))
# expect_that(floor_base("week"), is_time("2009-08-02 00:00:00"))
# expect_that(floor_base("month"), is_time("2009-08-01 00:00:00"))
# expect_that(floor_base("year"), is_time("2009-01-01 00:00:00"))
# })
test_dir("inst/tests/")
# String and pattern checks : ......
# Detecting patterns : .........
# Duplicating strings : ......
# Extract patterns : ..
# Joining strings : ......
# String length : .........
# Locations : ............
# Matching groups : ..............
# Test padding : ....
# Splitting strings : .........................
# Extracting substrings : ...................
# Trimming strings : ........
test_dir("inst/tests/", "minimal")
# ...............................................
# context("String length")
# test_that("str_length is number of characters", {
# expect_that(str_length("a"), equals(1))
# expect_that(str_length("ab"), equals(2))
# expect_that(str_length("abc"), equals(3))
# })
# test_that("str_length of missing is missing", {
# expect_that(str_length(NA), equals(NA_integer_))
# expect_that(str_length(c(NA, 1)), equals(c(NA, 1)))
# expect_that(str_length("NA"), equals(2))
# })
# test_that("str_length of factor is length of level", {
# expect_that(str_length(factor("a")), equals(1))
# expect_that(str_length(factor("ab")), equals(2))
# expect_that(str_length(factor("abc")), equals(3))
# })
source("test-str_length.r")
test_file("test-str_length.r")
# .........
source("test-nchar.r")
# Error: Test failure in 'nchar of missing is missing'
# * nchar(NA) not equal to NA_integer_
# * nchar(c(NA, 1)) not equal to c(NA, 1)
test_file("test-nchar.r")
# ...12..34
# 1. Failure: nchar of missing is missing ---------------------------------
# nchar(NA) not equal to NA_integer_
# 2. Failure: nchar of missing is missing ---------------------------------
# nchar(c(NA, 1)) not equal to c(NA, 1)
# 3. Failure: nchar of factor is length of level ---------------------------------
# nchar(factor("ab")) not equal to 2
# Mean relative difference: 0.5
# 4. Failure: nchar of factor is length of level ---------------------------------
# nchar(factor("abc")) not equal to 3
# Mean relative difference: 0.6666667
# 'is.NA' value mismatch: 0 in current 1 in target
# 'is.NA' value mismatch: 0 in current 1 in target
# 'is.NA' value mismatch: 0 in current 1 in target
# 'is.NA' value mismatch: 0 in current 1 in target
