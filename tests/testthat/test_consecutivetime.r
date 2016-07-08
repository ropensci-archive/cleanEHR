context("Tests for conscutivetime")

test_that("Check the sequence is the wanted",
{
  # with no changes
  input_seq <- c( T, T, F, F, T, T)
  output_seq <- c( 1, 1, 2, 3, 4, 4)
  expect_equal(consecutivetime(input_seq), output_seq)
})

test_that("All from the same",
{
  # with no changes
  input_seq <- c( T, T, T, T, T, T)
  output_seq <- c( 1, 1, 1, 1, 1, 1)
  expect_equal(consecutivetime(input_seq), output_seq)
})

test_that("All from different",
{
  # with no changes
  input_seq <- ! c( T, T, T, T, T, T)
  output_seq <- c( 1:6 )
  expect_equal(consecutivetime(input_seq), output_seq)
})
