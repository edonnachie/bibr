library(tidyverse)
context("test-pdf_parsing.R")

test_that("PDF filename parsed correctly", {
  filenames <- c(
    "Cox1972_Regression Models and Life-Tables.pdf",
    "Cox-1972-Regression Models and Life-Tables.pdf",
    "Kaplan-Meier-1958-Nonparametric_estimation.pdf",
    "Kaplan_Meier1958-Non-parametric_estimation.pdf")

  expected_output <- tibble:tibble(
      filename = filenames[1],
      author = "Cox",
      year = "1972",
      title = "Regression Models and Life-Tables"
  ) %>%
    tibble::add_row(
      filename = filenames[2],
      author = "Cox",
      year = "1972",
      title = "Regression Models and Life-Tables"
    )
    tibble::add_row(
      filename = filenames[3],
      author = "Kaplan-Meier",
      year = "1958",
      title = "Nonparametric estimation"
    ) %>%
    tibble::add_row(
      filename = filenames[4],
      author = "Kaplan Meier",
      year = "1958",
      title = "Non-parametric estimation"
    )

  expect_equal(
    pdf_parse_filename(filenames),
    expected_output
    )
})
