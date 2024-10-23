values_long <- c(
    0.8, 0.2, NA, NA, 0.8, 0.1, NA, NA,
    0.7, 0.1, NA, NA, 0.7, 0.2, 0.5, NA,
    0.7, 0.2, 0.5, NA, 0.7, 0.3, 0.7, NA,
    0.7, 0.2, 0.7, 0.4, 0.7, 0.3, 0.7, 0.3
)

input_long_1 <- data.frame(
    rev_date = rep(x = c(
        "2022-07-31", "2022-08-31", "2022-09-30", "2022-10-31",
        "2022-11-30", "2022-12-31", "2023-01-31", "2023-02-28"
    ), each = 4L),
    time_period = rep(x = c("2022Q1", "2022Q2", "2022Q3", "2022Q4"), times = 8L),
    obs_value = values_long,
    stringsAsFactors = FALSE
)

input_long_2 <- data.frame(
    rev_date = rep(x = c(
        "31/07/2022", "31/08/2022", "30/09/2022", "31/10/2022",
        "30/11/2022", "31/12/2022", "31/01/2023", "28/02/2023"
    ), each = 4L),
    time_period = c(rep(x = c("2022 T1", "2022 T2", "2022 T3", "2022 T4"), times = 8L)),
    obs_value = values_long,
    stringsAsFactors = FALSE
)

input_long_3 <- data.frame(
    rev_date = rep(x = c(
        "2022-07-31", "2022-08-31", "2022-09-30", "2022-10-31",
        "2022-11-30", "2022-12-31", "2023-01-31", "2023-02-28"
    ), each = 4L),
    time_period = rep(x = c("2022-01-01", "2022-04-01", "2022-07-01", "2022-10-01"), times = 8L),
    obs_value = values_long,
    stringsAsFactors = FALSE
)

input_long_4 <- data.frame(
    rev_date = rep(x = c(
        "07/31/22", "08/31/22", "09/30/22", "10/31/22",
        "11/30/22", "12/31/22", "01/31/23", "02/28/23"
    ), each = 4L),
    time_period = c(rep(x = c("2022 Q1", "2022 Q2", "2022 Q3", "2022 Q4"), times = 8L)),
    obs_value = values_long,
    stringsAsFactors = FALSE
)

values_horizontal <- c(0.8, 0.8, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.2, 0.1,
                       0.1, 0.2, 0.2, 0.3, 0.2, 0.3, NA, NA, NA, 0.5, 0.5, 0.7, 0.7,
                       0.7, NA, NA, NA, NA, NA, NA, 0.4, 0.3)

input_horizontal_1 <- structure(
    .Data = values_horizontal,
    dim = c(8L, 4L),
    dimnames = list(
        c("2022-07-31", "2022-08-31", "2022-09-30", "2022-10-31",
          "2022-11-30", "2022-12-31", "2023-01-31", "2023-02-28"),
        c("2022Q1", "2022Q2", "2022Q3", "2022Q4")
    )
)

input_horizontal_2 <- structure(
    .Data = values_horizontal,
    dim = c(8L, 4L),
    dimnames = list(
        c("31/07/2022", "31/08/2022", "30/09/2022", "31/10/2022",
          "30/11/2022", "31/12/2022", "31/01/2023", "28/02/2023"),
        c("2022 T1", "2022 T2", "2022 T3", "2022 T4")
    )
)

input_horizontal_3 <- structure(
    .Data = values_horizontal,
    dim = c(8L, 4L),
    dimnames = list(
        c("2022-07-31", "2022-08-31", "2022-09-30", "2022-10-31",
          "2022-11-30", "2022-12-31", "2023-01-31", "2023-02-28"),
        c("2022-01-01", "2022-04-01", "2022-07-01", "2022-10-01")
    )
)

input_horizontal_4 <- structure(
    .Data = values_horizontal,
    dim = c(8L, 4L),
    dimnames = list(
        c("07/31/22", "08/31/22", "09/30/22", "10/31/22",
          "11/30/22", "12/31/22", "01/31/23", "02/28/23"),
        c("2022Q1", "2022Q2", "2022Q3", "2022Q4")
    )
)

values_vertical <- c(0.8, 0.2, NA, NA, 0.8, 0.1, NA, NA, 0.7, 0.1, NA,
                     NA, 0.7, 0.2, 0.5, NA, 0.7, 0.2, 0.5, NA, 0.7, 0.3, 0.7, NA,
                     0.7, 0.2, 0.7, 0.4, 0.7, 0.3, 0.7, 0.3)

input_vertical_1 <- ts(
    data = structure(
        .Data = values_vertical,
        dim = c(4L, 8L),
        dimnames = list(
            NULL,
            c("2022-07-31", "2022-08-31", "2022-09-30", "2022-10-31",
              "2022-11-30", "2022-12-31", "2023-01-31", "2023-02-28")
        )
    ),
    start = 2022,
    frequency = 4
)

input_vertical_2 <- ts(
    data = structure(
        .Data = values_vertical,
        dim = c(4L, 8L),
        dimnames = list(
            NULL,
            c("31/07/2022", "31/08/2022", "30/09/2022", "31/10/2022",
              "30/11/2022", "31/12/2022", "31/01/2023", "28/02/2023")
        )
    ),
    start = 2022,
    frequency = 4
)

input_vertical_3 <- ts(
    data = structure(
        .Data = values_vertical,
        dim = c(4L, 8L),
        dimnames = list(
            NULL,
            c("07/31/22", "08/31/22", "09/30/22", "10/31/22",
              "11/30/22", "12/31/22", "01/31/23", "02/28/23")
        )
    ),
    start = 2022,
    frequency = 4
)

input_vertical_4 <- structure(
    .Data = values_vertical,
    dim = c(4L, 8L),
    dimnames = list(
        c("2022Q1", "2022Q2", "2022Q3", "2022Q4"),
        c("2022-07-31", "2022-08-31", "2022-09-30", "2022-10-31",
          "2022-11-30", "2022-12-31", "2023-01-31", "2023-02-28")
    )
)

input_vertical_5 <- structure(
    .Data = values_vertical,
    dim = c(4L, 8L),
    dimnames = list(
        c("2022 T1", "2022 T2", "2022 T3", "2022 T4"),
        c("31/07/2022", "31/08/2022", "30/09/2022", "31/10/2022",
          "30/11/2022", "31/12/2022", "31/01/2023", "28/02/2023")
    )
)

input_vertical_6 <- structure(
    .Data = values_vertical,
    dim = c(4L, 8L),
    dimnames = list(
        c("2022-01-01", "2022-04-01", "2022-07-01", "2022-10-01"),
        c("2022-07-31", "2022-08-31", "2022-09-30", "2022-10-31",
          "2022-11-30", "2022-12-31", "2023-01-31", "2023-02-28")
    )
)

input_vertical_7 <- structure(
    .Data = values_vertical,
    dim = c(4L, 8L),
    dimnames = list(
        c("2022Q1", "2022Q2", "2022Q3", "2022Q4"),
        c("07/31/22", "08/31/22", "09/30/22", "10/31/22",
          "11/30/22", "12/31/22", "01/31/23", "02/28/23")
    )
)

test_that("Creation of vintages works for all format", {

    vintage1 <- create_vintages(x = input_long_1, type = "long", periodicity = 4L)
    vintage2 <- create_vintages(x = input_long_2, type = "long", periodicity = 4L, date_format = "%d/%m/%Y")
    vintage3 <- create_vintages(x = input_long_3, type = "long", periodicity = 4L)
    vintage4 <- create_vintages(x = input_long_4, type = "long", periodicity = 4L, date_format = "%m/%d/%y")

    vintage5 <- create_vintages(x = input_horizontal_1, type = "horizontal", periodicity = 4L)
    vintage6 <- create_vintages(x = input_horizontal_2, type = "horizontal", periodicity = 4L, date_format = "%d/%m/%Y")
    vintage7 <- create_vintages(x = input_horizontal_3, type = "horizontal", periodicity = 4L)
    vintage8 <- create_vintages(x = input_horizontal_4, type = "horizontal", periodicity = 4L, date_format = "%m/%d/%y")

    vintage9 <- create_vintages(x = input_vertical_1, type = "vertical")
    vintage10 <- create_vintages(x = input_vertical_2, type = "vertical", date_format = "%d/%m/%Y")
    vintage11 <- create_vintages(x = input_vertical_3, type = "vertical", date_format = "%m/%d/%y")
    vintage12 <- create_vintages(x = input_vertical_4, type = "vertical", periodicity = 4L)
    vintage13 <- create_vintages(x = input_vertical_5, type = "vertical", periodicity = 4L, date_format = "%d/%m/%Y")
    vintage14 <- create_vintages(x = input_vertical_6, type = "vertical", periodicity = 4L)
    vintage15 <- create_vintages(x = input_vertical_7, type = "vertical", periodicity = 4L, date_format = "%m/%d/%y")

    expect_identical(vintage1, vintage2)
    expect_identical(vintage1, vintage3)
    expect_identical(vintage1, vintage4)
    expect_identical(vintage1, vintage5)
    expect_identical(vintage1, vintage6)
    expect_identical(vintage1, vintage7)
    expect_identical(vintage1, vintage8)
    expect_identical(vintage1, vintage9)
    expect_identical(vintage1, vintage10)
    expect_identical(vintage1, vintage11)
    expect_identical(vintage1, vintage12)
    expect_identical(vintage1, vintage13)
    expect_identical(vintage1, vintage14)
    expect_identical(vintage1, vintage15)

})


test_that("Creation of vintages works for all format", {

    vintage1 <- create_vintages(x = input_long_1, type = "long", periodicity = 4L)
    vintage2 <- create_vintages(x = input_long_2, type = "long", periodicity = 4L, date_format = "%d/%m/%Y")
    vintage3 <- create_vintages(x = input_long_3, type = "long", periodicity = 4L)
    vintage4 <- create_vintages(x = input_long_4, type = "long", periodicity = 4L, date_format = "%m/%d/%y")

    vintage5 <- create_vintages(x = input_horizontal_1, type = "horizontal", periodicity = 4L)
    vintage6 <- create_vintages(x = input_horizontal_2, type = "horizontal", periodicity = 4L, date_format = "%d/%m/%Y")
    vintage7 <- create_vintages(x = input_horizontal_3, type = "horizontal", periodicity = 4L)
    vintage8 <- create_vintages(x = input_horizontal_4, type = "horizontal", periodicity = 4L, date_format = "%m/%d/%y")

    vintage9 <- create_vintages(x = input_vertical_1, type = "vertical")
    vintage10 <- create_vintages(x = input_vertical_2, type = "vertical", date_format = "%d/%m/%Y")
    vintage11 <- create_vintages(x = input_vertical_3, type = "vertical", date_format = "%m/%d/%y")
    vintage12 <- create_vintages(x = input_vertical_4, type = "vertical", periodicity = 4L)
    vintage13 <- create_vintages(x = input_vertical_5, type = "vertical", periodicity = 4L, date_format = "%d/%m/%Y")
    vintage14 <- create_vintages(x = input_vertical_6, type = "vertical", periodicity = 4L)
    vintage15 <- create_vintages(x = input_vertical_7, type = "vertical", periodicity = 4L, date_format = "%m/%d/%y")

    expect_identical(vintage1, vintage2)
    expect_identical(vintage1, vintage3)
    expect_identical(vintage1, vintage4)
    expect_identical(vintage1, vintage5)
    expect_identical(vintage1, vintage6)
    expect_identical(vintage1, vintage7)
    expect_identical(vintage1, vintage8)
    expect_identical(vintage1, vintage9)
    expect_identical(vintage1, vintage10)
    expect_identical(vintage1, vintage11)
    expect_identical(vintage1, vintage12)
    expect_identical(vintage1, vintage13)
    expect_identical(vintage1, vintage14)
    expect_identical(vintage1, vintage15)

})
