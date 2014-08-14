context("adapt")

test_that("str_length is number of characters", {
    data(mtcars)
    m = adapt(mpg~., data=mtcars, family=gaussian)
})
