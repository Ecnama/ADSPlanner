# Test assign_depart_hard

test_that("assign_depart_hard assigns a department correctly", {
    result <- assign_depart_hard(parse_file_output, c(1, 2, 3), 1)
    expect_equal(result$df$Aff_depart_1, parse_file_output$V1)
})

test_that("assign_depart_hard doesn't assign the same department twice", {
    result <- assign_depart_hard(parse_file_output, c(1, 2, 3), 1)
    result <- assign_depart_hard(parse_file_output, c(1, 2, 3), 1)
    expect_equal(result$df$Aff_depart_2, c(NA_character_, NA_character_, NA_character_))
})

test_that("assign_depart_hard only affects selected students", {
    result <- assign_depart_hard(parse_file_output, c(1, 2), 1)
    expect_equal(result$df$Aff_depart_1, c(parse_file_output$V1[1], parse_file_output$V1[2], NA_character_))
})

# Test assign_depart_erase

test_that("assign_depart_erase erases all affected departments", {
    result <- assign_depart_hard(parse_file_output, c(1, 2, 3), 1)
    result <- assign_depart_erase(result$df)
    expect_equal(result$df$Aff_depart_1, c(NA_character_, NA_character_, NA_character_))
})

test_that("assign_depart_erase only affects selected students", {
    result <- assign_depart_hard(parse_file_output, c(1, 2, 3), 1)
    result <- assign_depart_erase(result$df, c(1, 2))
    expect_equal(result$df$Aff_depart_1, c(NA_character_, NA_character_, parse_file_output$V1[3]))
})
