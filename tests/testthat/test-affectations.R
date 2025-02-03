# Test assign_depart_hard

test_that("assign_depart_hard assigns a department correctly", {
    result <- assign_depart_hard(parse_file_output, 1)
    expect_equal(result$Aff_depart_1, parse_file_output$V1)
})

test_that("assign_depart_hard doesn't assign the same department twice", {
    result <- assign_depart_hard(parse_file_output, 1)
    result <- assign_depart_hard(parse_file_output, 1)
    expect_equal(result$Aff_depart_2, c(NA_character_, NA_character_, NA_character_))
})

# Test assign_depart_erase

test_that("assign_depart_erase erases all affected departments", {
    result <- assign_depart_hard(parse_file_output, 1)
    result <- assign_depart_erase(result)
    expect_equal(result$Aff_depart_1, c(NA_character_, NA_character_, NA_character_))
})
