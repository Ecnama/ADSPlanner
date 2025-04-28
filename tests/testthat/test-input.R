# Testing synthesize_wishes

test_that("synthesize_wishes return correct result for FC_FIRE", {
    result <- synthesize_wishes(synth_wishes_input_fc_fire)
    expect_true(is.vector(result))
    expect_equal(length(result), 8)
    expect_equal(result, synth_wishes_output_fc_fire)
})

test_that("synthesize_wishes return correct result for EMIR", {
    result <- synthesize_wishes(synth_wishes_input_emir)
    expect_true(is.vector(result))
    expect_equal(length(result), 5)
    expect_equal(result, synth_wishes_output_emir)
})

test_that("synthesize_wishes return correct result for MICA", {
    result <- synthesize_wishes(synth_wishes_input_mica)
    expect_true(is.vector(result))
    expect_equal(length(result), 5)
    expect_equal(result, synth_wishes_output_mica)
})

test_that("synthesize_wishes throws error for non existing filière", {
    expect_error(synthesize_wishes(synth_wishes_input_invalid), "Unknown filiere")
})

# Testing parse_file

test_that("parse_file returns correct result for a valid input file", {
    result <- parse_file("../data/test_file.xlsx")
    expect_equal(result, parse_file_output)
})

test_that("parse_file throws error for unsupported file types", {
    file_path <- "../data/test_file.csv"
    expect_error(parse_file(file_path), "File type not supported")
})

test_that("parse_file throws error for non existing file", {
    file_path <- "../data/imaginary_file.xlsx"
    expect_error(parse_file(file_path), "File does not exist")
})

test_that("parse_file throws error for invalid data", {
    mockr::local_mock(
        synthesize_wishes = function(wishes) stop("Unknown filière")
    )
    expect_error(parse_file("../data/test_file.xlsx"), "Error while synthesizing the wishes \\(l\\.1\\): Unknown filière")
})

# TODO: Add tests for other failure modes and ADSPlanner import
