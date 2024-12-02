# Testing read_file

test_that("read_file reads xlsx files correctly", {
    file_path <- "../data/test_file.xlsx"
    result <- read_file(file_path)
    expect_true(is.data.frame(result))
    expect_equal(ncol(result), 18)
    expect_equal(result, read_file_output)
})

test_that("read_file reads ods files correctly", {
    file_path <- "../data/test_file.ods"
    result <- read_file(file_path)
    expect_true(is.data.frame(result))
    expect_equal(ncol(result), 18)
    expect_equal(result, read_file_output)
})

test_that("read_file throws error for unsupported file types", {
    file_path <- "../data/test_file.csv"
    expect_error(read_file(file_path), "File type not supported")
})

test_that("read_file throws error for non existing file", {
    file_path <- "../data/imaginary_file.xlsx"
    expect_error(read_file(file_path), "File does not exist")
})

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
    mockr::local_mock(read_file = function(file_path) read_file_output)
    result <- parse_file("../data/test_file.xlsx")
    expect_equal(result, parse_file_output)
})

test_that("parse_file throws error for invalid input file type", {
    mockr::local_mock(read_file = function(file_path) stop("File type not supported"))
    expect_error(parse_file("../data/test_file.csv"), "Error while reading the file : File type not supported")
})

test_that("parse_file throws error for non existent file", {
    mockr::local_mock(read_file = function(file_path) stop("File does not exist"))
    expect_error(parse_file("../data/imaginary_file.xlsx"), "Error while reading the file : File does not exist")
})

test_that("parse_file throws error for invalid data", {
    mockr::local_mock(
        read_file = function(file_path) read_file_output_invalid,
        synthesize_wishes = function(wishes) stop("Unknown filière")
    )
    expect_error(parse_file("../data/test_file.xlsx"), "Error while synthesizing the wishes \\(l\\.1\\): Unknown filière")
})
