
test_that("complete projects are complete", {
  log <- inspect_project("./finished_double_transcription", write_reports = FALSE)
  expect_true(log$project_structure_correct)
  expect_true(log$all_pdfs_hashed)
  expect_true(log$transcription1_complete)
  expect_true(log$transcription2_complete)
  expect_true(log$transcription_merged_complete)
  expect_true(log$transcription_complete)

  log <- inspect_project("./finished_single_transcription", write_reports = FALSE)
  expect_true(log$project_structure_correct)
  expect_true(log$all_pdfs_hashed)
  expect_true(log$transcription1_complete)
  expect_true(log$transcription_complete)

})

test_that("unstaged changes detected", {
  log <- inspect_project("./unstaged_changes", write_reports = FALSE)
  expect_false(out$all_changes_committed)
})

test_that("no metadata returns an error", {
  expect_error(inspect_project("./no_metadata", write_reports = FALSE))
})

test_that("bad structure is detected as such", {
  log <- inspect_project("./bad_structure", write_reports = FALSE)
  expect_false(log$project_structure_correct)
  expect_false("transcription_complete" %in% names(log))
})

test_that("empty project has correct structure but nothing else", {
  log <- inspect_project("./empty_project", write_reports = FALSE)
  expect_true(log$project_structure_correct)
  expect_false(log$scanning_complete)
  expect_true(is.na(log$all_pdfs_hashed))
  expect_true(is.na(log$transcription1_complete))
  expect_true(is.na(log$transcription2_complete))
  expect_true(is.na(log$transcription_merged_complete))
  expect_true(is.na(log$transcription_complete))
})

test_that("unfinished are unfinished", {
  log <- inspect_project("./unfinished_double_transcription", write_reports = FALSE)
  expect_true(log$project_structure_correct)
  expect_true(log$all_pdfs_hashed)
  expect_true(log$transcription1_complete)
  expect_false(log$transcription2_complete)
  expect_true(is.na(log$transcription_merged_complete))
  expect_true(log$transcription_complete)

  log <- inspect_project("./unfinished_single_transcription", write_reports = FALSE)
  expect_true(log$project_structure_correct)
  expect_true(log$all_pdfs_hashed)
  expect_false(log$transcription1_complete)
  expect_true(is.na(log$transcription2_complete))
  expect_false(log$transcription_complete)

  log <- inspect_project("./unfinished_scanning_finished_double_transcription", write_reports = FALSE)
  expect_true(log$project_structure_correct)
  expect_true(log$all_pdfs_hashed)
  expect_false(log$scanning_complete)
  expect_true(log$transcription1_complete)
  expect_true(log$transcription2_complete)
  expect_true(log$transcription_merged_complete)
  expect_true(log$transcription_complete)
})
