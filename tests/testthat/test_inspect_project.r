
test_that("complete projects are complete", {
  log <- inspect_project("./finished_double_transcription", write_reports = FALSE)
  expect_true(log$project_structure_correct)
  expect_true(log$n_commits == 24)
  expect_true(log$transcription1_complete)
  expect_true(log$transcription2_complete)
  expect_true(log$transcription_merged_complete)
  expect_true(log$transcription_complete)

  log <- inspect_project("./finished_single_transcription", write_reports = FALSE)
  expect_true(log$project_structure_correct)
  expect_true(log$transcription1_complete)
  expect_true(log$transcription_complete)

})

test_that("unstaged changes detected", {
  log <- inspect_project("./unstaged_changes", write_reports = FALSE)
  expect_false(log$all_changes_committed)
})

test_that("no metadata returns an error", {
  expect_error(inspect_project("./no_metadata", write_reports = FALSE))
})

test_that("bad structure is detected as such", {
  log <- inspect_project("./bad_structure", write_reports = FALSE)
  expect_false(log$is_git_repo)
  expect_false("transcription_complete" %in% names(log))
})

test_that("empty project has correct structure but nothing else", {
  log <- inspect_project("./empty_project", write_reports = FALSE)
  expect_true(log$project_structure_correct)
  expect_false(log$scanning_complete)
  expect_false(log$double_transcription_started)
  expect_false(log$transcription1_complete)
  expect_false(log$transcription2_complete)
  expect_false(log$transcription_merged_complete)
  expect_false(log$transcription_complete)
})

test_that("unfinished are unfinished", {
  # single transcription has begun, but is not finished
  log <- inspect_project("./unfinished_single_transcription", write_reports = FALSE)
  expect_true(log$project_structure_correct)
  expect_true(log$scanning_complete)
  expect_false(log$double_transcription_started)
  expect_false(log$transcription1_complete)
  expect_false(log$transcription2_complete)
  expect_false(log$transcription_complete)

  # double transcription has begun, but is not finished
  log <- inspect_project("./unfinished_double_transcription", write_reports = FALSE)
  expect_true(log$project_structure_correct)
  expect_true(log$transcription1_complete)
  expect_true(log$double_transcription_started)
  expect_false(log$transcription2_complete)
  expect_false(log$transcription_merged_complete)
  expect_false(log$transcription_complete)

  # scanning is not complete, but all scans have been transcribed
  log <- inspect_project("./unfinished_scanning_all_scans_double_transcribed", write_reports = FALSE)
  expect_true(log$project_structure_correct)
  expect_false(log$scanning_complete)
  expect_true(log$double_transcription_started)
  expect_false(log$transcription1_complete) # because of n_interviews_handcount
  expect_false(log$transcription2_complete) # because of n_interviews_handcount
  expect_false(log$transcription_merged_complete) # because of n_interviews_handcount
  expect_false(log$transcription_complete) # because of n_interviews_handcount

  # scanning is not complete, transcription2 is not complete
  log <- inspect_project("./unfinished_scanning_unfinished_double_transcription", write_reports = FALSE)
  expect_true(log$project_structure_correct)
  expect_false(log$scanning_complete)
  expect_true(log$double_transcription_started)
  expect_false(log$transcription1_complete) # because of n_interviews_handcount
  expect_false(log$transcription2_complete) # because yamls < pdfs
  expect_false(log$transcription_merged_complete) # because no yamls at all
  expect_false(log$transcription_complete) # because transcription_merged_complete is false

})
