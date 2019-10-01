
test_that("complete project is complete", {
  log <- inspect_project("./finished_project", write_reports = FALSE)
  expect_true(log$project_structure_correct)
  expect_true(log$all_filenames_hashed)
  expect_true(log$transcription1_complete)
  expect_true(log$transcription2_complete)
  expect_true(log$transcription_merge_complete)
  expect_true(log$transcription_complete)
})

test_that("empty project has correct structure", {
  log <- inspect_project("./empty_project", write_reports = FALSE)
  expect_true(log$project_structure_correct)

})
