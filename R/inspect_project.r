
inspect_project <- function(path, write_reports = FALSE, outdir = ".") {

  current_directory <- getwd()
  setwd(path)

  if (!file.exists("./0_metadata/project_overview.yaml")) {
    setwd(current_directory)
    stop("0_metadata/project_overview.yaml not found!")
  }
  meta <- yamltools::read_yaml("./0_metadata/project_overview.yaml")

  if (is.null(meta$n_interviews_handcount)) meta$n_interviews_handcount <- NA
  if (is.null(meta$interview_date_key)) meta$interview_date_key <- NA

  ############################
  # initialize daemon report

  out <- list(
    daemon_report_date = Sys.time(),
    project_name = meta$project_name,
    principal_investigator = meta$principal_investigator,
    interview_start_date = NA,
    interview_end_date = NA,
    n_interviews_handcount = as.numeric(meta$n_interviews_handcount),
    transcribers = NA,
    n_transcribers = 0,
    reviewers = NA,
    n_reviewers = 0,
    n_commits = 0,
    date_first_commit = NA,
    date_last_commit = NA,
    all_changes_committed = NA
  )

  ############################
  # project initialization checks

  # bug: what if the ecodata project is *inside* a git repo but isn't a git repo, e.g. brian's GPSLog
  out$is_git_repo <- file.exists(".git")
  out$has_gitignore <- file.exists(".gitignore")
  git_set_up <- out$is_git_repo & out$has_gitignore

  out$has_metadata_folder <- file.exists("./0_metadata")
  out$has_primary_sources_folder <- file.exists("./1_primary_sources")
  has_folders <- out$has_metadata_folder & out$has_primary_sources_folder

  out$project_structure_correct <- git_set_up & has_folders

  if (out$project_structure_correct) {

    out$has_template_yaml <- length(dir("./0_metadata", pattern = ".*template.*\\.yaml$")) > 0
    out$has_template_pdf <- length(dir("./0_metadata", pattern = ".*template.*\\.pdf$")) > 0

    ############################
    # save the commit history

    parse_log_simple() %>% as.data.frame() -> commits
    out$n_commits <- nrow(commits)

    if (out$n_commits > 0) {
      commits$timestamp <- as.POSIXlt(commits$date)
      commits$date <- substr(commits$timestamp, 1, 10)
      commits$project_name <- meta$project_name
      commits$principal_investigator <- meta$principal_investigator
      if (write_reports) write.csv(commits, file.path(outdir, "project_commits.csv"),
        row.names = FALSE)
    }

    check_unstaged <- "if [[ `git status --porcelain` ]]; then echo \"TRUE\"; \
      else echo \"FALSE\"; fi"
    out$all_changes_committed <- !as.logical(system(check_unstaged, intern = TRUE))

    ############################
    # catalogue all files created so far

    pdfs <- list.files("./1_primary_sources/1_pdf", full.names = TRUE,
      pattern = "*.pdf$", recursive = TRUE)

    yamls_transcription1 <- c(
      list.files("./1_primary_sources/2_transcription1/1_pdf/0_completed",
        pattern = ".yaml", recursive = TRUE, full.names = TRUE),
      list.files("./1_primary_sources/2_transcription1/2_yaml",
        pattern = ".yaml", recursive = TRUE, full.names = TRUE)
    )

    yamls_transcription2 <- c(
      list.files("./1_primary_sources/2_transcription2/1_pdf/0_completed",
        pattern = ".yaml", recursive = TRUE, full.names = TRUE),
      list.files("./1_primary_sources/2_transcription2/2_yaml",
        pattern = ".yaml", recursive = TRUE, full.names = TRUE)
    )

    yamls_merged <- c(
      list.files("./1_primary_sources/3_transcription_merged",
        pattern = ".yaml", recursive = TRUE, full.names = TRUE)
    )

    csvs <- list.files("./3_relational_tables", pattern = "*\\.csv$", full.names = TRUE)

    files <- c(pdfs, yamls_transcription1, yamls_transcription2, yamls_merged, csvs)

    if (length(files) > 0) {
      project_files <- file.info(files)

      project_files <- project_files[order(project_files$ctime), ]

      project_files$full_filename <- rownames(project_files)
      project_files$filename <- basename(project_files$full_filename)
      project_files$dirname <- dirname(project_files$full_filename)
      project_files$type <- substr(project_files$filename, regexpr("\\.",
        project_files$filename) + 1, nchar(project_files$filename))

      project_files$project_name <- meta$project_name
      project_files$principal_investigator <- meta$principal_investigator

      project_files <- select(project_files, project_name, principal_investigator, filename,
        dirname, type, full_filename, size, ctime)

      if (write_reports) write.csv(project_files,
        file.path(outdir, "project_files.csv"), row.names = FALSE)

      if (out$n_commits > 0) {
        out$date_first_commit <- as.character(as.Date(min(commits$timestamp)))
        out$date_last_commit <- as.character(as.Date(max(commits$timestamp)))
      }

    }

    ############################
    # track transcription progress

    pdfs <- list.files("./1_primary_sources/1_pdf", full.names = TRUE,
      pattern = "*.pdf$", recursive = TRUE)

    pdf_hashes <- pdfs %>% basename %>% substr(1, 7)

    out$n_interviews_scanned <- length(pdfs)
    out$n_interviews_unscanned <- out$n_interviews_handcount - out$n_interviews_scanned

    out$scanning_complete <- FALSE
    if (!is.na(out$n_interviews_unscanned)) out$scanning_complete <- out$n_interviews_handcount == out$n_interviews_scanned

    out$all_pdfs_hashed <- NA
    if (length(pdfs) > 0) out$all_pdfs_hashed <- all(nchar(basename(pdfs)) == 11)

    # inspect completed yamls in 2_transcription1

    out$n_transcription1_transcribed <- 0
    out$transcription1_yamls_named_correctly <- NA
    out$transcription1_yamls_valid <- NA
    out$transcription1_complete <- NA

    yamls1 <- c(
      list.files("./1_primary_sources/2_transcription1/1_pdf/0_completed",
        pattern = ".yaml", recursive = TRUE, full.names = TRUE),
      list.files("./1_primary_sources/2_transcription1/2_yaml",
        pattern = ".yaml", recursive = TRUE, full.names = TRUE)
    )
    out$n_transcription1_transcribed <- length(unique(basename(yamls1)))
    if (out$n_transcription1_transcribed > 0) {
      files <- yamls1
      files %>% basename() %>% substr(1, 7) -> yaml_filename_hashes
      loads <- rep(NA, length(files))
      hash_ok <- rep(NA, length(files))
      transcriber_ok <- rep(NA, length(files))
      stamp_ok <- rep(NA, length(files))
      for (i in 1:length(files)) {
        loads[i] <- yamltools::yaml_loads(files[i])
        if (loads[i]) {
          data <- read_yaml(files[i])
          hash_ok[i] <- !bad_hash(data, hash_length = 7) &
            data$pdf_hash == yaml_filename_hashes[i]
          transcriber_ok[i] <- !bad_transcriber(data)
    #     stamp_ok[i] <- !bad_stamp(data)
        }
      }
      if (any(!loads, na.rm = TRUE)) {
        print(paste("invalid yamls:", files[!loads]))
      }
      if (any(!hash_ok, na.rm = TRUE)) {
        print(paste("invalid pdf_hash values or filenames:", files[!hash_ok]))
      }
      if (any(!stamp_ok, na.rm = TRUE)) {
        print(paste("invalid stamp number:", files[!hash_ok]))
      }
      if (any(!transcriber_ok, na.rm = TRUE)) {
        print(paste("missing transcriber information:", files[!transcriber_ok]))
      }
      out$transcription1_yamls_named_correctly <- all(yaml_filename_hashes %in% pdf_hashes)
      out$transcription1_yamls_valid <- out$transcription1_yamls_named_correctly &
        all(loads) & all(hash_ok) & all(transcriber_ok)
      out$transcription1_complete <- all(pdf_hashes %in% yaml_filename_hashes) &
        out$transcription1_yamls_valid
      # silke wants the transcriber names for each transcription job separated here!
    }

    # can the yaml be transformed to a json file, and if so, DOES THAT JSON LOAD PROPERLY

    # inspect completed yamls in 2_transcription2

    out$n_transcription2_transcribed <- 0
    out$transcription2_yamls_named_correctly <- NA
    out$transcription2_yamls_valid <- NA
    out$transcription2_complete <- NA

    yamls2 <- c(
      list.files("./1_primary_sources/2_transcription2/1_pdf/0_completed",
        pattern = ".yaml", recursive = TRUE, full.names = TRUE),
      list.files("./1_primary_sources/2_transcription2/2_yaml",
        pattern = ".yaml", recursive = TRUE, full.names = TRUE)
    )
    out$n_transcription2_transcribed <- length(unique(basename(yamls2)))
    if (out$n_transcription2_transcribed > 0) {
      files <- yamls2
      files %>% basename() %>% substr(1, 7) -> yaml_filename_hashes
      loads <- rep(NA, length(files))
      hash_ok <- rep(NA, length(files))
      transcriber_ok <- rep(NA, length(files))
      stamp_ok <- rep(NA, length(files))
      for (i in 1:length(files)) {
        loads[i] <- yamltools::yaml_loads(files[i])
        if (loads[i]) {
          data <- read_yaml(files[i])
          hash_ok[i] <- !bad_hash(data, hash_length = 7) &
            data$pdf_hash == yaml_filename_hashes[i]
          transcriber_ok[i] <- !bad_transcriber(data)
     #    stamp_ok[i] <- !bad_stamp(data)
        }
      }
      if (any(!loads, na.rm = TRUE)) {
        print(paste("invalid yamls:", files[!loads]))
      }
      if (any(!hash_ok, na.rm = TRUE)) {
        print(paste("invalid pdf_hash values or filenames:", files[!hash_ok]))
      }
      if (any(!stamp_ok, na.rm = TRUE)) {
        print(paste("invalid stamp number:", files[!hash_ok]))
      }
      if (any(!transcriber_ok, na.rm = TRUE)) {
        print(paste("missing transcriber information:", files[!transcriber_ok]))
      }
      out$transcription2_yamls_named_correctly <- all(yaml_filename_hashes %in% pdf_hashes)
      out$transcription2_yamls_valid <- out$transcription2_yamls_named_correctly &
        all(loads) & all(hash_ok) & all(transcriber_ok)
      out$transcription2_complete <- all(pdf_hashes %in% yaml_filename_hashes) &
        out$transcription2_yamls_valid
    }

    # inspect completed yamls in 3_transcription_merged

    out$n_transcription_merged <- 0
    out$merged_yamls_named_correctly <- NA
    out$merged_yamls_valid <- NA
    out$transcription_merged_complete <- NA

    yamls_merged <- list.files("./1_primary_sources/3_transcription_merged/2_yaml",
        pattern = ".yaml", recursive = TRUE, full.names = TRUE)
    out$n_transcription_merged <- length(unique(basename(yamls_merged)))
    if (out$n_transcription_merged > 0) {
      files <- yamls_merged
      files %>% basename() %>% substr(1, 7) -> yaml_filename_hashes
      loads <- rep(NA, length(files))
      hash_ok <- rep(NA, length(files))
      transcriber_ok <- rep(NA, length(files))
      reviewer_ok <- rep(NA, length(files))
      stamp_ok <- rep(NA, length(files))
      for (i in 1:length(files)) {
        loads[i] <- yamltools::yaml_loads(files[i])
        if (loads[i]) {
          data <- read_yaml(files[i])
          hash_ok[i] <- !bad_hash(data, hash_length = 7) &
            data$pdf_hash == yaml_filename_hashes[i]
          transcriber_ok[i] <- !bad_transcriber(data)
     #    reviewer_ok[i] <- !bad_reviewer(data)
     #    stamp_ok[i] <- !bad_stamp(data)
        }
      }
      if (any(!loads, na.rm = TRUE)) {
        print(paste("invalid yamls:", files[!loads]))
      }
      if (any(!hash_ok, na.rm = TRUE)) {
        print(paste("invalid pdf_hash values or filenames:", files[!hash_ok]))
      }
      if (any(!stamp_ok, na.rm = TRUE)) {
        print(paste("invalid stamp number:", files[!hash_ok]))
      }
      if (any(!transcriber_ok, na.rm = TRUE)) {
        print(paste("missing transcriber information:", files[!transcriber_ok]))
      }
      if (any(!reviewer_ok, na.rm = TRUE)) {
        print(paste("these files have missing reviewer information", files[!reviewer_ok]))
      }
      out$merged_yamls_named_correctly <- all(yaml_filename_hashes %in% pdf_hashes)
      out$merged_yamls_valid <- out$merged_yamls_named_correctly & all(loads) &
        all(hash_ok) & all(transcriber_ok)
      out$transcription_merged_complete <- all(pdf_hashes %in% yaml_filename_hashes) & out$merged_yamls_valid

    }

    if (is.na(out$transcription_merged_complete)) {
      out$transcription_complete <- out$transcription1_complete
    } else {
      out$transcription_complete <- out$transcription_merged_complete
    }

    ############################
    # extract interviews.csv data

    if (file.exists("./3_relational_tables/interviews.csv")) {

      ints <- read.csv("./3_relational_tables/interviews.csv", stringsAsFactors = FALSE)

      if (is.na(meta$interview_date_key) | !meta$interview_date_key %in% colnames(ints)) {
        print("interview date variable not found")
      } else {
        out$interview_start_date = sort(as.character(ints[[meta$interview_date_key]]))[1]
        out$interview_end_date = rev(sort(as.character(ints[[meta$interview_date_key]])))[1]
      }

      transcribers <- sort(unique(unlist(strsplit(ints$transcriber, ", "))))
      out$transcribers <- paste(transcribers, collapse = ", ")
      out$n_transcribers = length(transcribers)

      if (length(ints$reviewer) > 0) {
        reviewers <- sort(unique(unlist(strsplit(ints$reviewer, ", "))))
        out$reviewers <- paste(reviewers, collapse = ", ")
        out$n_reviewers = length(reviewers)
      }
    }

  }

  ############################
  # check relational integrity of tables

  # this has to be customized to the project I suppose...
  # out$has_relational_integrity <- FALSE

  ############################
  # report findings

  # some kind of global completion check?

  if (write_reports) write_json(out, file.path(outdir, "project_report.json"), pretty = TRUE)
  setwd(current_directory)

  return(out)
  print(paste(meta$project_name, "inspected!"))

}