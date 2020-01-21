

init_project <- function(path = ".", double_transcription = TRUE) {

  current_directory <- getwd()
  if (!file.exists(path)) dir_init(path)
  setwd(path)

  contents <- dir(all.files = TRUE)

  init_okay <- !any(contents %in% c(".git", "0_metadata", "0_scripts", "1_primary_sources",
    "2_data_cleaning", "3_relational_tables"))

  if (init_okay) {

    system("git init")
    write_gitignore()

    double_transcription <- TRUE

    dir_init("./0_metadata")
    # create a YAML file with blanks

    meta <- list(
      project_name = "untitled ecodata project",
      principal_investigator = "",
      n_interviews_handcount = "",
      interview_date_key = ""
    )

    write_yaml(meta, "0_metadata/project_overview.yaml")

    dir_init("./1_primary_sources")
    dir_init("./1_primary_sources/0_raw")
    dir_init("./1_primary_sources/1_pdf")
    dir_init("./1_primary_sources/2_transcription1")
    dir_init("./1_primary_sources/2_transcription1/1_pdf")
    dir_init("./1_primary_sources/2_transcription1/1_pdf/0_completed")
    dir_init("./1_primary_sources/2_transcription1/2_yaml")
    if (double_transcription) {
      dir_init("./1_primary_sources/2_transcription2")
      dir_init("./1_primary_sources/2_transcription2/1_pdf")
      dir_init("./1_primary_sources/2_transcription2/1_pdf/0_completed")
      dir_init("./1_primary_sources/2_transcription2/2_yaml")
    }
    dir_init("./2_data_cleaning")
    dir_init("./3_relational_tables")

    print("ecodata project initialized")

  } else {
    setwd(current_directory)
    stop("this is not an empty ecodata project! cannot initialize without losing data")
  }

  setwd(current_directory)

}
