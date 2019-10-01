
init_transcription_merged <- function(ignore_capitalizations = TRUE) {

  dir_init("./3_transcription_merged")
  dir_init("./3_transcription_merged/1_diff")
  dir_init("./3_transcription_merged/1_yaml")
  dir_init("./3_transcription_merged/1_pdf")

  # check that every file is duplicated

  yamls1 <- list.files("./2_transcription1/yaml")
  yamls2 <- list.files("./2_transcription2/yaml")

  identical(yamls1, yamls2)

  # copy pdfs - they all should be inside a pdf/ folder (no subfolders or mixed folders)
  pdfs <- list.files("./1_pdf", full.names = TRUE)
  file.copy(pdfs, "./3_transcription_merged")

  # copy yamls - they all should be inside a yaml/ folder (no subfolders or mixed folders)
  yamls1 <- list.files("./2_transcription1/2_yaml", full.names = TRUE)
  yamls2 <- list.files("./2_transcription2/2_yaml", full.names = TRUE)

  file.copy(yamls2, "./3_transcription_merged")

  # create diff comparisons between yamls1 and yamls2
  diffnames <- paste0("./3_transcription_merged/",
    gsub("\\.yaml$", ".diff", basename(yamls2)))

  for (i in 1:length(diffnames)) {

    if (ignore_capitalizations) {
      system(paste("diff -aiEbw --suppress-common-lines -c", yamls1[i], yamls2[i],
      ">", diffnames[i])) 
    } else {
      system(paste("diff -aEbw --suppress-common-lines -c", yamls1[i], yamls2[i],
      ">", diffnames[i])) 
    }
  }

}