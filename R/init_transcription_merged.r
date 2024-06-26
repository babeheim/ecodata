
init_transcription_merged <- function(ignore_capitalizations = TRUE, context = FALSE) {

  # need failure conditions

  dir_init("./3_transcription_merged")
  dir_init("./3_transcription_merged/0_completed")
  dir_init("./3_transcription_merged/1_diff")
  dir_init("./3_transcription_merged/1_pdf")
  dir_init("./3_transcription_merged/2_yaml")

  # check that every file is duplicated

  yamls1 <- list.files("./2_transcription1/2_yaml")
  yamls2 <- list.files("./2_transcription2/2_yaml")

  identical(yamls1, yamls2)

  # copy pdfs - they all should be inside a pdf/ folder (no subfolders or mixed folders)
  pdfs <- list.files("./1_pdf", full.names = TRUE)
  file.copy(pdfs, "./3_transcription_merged")

  # copy yamls - they all should be inside a yaml/ folder (no subfolders or mixed folders)
  yamls1 <- list.files("./2_transcription1/2_yaml", full.names = TRUE)
  yamls2 <- list.files("./2_transcription2/2_yaml", full.names = TRUE)

  for (i in 1:length(yamls2)) {
    
    dat1 <- readLines(yamls1[i])
    dat2 <- readLines(yamls2[i])
    if (any(grepl("^transcriber\\s*:", dat1))) {
      t1_location <- grep("^transcriber\\s*:", dat1)
      dat1[t1_location] <- gsub("^transcriber", "transcriber1", dat1[t1_location])
    } else {
      stop("'transcriber' field missing from", yamls1[i])
    }
    if (any(grepl("^transcriber\\s*:", dat2))) {
      t2_location <- grep("^transcriber\\s*:", dat2)
      dat2[t2_location] <- gsub("^transcriber", "transcriber2", dat2[t2_location])
      dat2 <- c(dat2[1:(t2_location-1)], dat1[t1_location], dat2[t2_location:length(dat2)])
    } else {
      stop("'transcriber' field missing from", yamls2[i])
    }

    writeLines(dat2, file.path("3_transcription_merged", basename(yamls2[i])))

  }

  # create diff comparisons between yamls1 and yamls2
  diffnames <- paste0("./3_transcription_merged/",
    gsub("\\.yaml$", ".diff", basename(yamls2)))
  
  if (context) {
    for (i in 1:length(diffnames)) {
      if (ignore_capitalizations) {
        system(paste("diff -aibw --suppress-common-lines -c", yamls1[i], yamls2[i],
        ">", diffnames[i])) 
      } else {
        system(paste("diff -abw --suppress-common-lines -c", yamls1[i], yamls2[i],
        ">", diffnames[i])) 
      }
    }
  } else {
    for (i in 1:length(diffnames)) {
      if (ignore_capitalizations) {
        system(paste("diff -aibw --suppress-common-lines", yamls1[i], yamls2[i],
        ">", diffnames[i])) 
      } else {
        system(paste("diff -abw --suppress-common-lines", yamls1[i], yamls2[i],
        ">", diffnames[i])) 
      }
    }
  }

}
