
rename_pdfs <- function(path) {

  files <- list.files(path, pattern="*.pdf|*.PDF", full.names = TRUE, recursive = FALSE)

  rename_counter <- 0
  unchanged_counter <- 0

  for(i in 1:length(files)){
      hash <- digest(files[i], algo = "sha1", file = TRUE)
      hash_name <- paste(substr(hash, 1, 11) ,".pdf", sep = "") 
      hash_name <- file.path(path, hash_name)
      if(files[i] != hash_name){ 
          file.rename(files[i], hash_name)
          rename_counter <- rename_counter + 1
      } else {
          unchanged_counter <- unchanged_counter + 1
      }
  }

  print(paste0(rename_counter, " files have been renamed"))
  print(paste0(unchanged_counter, " files unchanged"))

}