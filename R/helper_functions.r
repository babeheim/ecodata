
bad_transcriber <- function(transcriber) {
  if (is.null(transcriber)) {
    out <- TRUE
  } else {
    transcriber <- gsub("'", "", transcriber)
    out <- (is.na(transcriber) | nchar(transcriber) == 0)
  }
  return(out)
}

bad_hash <- function(hash, hash_length = 7) {
  if (is.null(hash)) {
    out <- TRUE
  } else {
    hash <- gsub("'", "", hash)
    out <- nchar(hash) != hash_length
  }
  return(out)
}

bad_date <- function(date) {
  if (is.null(date)) {
    out <- TRUE
  } else {
    out <- is.na(as.Date(date, "%Y-%m-%d"))
  }
  return(out)
}

bad_stamp <- function(stamp_number) {
  if (is.null(stamp_number)) {
    out <- TRUE
  } else {
    out <- nchar(stamp_number) != 6 | is.na(as.numeric(stamp_number))
  }
  return(out)
}

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

dir_init <- function(path, verbose=FALSE){
  if(substr(path, 1, 2)!='./') stop('path argument must be formatted
    with "./" at beginning')
  contents <- dir(path, recursive=TRUE)
  if(verbose){
    if(length(contents)==0) print(paste('folder ', path, ' created.', sep=""))
    if(length(contents)>0) print(paste('folder ', path, ' wiped of ', length(contents), ' files/folders.', sep=""))
  }
  if(dir.exists(path)) unlink(path, recursive=TRUE)
  dir.create(path)
}

# check that the project folder is empty

write_gitignore <- function() {
  writeLines("
# Compiled source #
###################
*.com
*.class
*.dll
*.exe
*.o
*.so

# Packages #
############
# it's better to unpack these files and commit the raw source
# git has its own built in compression methods
*.7z
*.dmg
*.gz
*.iso
*.jar
*.rar
*.tar
*.zip

# Logs and databases #
######################
*.log
*.sql
*.sqlite

# OS generated files #
######################
.DS_Store
.DS_Store?
._*
.Spotlight-V100
.Trashes
ehthumbs.db
Thumbs.db

*.gif
*.png
*.jpg
*.jpeg

# common binaries
*.pdf
*.docx
*.xls

*.PDF
*.JPG
*.xlsx
*.xlsx alias
*.wma
*.mp3
*.mp4
*.MP4
*.MOV
*.WAV
*.THM
*.MTS
*.bmp
*.tiff
*.3gp
*.LRV
*.RW2
*.pkf
*.psd
*.wmv
*.mvp
*.prproj
*.jpg.psd

# R Kruft
*.Rhistory
*.RData
*.pdf
*.Rapp.history
*.Rhistory
*.Rproj.user
", ".gitignore"
)
print(".gitignore created")}
