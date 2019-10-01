
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
