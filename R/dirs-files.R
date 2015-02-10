#' file_path_sans_ext
#' @name file_path_sans_ext
#' @description borrowed from tools package
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
file_path_sans_ext <- function (x) 
{
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

#' file_ext
#' @name file_ext
#' @description borrowed from tools package
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
file_ext <- function (x) 
{
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}


#' write_file
#' @name write_file
#' @description write_file
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
write_file <- function (x, file) 
{
  dir.create(dirname(file), recursive = TRUE)
  writeLines(x, file)
}

#' read_file
#' @name read_file
#' @description read_file
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
read_file <- function (file, warn = F, ...) 
{
  paste(readLines(file, warn = warn, ...), collapse = "\n")
}

#' Read contents of a system file into a character string
#' @name read_sysfile
#' @description read_sysfile
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
read_sysfile <- function(..., package = "utter"){
  if (is.null(package)){
    path = file.path(...)
  } else {
    path = system.file(..., package = package)
  }
  #   read_file(path)
  path
}

#' Copy directories recursively, creating a new directory if not already there
#' @name copy_dirs
#' @description copy_dirs
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
copy_dirs <- function(from, to){
  for(i in length(from)){
    copy_dir(from[i],to[i])
  }  
}

#' Copy directories recursively, creating a new directory if not already there
#' @name copy_dir
#' @description copy_dir
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
copy_dir <- function(from, to, overwrite = TRUE){
  #   if (!(file.exists(to))){
  dir.create(to, recursive = TRUE)
  message('Copying files from:',from,  ' to: ' , to, '...')
  file.copy(list.files(from, full.names = T), to, recursive = TRUE, overwrite = TRUE)
  #   }
}


#' Copy directories recursively, creating a new directory if not already there
#' @name copy_files
#' @description copy_files
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
copy_files <- function(from, to, overwrite = TRUE){
  for(i in length(from)){
    copy_file(from[i],to[i], overwrite = overwrite)
  }  
}

#' Copy directories recursively, creating a new directory if not already there
#' @name copy_file
#' @description copy_file
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
copy_file <- function(from, to, overwrite = TRUE){
  file.copy(from, to, overwrite = TRUE)
}

#' Create dirs
#' @name create_dirs
#' @description create_dirs
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
create_dirs <- function(path, dirs){
  lapply(dirs, function(folder){ 
    filePath <- file.path(path,folder)
    dir.create(file.path(filePath), showWarnings = FALSE)  
  })
}


#' List directories in a directory 
#' @name list_dirs
#' @description list_dirs
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
list_dirs <- function(path=".", recursive=TRUE, fullPath = TRUE) {
  file_list <- list.files(path, all.files = FALSE, full.names = TRUE, include.dirs = TRUE, recursive = recursive)
  file_info <- file.info(file_list)
  dirs <- row.names(file_info[file_info$isdir,])
  if(!fullPath){ dirs <- basename(dirs)}
  dirs
}

#' List directories in a directory 
#' @name list_files
#' @description list_files
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
list_files <- function (path = ".", recursive = TRUE, fullPath = TRUE) 
{
  file_list <- list.files(path, all.files = TRUE, full.names = TRUE, 
                          include.dirs = FALSE, recursive = recursive)
  file_list
}

