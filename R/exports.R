#' %||%
# @name this-if-null
#' @rdname this-if-null
#' @description this or that: %||% 
#' @param x any
#' @param y any
#' @return y if x is undefined
#' @export
`%||%` <- function (x, y) 
{
  if (is.empty(x)) 
    return(y)
  else if (is.null(x) || is.na(x)) 
    return(y)
  else if (class(x) == "character" && nchar(x) == 0) 
    return(y)
  else x
}


#' Sets up directory structure for a new Borges site
#' is.empty
#' @name is.empty
#' @description is.empty
#' @param pos string, "first" or "last"
#' @return bool
#' @export
#' @examples \dontrun{
#' }
is.empty <- function (x) 
{
  !as.logical(length(x))
}

#' is.empty
#' @name isnt.empty
#' @description isnt.empty
#' @param x
#' @return bool
#' @export
#' @examples \dontrun{
#' }
isnt.empty <- function(x){
  #   !is.null(x)
  as.logical(length(x))  
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

#' call_fun
#' @name call_fun
#' @description call_fun
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
call_fun <- function(f, ...) f(...)

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

#' power_set
#' @name power_set
#' @description power_set
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
power_set <- function(set) { 
  n <- length(set)
  masks <- 2^(1:n-1)
  lapply( 1:2^n-1, function(u) set[ bitwAnd(u, masks) != 0 ] )
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

#' List to Data Frame 
#' @name list_to_df
#' @description list_to_df
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
list_to_df <- function(l){
  plyr::ldply(l, data.frame)
}

#' List to Data Frame 
#' @name list_to_df2
#' @description list_to_df2
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
list_to_df2 <- function(l){
  llen <- unlist(lapply(l, length))
  l <- l[llen ==1]
  as.data.frame(l)
}


#' naToEmpty
#' @name naToEmpty
#' @description naToEmpty
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
naToEmpty <- function(df, empty = c(" ")){
  df[is.na(df)] <- ""
  df[df %in% empty] <- ""
  df
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







#' Random name
#' @name random_name
#' @description random_name
#' @param n
#' @return string
#' @export
#' @examples \dontrun{
#' }
random_name <- function(n=31){
  x <- c(letters, LETTERS, seq(0,9))
  paste(sample(x,n,replace=TRUE),collapse="")
}







