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
#' clean_ws
#' @name clean_ws
#' @description is.empty
#' @param pos string, "first" or "last"
#' @return bool
#' @export
#' @examples \dontrun{
#' }
clean_ws <- function(keep = c()){
  vars <- keep
  lapply(vars, function(var){
    rm(list=setdiff(ls(), var), envir = .GlobalEnv)    
  })
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
  if("data.frame" %in% class(x)){
    out <- apply(x,1,function(r){
      any(unlist(lapply(r, is.empty)))
    })
    return(out)
  }
  if(x == "") return(TRUE)
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






#' call_fun
#' @name call_fun
#' @description call_fun
#' @param string
#' @return string
#' @export
#' @examples \dontrun{
#' }
call_fun <- function(f, ...) f(...)



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

#' try2
#' @name try2
#' @description try2
#' @param n
#' @return string
#' @export
#' @examples \dontrun{
#' }
try2 <- function(code, silent = FALSE) {
  tryCatch(code, error = function(c) {
    msg <- conditionMessage(c)
    if (!silent) gsub( "Error in try({ : ","",message(c), fixed="TRUE")
    invisible(structure(msg, class = "try-error"))
  })
}




