#
# hamsteR
# Simple Module Management for R
#

# hidden environment used for this package only
.env = new.env(parent = emptyenv())












# .onLoad -----------------------------------------------
#' @title OnLoad
#' @description will be executed when the package is loaded
#' @param libname name of the library
#' @param pkgname name of the package
#' @details default function for package creation, here:
#' predefines the data structure for storing the loading states and sets the initial options
#' @rdname .onLoad

.onLoad <- function(libname, pkgname){

  # DEBUG
  # message("hamsteR.onLoad: loading...")

  # create basic data structure
  if ( ! exists("Modules", envir = .env) ){
    .env$Modules <- data.frame(
      loading_state=factor(levels = c("undefined", "loading", "loaded"), exclude=""),
      stringsAsFactors = FALSE
      )
    #packageStartupMessage("data structure prepared.")
  }

  op <- options()
  op.hamsteR <- list(
    hamsteR.path = "~/R-dev",
    hamsteR.install.args = "",
    hamsteR.name = "hamsteR",
    hamsteR.desc.author = "Marc O. R\U00FCdel <mor@uni-bremen.de> [aut, cre]",
    hamsteR.desc.license = "What license is it under?",
    hamsteR.file.ext.allowed = c("R"),
    hamsteR.debug = FALSE
  )
  toset <- !(names(op.hamsteR) %in% names(op))
  if(any(toset)) options(op.hamsteR[toset])

  # DEBUG
  # message("hamsteR.onLoad: finished")

  invisible()

}






# .onAttach --------------------------------------------------------------
#' @title OnAttach
#' @description will be executed when the package is attached (by \code{library} or \code{hamsteR::})
#' @inheritParams .onLoad
#' @details default function for package attachment, onyl shows an attachment message
#' @rdname .onAttach

.onAttach <- function(libname, pkgname){
  packageStartupMessage("hamsteR attached.")
}






# source_ifnotloaded -----------------------------------------------------
#' @title Conditional read and parse code from a file
#' @description comparable to \code{\link[base]{source}}, \code{source_ifnotloaded} reads and parses the code
#' of the given file, but only if it is not already marked as 'loaded'. The loading state
#' has to be set by \code{\link{set_loading_state}} and could be examined by \code{\link{loading_state_of}}.
#' @param file file name of the script file to load
#' @param force_load allows you to load the file even though it is marked as already loaded,
#' Default: FALSE
#' @param ... these parameters will be transferred to the base::source function
#' @details This is the main function of the package. source_ifnotloaded is
#' comparable with source of base R and it relays on it. The main differences
#' to base::source are
#' \itemize{
#' \item only files are allowed (no other type of connection, see \link{source})
#' \item  the named script will only be loaded, if a predefined
#' condition is fullfilled. We call this 'the script has a loading state'.
#' The loading state of a script can be manipulated (\code{\link{set_loading_state}}) and checked (\code{\link{loading_state_of}}).
#' }
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname source_ifnotloaded

source_ifnotloaded <- function(file, force_load=FALSE, ...) {

  # ----- Check Params -----------------------------------------------

  .check_parameter_type_chr_single(file, "file")

  if ( is.na(force_load) |
       ! is.logical(force_load) |
       length(force_load) != 1 ) {
    stop ("force_load parameter has to be just TRUE or FALSE.")
  }


  # ----- check data structure ---------------------------------------

  .check_data_structure()

  # ----- Check file name --------------------------------------------

  .check_filename_extension(file)

  # ----- module file exists? ----------------------------------------

  if (! file.exists(file)) {

    stop(paste0("file '", file,"' not found."))

  } # file.exists

  # ----- check already loaded ---------------------------------------

  # check if the module is already marked 'loaded'
  if ( ( ! is_managed(file) || ! loading_state_of(file) == "loaded") | # if dataset not marked as loaded
       force_load ){   # OR force load / load anyway

    # create new entry if not already managed
    .createIfNotFound(file, "loading_state")
    if (getOption("hamsteR.debug")) {message(paste0("hamsteR: loading '", file, "' ..."))} # DEBUG

    prev.loading.state <- loading_state_of(file)
    set_loading_state(file, "loading")
    base::source(file, ...)
    if (loading_state_of(file) == "loading") {
      set_loading_state(file, prev.loading.state)
      rm(prev.loading.state)
    }

  } # check or force_load

} # source_ifnotloaded








# .check_filename_extension, internal -------------------------------------------------------
#' @title Check filename extension
#' @description checks the extension of the given file name against allow extensions of the option 'hamsteR.file.ext.allowed'
#' @inheritParams source_ifnotloaded
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#'  getOption("hamsteR.file.ext.allowed")
#'  # [1] "R"
#'  .check_filename_extension("TEST.R") # == NULL
#'  .check_filename_extension("TEST") # -> ERROR
#'  .check_filename_extension("TEST.r") # -> ERROR
#'  .check_filename_extension("TEST.RR") # -> ERROR
#'
#'  options("hamsteR.file.ext.allowed" = c("R", "RR"))
#'  getOption("hamsteR.file.ext.allowed")
#'  # [1] "R", "RR"
#'  .check_filename_extension("TEST.R") # == NULL
#'  .check_filename_extension("TEST") # -> ERROR
#'  .check_filename_extension("TEST.r") # -> ERROR
#'  .check_filename_extension("TEST.RR") # -> ERROR

#'  options("hamsteR.file.ext.allowed" = c("*"))
#'  getOption("hamsteR.file.ext.allowed")
#'  # [1] "*"
#'  .check_filename_extension("TEST.R") # == NULL
#'  .check_filename_extension("TEST") # -> ERROR
#'  .check_filename_extension("TEST.r") # == NULL
#'  .check_filename_extension("TEST.RR") # == NULL
#'
#'  options("hamsteR.file.ext.allowed" = c("R","*"))
#'  getOption("hamsteR.file.ext.allowed")
#'  # [1] "R","*"
#'  .check_filename_extension("TEST.R") # == NULL
#'  .check_filename_extension("TEST") # -> ERROR
#'  .check_filename_extension("TEST.r") # == NULL
#'  .check_filename_extension("TEST.RR") # == NULL
#'
#'  options("hamsteR.file.ext.allowed" = NULL)
#'  getOption("hamsteR.file.ext.allowed")
#'  # [1] NULL
#'  .check_filename_extension("TEST.R") # -> ERROR
#'  .check_filename_extension("TEST") # == NULL
#'  .check_filename_extension("TEST.r") # -> ERROR
#'  .check_filename_extension("TEST.RR") # -> ERROR
#'
#'  options("hamsteR.file.ext.allowed" = c(""))
#'  getOption("hamsteR.file.ext.allowed")
#'  # [1] ""
#'  .check_filename_extension("TEST.R") # -> ERROR
#'  .check_filename_extension("TEST") # == NULL
#'  .check_filename_extension("TEST.r") # -> ERROR
#'  .check_filename_extension("TEST.RR") # -> ERROR
#'
#'  }
#' }
#' @rdname .check_filename_extension

.check_filename_extension <- function(file) {

  # ----- extract name and extension --------------------------

  file.name <- tools::file_path_sans_ext(file)
  file.extension <- tools::file_ext(file)

  # ----- check type of hamsteR.file.ext ------------------------

  allowed_file_extensions = getOption("hamsteR.file.ext.allowed")
  if ( ! is.null(allowed_file_extensions) &
       (
         ! is.atomic(allowed_file_extensions) |
         ! is.character(allowed_file_extensions)
       )
  ) {
    stop("option 'hamsteR.file.ext.allowed' not in expected character vector format.")
  }

  # ----- check file.extension  ---------------------------------

  if ( ! "*" %in% allowed_file_extensions &
       ! (file.extension=="" & is.null(allowed_file_extensions)) &
       ! file.extension %in% allowed_file_extensions
  ) {
    stop(paste0("current file extension '", file.extension, "' not allowed."))
  } #  * in allowed_file_extensions

}







# .createIfNotFound, internal --------------------------------------------------
#' @title create module
#' @description checks if the given file is already managed by \pkg{hamsteR},
#' if not creates an entry for the file with the given state in the given column
#' of the management table
#' @inheritParams source_ifnotloaded
#' @param column name of the column to set, currently only "loading_state"
#' @param state new state in the given column, Default: 'undefined'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  .createIfNotFound("Module_01.R", "loading_state")
#'  loading_state_of("Module_01.R) # == "undefinded"
#'
#'  .createIfNotFound("Module_02.R", "loading_state", "loaded")
#'  loading_state_of("Module_02.R) # == "loaded"

#'  }
#' }
#' @rdname .createIfNotFound

.createIfNotFound <- function(file, column, state="undefined") {

  # ----- Check Params -----------------------------------------------

  if ( ! column %in% colnames(.env$Modules) ) {
    stop("the column '", column, "' is not a valid column of the 'Modules' data frame.")
  }

  .check_level(state, levels(.env$Modules$loading_state))

  # ----- Main -------------------------------------------------------

  #print(isTRUE(is_managed(file)))
  if ( ! is_managed(file) ) { # if no such dataset
    .env$Modules[nrow(.env$Modules) + 1, column] <- state
    rownames(.env$Modules)[nrow(.env$Modules)] <- file
  }

  invisible()

} # .createIfNotFound








# set_loading_state -------------------------------------------------------
#' @title Set loading state of a file
#' @description sets the loading state of the given file to the given state
#' @inheritParams source_ifnotloaded
#' @param state desired state for the file
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#'  is_managed("Module_01.R") # FALSE
#'
#'  set_loading_state("Module_01.R", "undefined")
#'  loading_state_of("Module_01.R") # == "undefinded"
#'
#'  set_loading_state("Module_01.R", "loading")
#'  loading_state_of("Module_01.R") # == "loading"
#'
#'  set_loading_state("Module_01.R", "loaded")
#'  loading_state_of("Module_01.R") # == "loaded"
#'
#'  set_loading_state("Module_01.R", "somethingelse") # --> ERROR
#'
#'  }
#' }
#' @seealso
#' \code{\link{loading_state_of}} for determining the current loading state of a file managed by \pkg{hamsteR}, \code{\link{source_ifnotloaded}} for conditional loading and parsing code
#' @export
#' @rdname set_loading_state

set_loading_state <- function(file, state) {

  # ----- check data structure ---------------------------------------

  .check_data_structure()

  # ----- Check Params -----------------------------------------------

  .check_parameter_type_chr_single(file, "file")
  .check_parameter_type_chr_single(state, "state")

  # ----- Check and get module name ----------------------------------

  .check_filename_extension(file)

  # ----- Check Params -----------------------------------------------

  .check_level(state, levels(.env$Modules$loading_state))

  # ----- Main -------------------------------------------------------

  #print(file)

  # create module entry if not already managed
  .createIfNotFound(file, "loading_state")
  #print(status())
  #print(state)
  .env$Modules[rownames(.env$Modules) == file, "loading_state"] <- state

  invisible()

} # set_loading_state







# reset ---------------------------------------------------------------
#' @title Clears all states
#' @description Clears all states so that all files simply will be loaded and parsed the next time
#' @examples
#' \dontrun{
#' if(interactive()){
#'  source_ifnotloaded("TEST.R")
#'  set_loading_state("TEST.R", "loaded")
#'  source_ifnotloaded("TEST.R") # not loaded again because the loading state of the file was changed to 'loaded'
#'  is_managed("TEST.R") # TRUE
#'  reset() # clear all loading states
#'  is_managed("TEST.R") # FALSE
#'  source_ifnotloaded("TEST.R") # loaded again
#'  is_managed("TEST.R") # TRUE
#'  }
#' }
#' @seealso
#' \code{\link{set_loading_state}} to set the loading state and \code{\link{loading_state_of}} to get the current loading state of a file
#' @export
#' @rdname reset

reset <- function() {

  # ----- check data structure ---------------------------------------

  .check_data_structure()

  # ----- Main -------------------------------------------------------

  # remove all entries
  .env$Modules <- utils::head(.env$Modules,0)

} # reset








# .check_parameter_type_chr_single, internal ------------------------------------
#' @title Helper function for parameter range check
#' @description Helper for simple checking if the given parameter has a valid value type.
#' Valid value types in are this case a charcter vector with exactly one element.
#' @param x parameter to be checked
#' @param x_name name of the given parameter, used for error messages (possibly replacable by \code{quote} or something else)
#' @details This function wont check all possible cases, only the most common cases
#' @rdname .check_parameter_type_chr_single

.check_parameter_type_chr_single <- function(x, x_name){

  #print(x)

  if ( ! is.vector(x) ) {
    stop("parameter '", x_name, "' has to be a character vector of length 1.")
  } # ! is.vector

  if ( length(x) != 1 ) {
    stop("parameter '", x_name, "' has to be a character vector of length 1.")
  } # ! len==1

  if ( ! is.character(x) ) {
    stop("parameter '", x_name, "' has to be a character vector of length 1.")
  } # ! is.character

} #.check_parameter_type_chr_single








# loading_state_of -----------------------------------------------
#' @title Determine the loading state of the given file
#' @description Returns the loading state of the given file
#' @aliases get_loading_state
#' @inheritParams source_ifnotloaded
#' @return the state of the module, a value out of 'undefined', 'loading', 'loaded'
#' @details hamster is capable of managing loading state for file. With
#' this function you can determine the current loading state of the given
#' file. The loading state can be changed by \code{\link{set_loading_state}}
#' @examples
#' \dontrun{
#' if(interactive()){
#'  ## check state 'loaded'
#'  set_loading_state(file="TEST01.R", state="loaded")
#'  loading_state_of("TEST01.R")  # "loaded"
#'
#'  ## check state 'loading'
#'  set_loading_state(file="TEST01.R", state="loading")
#'  loading_state_of("TEST01.R")  # "loading"
#'
#'  ## check state 'undefinded'
#'  set_loading_state(file="TEST01.R", state="undefined")
#'  loading_state_of("TEST01.R")  # "undefined"
#' }}
#' @seealso
#' \code{\link{set_loading_state}} for setting the loading state, \code{\link{source_ifnotloaded}} for conditional loading and parsing a file
#' @export
#' @rdname loading_state_of

loading_state_of <- function(file) {

  # ----- check data structure ---------------------------------------

  .check_data_structure()

  # ----- column exists ----------------------------------------------

  if ( ! "loading_state" %in% colnames(.env$Modules)){
    stop("internal data structure error.")
  }

  # ----- Check Params -----------------------------------------------

  .check_parameter_type_chr_single(file, "file")

  # ----- check contains ---------------------------------------------

  if (!is_managed(file)) {
    return(NA)
  }

  # ----- Main -------------------------------------------------------

  return(
      as.character(
        .env$Modules$loading_state[rownames(.env$Modules) == file]
        )
    )

} # loading_state_of






# is_managed ----------------------------------------------------------
#' @title Check if the file is managed by \pkg{hamsteR}
#' @description checks if the given file is already managed by \pkg{hamsteR}
#' @aliases contains, managed
#' @inheritParams source_ifnotloaded
#' @examples
#' \dontrun{
#' if(interactive()){
#'  set_loading_state("another_script.R", "undefined")
#'  is_managed("another_script.R") # TRUE
#'  reset()
#'  is_managed("another_script.R") # FALSE
#' }}
#' @seealso
#' \code{\link{set_loading_state}} for setting the loading state of a file. This file is now \emph{managed} by \pkg{hamsteR}.
#' \code{\link{reset}} to delete all loading states.
#' @export
#' @rdname is_managed

is_managed <- function(file) {

  # ----- check data structure ---------------------------------------

  .check_data_structure()

  # ----- Check Params -----------------------------------------------

  .check_parameter_type_chr_single(file, "file")

  # ----- Check file name --------------------------------------------

  .check_filename_extension(file)

  # ----- Main -------------------------------------------------------

  return(file %in% rownames(.env$Modules))

} # is_managed








# status -------------------------------------------------------------
#' @title Status of all managed files
#' @description returns a data.frame with the state of managed files
#' @return a data table, module names as row.names,
#' loading state in column 'loading_state'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  set_loading_state("TEST_01.R", "loaded")
#'  set_loading_state("TEST_02.R", "undefined")
#'  status()
#'  ##           loading_state
#'  ## TEST_01.R loaded
#'  ## TEST_02.R undefined
#' }}
#' @export
#' @rdname status

status <- function() {

  # ----- check Modules ----------------------------------------------

  .check_data_structure()

  # ----- Main -------------------------------------------------------

  return(.env$Modules)

} # status








# .check_data_structure, internal --------------------------------------------
#' @title Checks the internal data structure
#' @description returns a data.frame with the state of managed files
#' @return a data table, module names as row.names,
#' loading state in column 'loading_state'
#' @rdname .check_data_structure

.check_data_structure <- function(){

  if ( ! exists("Modules", envir = .env) ) {
    stop("unable to access internal data structure.")
  } # exists .env$Module

} #.check_data_structure






# .check_level, internal ---------------------------------------------------
#' @title Checks if is loading state level
#' @description Checks if the given character value is part of the loading state levels
#' @rdname .check_level

.check_level <- function(x, levels) {

  if ( ! x %in% levels) {
    stop("'", x, "' is not a valid level out of ", paste0("'", paste0(levels, collapse="','"), "'"), ".")
  } # ! x in levels

} # .check_level


