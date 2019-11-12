#
# hamsteR
# Simple Module Management for R
#


.env = new.env(parent = emptyenv())

LoadingStates = factor(levels = c("undefined", "loading", "loaded"))


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param libname PARAM_DESCRIPTION
#' @param pkgname PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname .onLoad

.onLoad <- function(libname, pkgname){

  # DEBUG
  # message("hamsteR.onLoad: loading...")

  # create basic data structure
  if ( ! exists("Modules", envir = .env) ){
    .env$Modules <- data.frame(LoadingState=character(), stringsAsFactors = FALSE)
    cat("\U2713 data structure prepared.\n")
  }

  op <- options()
  op.hamsteR <- list(
    hamsteR.path = "~/R-dev",
    hamsteR.install.args = "",
    hamsteR.name = "hamsteR",
    hamsteR.desc.author = "Marc O. Rüdel <mor@uni-bremen.de> [aut, cre]",
    hamsteR.desc.license = "What license is it under?",
    hamsteR.debug = FALSE
  )
  toset <- !(names(op.hamsteR) %in% names(op))
  if(any(toset)) options(op.hamsteR[toset])

  # DEBUG
  # message("hamsteR.onLoad: finished")

  invisible()

}




#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param libname PARAM_DESCRIPTION
#' @param pkgname PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname .onAttach

.onAttach <- function(libname, pkgname){
  message("hamsteR attached.")
}






#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param module_file PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname .get_module_name

.get_module_name <- function(module_file) {

  err <- NULL

  module_name <- tools::file_path_sans_ext(module_file)
  file_name.extension <- tools::file_ext(module_file)

  if (file_name.extension != "R") {
    module_name <- ""
    err <- paste0("ERROR: file '", module_file,"' has no '*.R' extension")
  } # ext ?= "R"

  res = c(name = module_name,
          err  = err
          )
  if (getOption("hamsteR.debug")) {print(res)}
  return(res)

}



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param module_file PARAM_DESCRIPTION
#' @param OnlyIfNotAlreadyLoaded PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname Load

Load <- function(module_file, force_load=FALSE) {

  # ----- Check Params -----------------------------------------------

  check = .check_module_file(module_file)
  if (check != "ok") {
    return(check)
  }

  if ( is.na(force_load) |
       ! is.logical(force_load) |
       length(force_load) != 1 ) {
    return ("ERROR: force_load parameter has to be just TRUE or FALSE.")
  }


  # ----- check Modules ----------------------------------------------

  if (! exists("Modules", envir = .env)) {
    return("ERROR: unable to access internal data structure")
  } # exists .env$Module

  # ----- check module name ------------------------------------------

  module_name_result <- .get_module_name(module_file)

  if ("err" %in% names(module_name_result)) {
    #  error in module file name
    warning(paste0("file '", module_file,"' has no '*.R' extension"))
    return(module_name_result["err"])
  } # .get_module_name)

  # grab module name (module file name w/o extension)
  module_name = module_name_result["name"]

  # ----- check already loaded ---------------------------------------

  # check if the module is already marked 'loaded'
  if ( ! is_notloaded(module_file) & # if dataset marked as loaded
      !force_load){   # OR force load / load anyway

    if (getOptions("hamsteR.debug")) {message(paste0("hamsteR: '", module_file, "' already marked as loaded, skipping"))} # DEBUG
    return("already marked as loaded")

  } # ! notloaded & ! force_load

  # ----- module file exists? ----------------------------------------

  if (! file.exists(module_file)) {

    warning(paste0("file '", module_file,"' not found"))
    return(paste0("file '", module_file,"' not found"))

  } # file.exists

  # ----- Main -------------------------------------------------------

  .createIfNotFound(module_name, "LoadingState")
  if (getOption("hamsteR.debug")) {message(paste0("hamsteR: loading '", module_file, "' ..."))} # DEBUG

  require(module_file)
  return("loaded")

}





#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param module_file PARAM_DESCRIPTION
#' @param column_name PARAM_DESCRIPTION
#' @param state PARAM_DESCRIPTION, Default: "not loaded"
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname .createIfNotFound

.createIfNotFound <- function(module_name, column, state="undefined") {

  # ----- Check Params -----------------------------------------------

  if ( ! column %in% colnames(.env$Modules) ) {
    return("ERROR: parameter 'column' is not a valid column of the 'Modules' data frame.")
  }

  if ( ! state %in% levels(hamsteR::LoadingStates)) {
    return("ERROR: '", state, "' is not a valid level of 'LoadingStates'.")
  } # ! state in LoadingStates

  # ----- check Modules ----------------------------------------------

  if ( ! exists("Modules", envir = .env) ) {
    return("ERROR: unable to access internal data structure")
  } # exists .env$Module

  # ----- module name exists? ----------------------------------------

  if (.contains(module_name)) { # if such dataset
    return("found")
  }

  # ----- Main -------------------------------------------------------

  .env$Modules[nrow(.env$Modules) + 1, column] <- state
  rownames(.env$Modules)[nrow(.env$Modules)] <- module_name
  return("created")

} # .createIfNotFound



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param module_file PARAM_DESCRIPTION
#' @param state PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname set_loading_state

set_loading_state <- function(module_file, state) {

  # ----- Check Params -----------------------------------------------

  if ( ! exists("Modules", envir = .env)) {
    return("ERROR: unable to access internal data structure")
  } # ! exists .env$Module

  if ( ! state %in% levels(hamsteR::LoadingStates)) {
    return("ERROR: '", state, "' is not a valid level of 'LoadingStates'.")
  } # ! state in LoadingStates

  check = .check_module_file(module_file)
  if (check != "ok") {
    return(check)
  }

  # ----- check Modules ----------------------------------------------

  if (! exists("Modules", envir = .env)) {
    return("ERROR: unable to access internal data structure")
  } # exists .env$Module

  # ----- Main -------------------------------------------------------

  module_name_result <- .get_module_name(module_file)
  #print(module_name_result)
  #cat("\nname err? ", !is.null(module_name_result[["err"]]), "\n", "  Err:  ", module_name_result[["err"]], "\n")

  if ( ! "err" %in% names(module_name_result)){
    # no error while gathering module name from module file name

    module_name <- module_name_result[["name"]]
    #print(module_name)
    .createIfNotFound(module_name, "LoadingState")
    #print(status())
    #print(state)
    .env$Modules[rownames(.env$Modules) == module_name, "LoadingState"] <- match(state, levels(LoadingStates))

    return(paste0("'", module_file,"' now marked as '", state, "'."))

  } else {

    # warning(module_name_result[["err"]])
    return(module_name_result[["err"]])

  } # !is.na


} # function





#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname set_all_unloaded

set_all_unloaded <- function() {

  if (exists("Modules", envir = .env)) {

    .env$Modules$LoadingState <- "not loaded" # ass
    return("ok")

  } else {
    return("ERROR: unable to access internal data structure")
  } # exists .env$Module
}





#' @title Clears all module states
#' @description Clears all module states so that all modules are reloaded the next time they are used
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname reset

reset <- function() {

  # ----- check Modules ----------------------------------------------

  if (! exists("Modules", envir = .env)) {
    return("ERROR: unable to access internal data structure")
  } # exists .env$Module

  # ----- Main -------------------------------------------------------

  .env$Modules <- head(.env$Modules,0)
  return("ok")

} # reset


.check_module_file <- function(module_file){

  if ( ! is.vector(module_file) ) {
    return("ERROR: first parameter is not a vector.")
  } # ! state in LoadingStates

  if ( length(module_file) != 1 ) {
    return("ERROR: length of first parameter is not 1.")
  } # ! state in LoadingStates

  if ( ! is.character(module_file) ) {
    return("ERROR: first parameter is a character vector.")
  } # ! state in LoadingStates

  return("ok")
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname check_loading_state

check_loading_state <- function(module_file, check_state) {

  # ----- Check Params -----------------------------------------------

  if ( ! exists("Modules", envir = .env)) {
    return("ERROR: unable to access internal data structure")
  } # ! exists .env$Module

  if ( ! check_state %in% levels(hamsteR::LoadingStates)) {
    return("ERROR: state parameter is not a valid level of 'LoadingStates'.")
  } # ! state in LoadingStates

  check = .check_module_file(module_file)
  if (check != "ok") {
    return(check)
  }

  # ----- check Modules ----------------------------------------------

  if (! exists("Modules", envir = .env)) {
    return("ERROR: unable to access internal data structure")
  } # exists .env$Module

  # ----- handle file name error -------------------------------------

  module_name_result <- .get_module_name(module_file)

  if ("err" %in% names(module_name_result)){
    # handle file name error
    return(module_name_result[["err"]])
  }

  # ----- Main -------------------------------------------------------

  module_name = module_name_result[["name"]]
  #print(module_name)
  .createIfNotFound(module_name, "LoadingState")

  #print(rownames(.env$Modules))
  #print(.env$Modules[rownames(.env$Modules) == module_name, "LoadingState"])
  #print(.env$Modules[rownames(.env$Modules) == module_name, "LoadingState"])
  # print(.env$Modules[rownames(.env$Modules) == module_name, "LoadingState"])
  # print(check_state)
  # print(match(check_state, levels(LoadingStates)))
  return(
    isTRUE(
      .env$Modules[rownames(.env$Modules) == module_name, "LoadingState"] == match(check_state, levels(LoadingStates))
      )
    )

} # is_x








#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname .contains

.contains <- function(module_name) {

  # ----- check Modules ----------------------------------------------

  if ( ! exists("Modules", envir = .env) ) {
    return("ERROR: unable to access internal data structure")
  } # exists .env$Module

  # ----- Main -------------------------------------------------------

  return(module_name %in% rownames(.env$Modules))
  # return ( is.element(module_name, .env$Modules) )

} # .contains






#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname status

status <- function() {

  # ----- check Modules ----------------------------------------------

  if ( ! exists("Modules", envir = .env) ) {
    return("ERROR: unable to access internal data structure")
  } # exists .env$Module

  # ----- Main -------------------------------------------------------

  return(.env$Modules)

} # status






