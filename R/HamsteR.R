

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

  # Create Modules if not already has been created
  if (!is.environment("ModuleREnv")){
      ModuleREnv <- new.env()
  }
  if (!exists("Modules", envir = ModuleREnv)) {
    assign("Modules", data.frame(LoadingState=character()
                         ),
           envir = ModuleREnv)
  }

  op <- options()
  op.ModuleR <- list(
    ModuleR.path = "~/R-dev",
    ModuleR.install.args = "",
    ModuleR.name = "ModuleR",
    ModuleR.desc.author = "Marc O. Ruedel <mor@uni-bremen.de> [aut]",
    ModuleR.desc.license = "What license is it under?",
    ModuleR.debug=FALSE
  )
  toset <- !(names(op.ModuleR) %in% names(op))
  if(any(toset)) options(op.ModuleR[toset])

  invisible()
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

  module_name = tools::file_path_sans_ext(module_file)
  file_name.extension = tools::file_ext(module_file)
	
  if (file_name.extension != "R") {
    module_name <- ""
    err <- paste0("file '", module_file,"' has no '*.R' extension")
  } # ext ?= "R"

  return(list("name"= module_name, "err" = err))

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

Load <- function(module_file, OnlyIfNotAlreadyLoaded=TRUE) {

  module_name_result <- Modules..get_module_name(module_file)

  if (is.null(module_name_result[["err"]])) {
    # no error in module file name

    # grab module name (module file name w/o extension)
    module_name = module_name_result[["name"]]

    # check if the module is already marked 'loaded'
    if (Modules.is_notloaded(module_file) || # if dataset not marked as loaded
        !OnlyIfNotAlreadyLoaded){   # OR load anyway

      if (file.exists(module_file)) {

        .createIfNotFound(module_name, "LoadingState")
        if (getOption("ModuleR.debug")) {message(paste0("ModuleR: loading '", module_file, "' ..."))} # DEBUG

        source(module_file)
        return("loaded")

      } else {

        warning(paste0("file '", module_file,"' not found"))
        return(paste0("file '", module_file,"' not found"))

      } # file.exists

    } else {

      if (getOptions("ModuleR.debug")) {message(paste0("ModuleR: '", module_file, "' already marked as loaded, skipping"))} # DEBUG
      return("already marked as loaded")

    } # isTRUE

  } else {

    warning(paste0("file '", module_file,"' has no '*.R' extension"))
    return(module_name_result[["err"]])

  } # Modules..get_module_name)

  rm(temp)

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

.createIfNotFound <- function(module_name, column_name, state="not loaded") {

  if (!contains(module_name)) { # if no such dataset
    Modules[nrow(Modules)+1, column_name] <<- state #assign!!!
    rownames(Modules)[nrow(Modules)] <<- module_name #assign!!!
    return("created")
  }
  return("found")

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
#' @rdname .set

.set <- function(module_file, state) {

  module_name_result <- Modules..get_module_name(module_file)

  if (is.null(module_name_result[["err"]])){

    module_name <- module_name_result[["name"]]
    .createIfNotFound(module_name, "LoadingState")
    Modules$LoadingState[rownames(Modules) == module_name] <<- state # assign!
    if (getOption("Modules.debug")) {message(paste0("ModuleR: '", module_file,"' marked as ", state, "."))}
    return(Modules)

  } else {

    return(module_name[["err"]])

  } # !is.na

} # function




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
#' @rdname set_notloaded

set_notloaded <- function(module_file) {
  return(.set(module_file, state="not loaded"))
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
#' @rdname set_loaded

set_loaded <- function(module_file) {
  return(.set(module_file, state="loaded"))
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
#' @rdname set_loading

set_loading <- function(module_file) {
  return(.set(module_file, state="loading"))
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
#' @rdname set_all_unloaded

set_all_unloaded <- function() {
  Modules$LoadingState <<- "not loaded" # ass

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
#' @rdname reset

reset <- function() {
  Modules <<- head(Modules,0)
}




.is_x <- function(module_file, checkState) {
  module_name_result <- .get_module_name(module_file)
  if (is.null(module_name_result[["err"]])){
    module_name = module_name_result[["name"]]
    .createIfNotFound(module_name, "LoadingState")
    return(isTRUE(Modules$LoadingState[rownames(Modules)==module_name] == checkState))
  } else {
    return(module_name_result[["err"]])
  }
}

is_loaded <- function(module_file) {
  return(.is_x(module_file, checkState="loaded"))
}


is_loading <- function(module_file) {
  return(.is_x(module_file, checkState="loading"))
}

is_not_loaded <- function(module_file) {
  return(.is_x(module_file, checkState="not loaded"))
}




.contains <- function(module_name) {
  return(module_name %in% rownames(Modules))
  # return ( is.element(module_name, Modules) )
}


get_status <- function() {
  return(Modules)
}





# # TEST
# #
#
# message(" 0. clean up")
# Modules.reset()
#
# message(" 1. print")
# print(Modules, row.names = TRUE)
#
# message(" 2. Modules.set_loaded('TEST1.R')")
# print(Modules.set_loaded("TEST1.R"), row.names=TRUE)
#
# message(" 2a. Modules.is.loaded('TEST1.R')")
# print(Modules.is.loaded("TEST1.R"), row.names=TRUE)
#
# message(" 2b. Modules.is.loading('TEST1.R')")
# print(Modules.is.loading("TEST1.R"), row.names=TRUE)
#
# message(" 2c. Modules.is.loaded('TEST2.R')")
# print(Modules.is.loaded("TEST2.R"), row.names=TRUE)
#
# message(" 2d. Modules.is.not.loaded('TEST2.R')")
# print(Modules.is.not.loaded("TEST2.R"), row.names=TRUE)
#
# message(" 3. print")
# print(Modules, row.names = TRUE)
#
# message(" 4. Modules.Load('TEST.R')")
# print(Modules.Load("TEST.R"))
#
# message(" 5. print")
# print(Modules, row.names = TRUE)
#
# message(" 6. Modules.Load('TEST1.R')")
# print(Modules.Load("TEST1.R"))
#
# message(" 7. print")
# print(Modules, row.names = TRUE)
#
# message(" 8. Modules.set_notloaded('TEST1.R')")
# print(Modules.set_notloaded("TEST1.R"))
#
# message(" 9. print")
# print(Modules, row.names = TRUE)
#
# message("10. Modules.Load('TEST1.R')")
# print(Modules.Load("TEST1.R"))
#
# message("11. print")
# print(Modules, row.names = TRUE)
#
