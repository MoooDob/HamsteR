context("hamsteR")

# hamsteR .onLoad? --------------------
test_that(".onLoad?", {

  # check if options were set
  expect_equal(getOption("hamsteR.path")                                   , "~/R-dev")
  expect_equal(getOption("hamsteR.install.args")                           , "")
  expect_equal(getOption("hamsteR.name")                                   , "hamsteR")
  expect_equal(getOption("hamsteR.desc.author")                            , "Marc O. R\U00FCdel <mor@uni-bremen.de> [aut, cre]")
  expect_equal(getOption("hamsteR.desc.license")                           , "What license is it under?")
  expect_equal(getOption("hamsteR.file.ext.allowed")                       , c("R"))
  expect_equal(getOption("hamsteR.debug")                                  , FALSE)

  # call .onLoad again with
  expect_equal(exists("Modules", envir=.env)                               , TRUE)
  .onLoad()
  expect_equal(exists("Modules", envir=.env)                               , TRUE)

  rm("Modules", envir=.env)
  expect_equal(exists("Modules", envir=.env)                               , FALSE)
  .onLoad()
  expect_equal(exists("Modules", envir=.env)                               , TRUE)

  # check if options were not changed
  expect_equal(getOption("hamsteR.path")                                   , "~/R-dev")
  expect_equal(getOption("hamsteR.install.args")                           , "")
  expect_equal(getOption("hamsteR.name")                                   , "hamsteR")
  expect_equal(getOption("hamsteR.desc.author")                            , "Marc O. R\U00FCdel <mor@uni-bremen.de> [aut, cre]")
  expect_equal(getOption("hamsteR.desc.license")                           , "What license is it under?")
  expect_equal(getOption("hamsteR.file.ext.allowed")                       , c("R"))
  expect_equal(getOption("hamsteR.debug")                                  , FALSE)


  # remove all options
  options("hamsteR.path" = NULL)
  options("hamsteR.install.args" = NULL)
  options("hamsteR.name" = NULL)
  options("hamsteR.desc.author" = NULL)
  options("hamsteR.desc.license" = NULL)
  options("hamsteR.file.ext.allowed" = NULL)
  options("hamsteR.debug" = NULL)

  .onLoad()

  # check if options were not changed
  expect_equal(getOption("hamsteR.path")                                   , "~/R-dev")
  expect_equal(getOption("hamsteR.install.args")                           , "")
  expect_equal(getOption("hamsteR.name")                                   , "hamsteR")
  expect_equal(getOption("hamsteR.desc.author")                            , "Marc O. R\U00FCdel <mor@uni-bremen.de> [aut, cre]")
  expect_equal(getOption("hamsteR.desc.license")                           , "What license is it under?")
  expect_equal(getOption("hamsteR.file.ext.allowed")                       , c("R"))
  expect_equal(getOption("hamsteR.debug")                                  , FALSE)


})

# hamsteR Environment created? --------------------
test_that("private environment created?", {
  try(.env$TEST <- "TEST")
  res <- NULL
  try(res <- .env$TEST)
  expect_equal(res, "TEST")
  }
)


# check .check_data_structure --------------------
test_that("check .check_data_structure", {

  temp <- .env$Modules
  rm("Modules", envir=.env)
  expect_error(.check_data_structure(), "unable to access internal data structure.")

  # recreate data structure
  # .env$Modules <- data.frame(
  #   loading_state=factor(levels = c("undefined", "loading", "loaded"), exclude=""),
  #   stringsAsFactors = FALSE
  # )

  .env$Modules <- temp
  rm(temp)

  }
)


# hamsteR:: 'Modules' data structure automatically created on start? --------------------
test_that("'Modules' data structure automatically created on start?", {

  expect_equal(exists("Modules", envir = .env), TRUE)

  }
)




# hamsteR:: 'Modules' structure empty? --------------------
test_that("'Modules' structure empty?", {

  expect_equal(status(), data.frame(
    loading_state=factor(levels = c("undefined", "loading", "loaded"), exclude=""),
    stringsAsFactors = FALSE
    )
  )
  expect_equal(nrow(status()), 0)
  expect_equal(dim(status()), c(0,1))
}

)





# hamsteR:: Check .check_filename_extension option 'hamsteR.file.ext.allowed'--------------------
test_that(".check_filename_extension works with changed option 'hamsteR.file.ext.allowed'", {

  old_option <- getOption("hamsteR.file.ext.allowed")

  options("hamsteR.file.ext.allowed" = c("R", "RR", "txt"))
  expect_equal(.check_filename_extension("TEST.R")                 , NULL)

  options("hamsteR.file.ext.allowed" = c(1, 2, 3))
  expect_error(.check_filename_extension("TEST.R")                 , "option 'hamsteR.file.ext.allowed' not in expected character vector format.")

  options("hamsteR.file.ext.allowed" = c("R", 1, 2)) # vector will be interpreted as character vector as long as least one element is a character, all other elements will be treated as characters too
  expect_equal(.check_filename_extension("TEST.R")                 , NULL)
  expect_equal(.check_filename_extension("TEST.1")                 , NULL)
  expect_error(.check_filename_extension("TEST.RR")                , "current file extension 'RR' not allowed.")
  expect_equal(.check_filename_extension("TEST.2")                 , NULL)
  expect_error(.check_filename_extension("TEST.5")                 , "current file extension '5' not allowed.")

  options("hamsteR.file.ext.allowed" = c("R", 1, "*")) # * in vector = everything allowed
  expect_equal(.check_filename_extension("TEST.R")                 , NULL)
  expect_equal(.check_filename_extension("TEST.1")                 , NULL)
  expect_equal(.check_filename_extension("TEST.RR")                , NULL)
  expect_equal(.check_filename_extension("TEST.2")                 , NULL)
  expect_equal(.check_filename_extension("TEST.5")                 , NULL)

  options("hamsteR.file.ext.allowed" = "R")
  expect_equal(.check_filename_extension("TEST.R")                 , NULL)

  options("hamsteR.file.ext.allowed" = data.frame(col1=c("R", "RR")))
  expect_error(.check_filename_extension("TEST.R")                 , "option 'hamsteR.file.ext.allowed' not in expected character vector format.")

  options("hamsteR.file.ext.allowed" = NA)
  expect_error(.check_filename_extension("TEST.R")                 , "option 'hamsteR.file.ext.allowed' not in expected character vector format.")

  options("hamsteR.file.ext.allowed" = NULL)  # no extension expected
  expect_error(.check_filename_extension("TEST.R")                 , "current file extension 'R' not allowed.")
  expect_equal(.check_filename_extension("TEST")                   , NULL)

  options("hamsteR.file.ext.allowed" = "")  # no extension expected
  expect_error(.check_filename_extension("TEST.R")                 , "current file extension 'R' not allowed.")
  expect_equal(.check_filename_extension("TEST")                   , NULL)

  options("hamsteR.file.ext.allowed" = "*")  # any extension expected ('non' included)
  expect_equal(.check_filename_extension("TEST")                   , NULL)
  expect_equal(.check_filename_extension("TEST.R")                 , NULL)
  expect_equal(.check_filename_extension("TEST.RR")                , NULL)
  expect_equal(.check_filename_extension("TEST.dfgfghk")           , NULL)

  options("hamsteR.file.ext.allowed" = old_option)

})


# hamsteR:: convert module_file to module_name? --------------------
test_that("convert module_file to module_name?", {

  # missing extension
  expect_error(.check_filename_extension("DummyModule01")               , "current file extension '' not allowed.")
  expect_error(.check_filename_extension("DummyModule01.RR")            , "current file extension 'RR' not allowed.")
  expect_equal(.check_filename_extension("DummyModule01.R")             , NULL)
  expect_error(.check_filename_extension("Transfer/DummyModule01")      , "current file extension '' not allowed.")
  expect_error(.check_filename_extension("Transfer/DummyModule01.txt")  , "current file extension 'txt' not allowed.")
  expect_equal(.check_filename_extension("Transfer/DummyModule01.R")    , NULL)

  # different subdirs and names
  expect_equal(.check_filename_extension("DummyModule01.R")             , NULL)
  expect_equal(.check_filename_extension("DummyModule_01.R")            , NULL)
  expect_equal(.check_filename_extension("Transfer/DummyModule01.R")    , NULL)
  expect_equal(.check_filename_extension("Dir1/dir2/DummyModule.R")     , NULL)

  # no new entries in module list after checking
  expect_equal(nrow(status()), 0)

  # change allowed extensions
  options("hamsteR.file.ext.allowed" = c("R", "RR"))
  expect_error(.check_filename_extension("DummyModule01")               , "current file extension '' not allowed.")
  expect_equal(.check_filename_extension("DummyModule01.RR")            , NULL)
  expect_error(.check_filename_extension("DummyModule01.txt")           , "current file extension 'txt' not allowed.")
  expect_equal(.check_filename_extension("DummyModule01.R")             , NULL)
  expect_error(.check_filename_extension("Transfer/DummyModule01")      , "current file extension '' not allowed.")
  options("hamsteR.file.ext.allowed" = c("R"))

  }
)



# hamsteR:: .createIfNotFound? --------------------
test_that(".createIfNotFound?", {

  reset()
  #print(is_managed("DummyModule"))
  expect_error(is_managed("DummyModule")                                , "current file extension '' not allowed.")
  expect_equal(is_managed("DummyModule.R")                              , FALSE)

  # createIfNotFound w/o file extension
  expect_error(.createIfNotFound(file="DummyModule",
                                 column="loading_state")                 , "current file extension '' not allowed.")
  expect_error(is_managed("DummyModule")                                , "current file extension '' not allowed.")
  expect_equal(nrow(status()), 0) # list still empty

  # createIfNotFound with extension
  expect_equal(.createIfNotFound(file="DummyModule.R",
                                 column="loading_state")                 , NULL)
  # print(status())
  expect_equal(is_managed("DummyModule.R")                              , TRUE)
  expect_equal(nrow(status()), 1)  # new entry added
  # print(str(status()))
  # print(row.names(status()))
  expect_equal(status(), data.frame(loading_state=factor(c("undefined"),
                                                        levels=c("undefined", "loading", "loaded"),
                                                        exclude=""
                                                        ),
                                    row.names=c("DummyModule.R"),
                                    stringsAsFactors=FALSE
                                    )
               )


  #again
  expect_error(is_managed("DummyModule")                                , "current file extension '' not allowed.")
  expect_equal(is_managed("DummyModule.R")                              , TRUE)

  # createIfNotFound w/o file extension
  expect_error(.createIfNotFound(file="DummyModule",
                                 column="loading_state")                 , "current file extension '' not allowed.")
  expect_error(is_managed("DummyModule")                                , "current file extension '' not allowed.")
  expect_equal(nrow(status()), 1) # still only one item (DummyModule.R)
  expect_equal(status(), data.frame(loading_state=factor(c("undefined"),
                                                        levels=c("undefined", "loading", "loaded"),
                                                        exclude=""
                                                        ),
                                    row.names=c("DummyModule.R"),
                                    stringsAsFactors=FALSE
                                    )
               )
  # createIfNotFound with extension
  expect_equal(.createIfNotFound(file="DummyModule.R",
                                 column="loading_state")                 , NULL)
  expect_equal(is_managed("DummyModule.R")                              , TRUE)
  expect_equal(nrow(status()), 1)  # still one item in the list
  expect_equal(status(), data.frame(loading_state=factor(c("undefined"),
                                                        levels=c("undefined", "loading", "loaded"),
                                                        exclude=""
                                                        ),
                                    row.names=c("DummyModule.R"),
                                    stringsAsFactors=FALSE
                                    )
               )


  # createIfNotFound with non-existing column
  expect_error(.createIfNotFound(file="DummyModule.R",
                                 column="Dummy")                        , "the column 'Dummy' is not a valid column of the 'Modules' data frame.")

  }
)


# hamsteR:: loading state 'Modules' readable? --------------------
test_that("loading state 'Modules' readable?", {

  reset()
  expect_equal(nrow(status()), 0)

  nModulesBefore = nrow(status())
  expect_error(set_loading_state("DummyModule", "loading")     , "current file extension '' not allowed.")
  nModulesAfter = nrow(status())
  #cat("\nbefore: ", nModulesBefore, "\n")
  #print(r)
  #cat("after : ", nModulesAfter, "\n")
  #m = status()
  #print(m)
  expect_equal(nModulesAfter, nModulesBefore)

  nModulesBefore = nrow(status())
  expect_equal(set_loading_state("DummyModule.R", "loading")               , NULL)
  nModulesAfter = nrow(status())
  # cat("\nbefore: ", nModulesBefore, "\n")
  # print(r)
  # cat("after : ", nModulesAfter, "\n")
  # m = status()
  # print(m)
  expect_equal(nModulesAfter - nModulesBefore                              , 1)

  # rename loading_state column
  reset()
  Mn <- names(.env$Modules)

  names(.env$Modules)[names(.env$Modules)=="loading_state"] <- "NewName"
  expect_equal(status()                                                    , data.frame(NewName=factor(levels = c("undefined", "loading", "loaded"), exclude=""),
                                                                                        row.names = character(),
                                                                                        stringsAsFactors = FALSE
                                                                                        )
               )

  expect_error(loading_state_of("DummyModule.R")                           , "internal data structure error.")

  names(.env$Modules) <- Mn

  }
)



# hamsteR:: loaded or not? --------------------
test_that("loaded or not?", {

  reset()
  expect_error(set_loading_state("DummyModule", "loading")                 , "current file extension '' not allowed.")
  expect_error(loading_state_of("DummyModule")                          , "current file extension '' not allowed.")

  reset()
  expect_equal(set_loading_state("DummyModule.R", "loading")               , NULL)
  expect_equal(loading_state_of("DummyModule.R")                        , "loading")

  reset()
  expect_equal(set_loading_state("DummyModule.R", "loaded")                , NULL)
  expect_equal(loading_state_of("DummyModule.R")                        , "loaded")

  reset()
  expect_equal(set_loading_state("DummyModule.R", "undefined")                      , NULL)
  expect_equal(loading_state_of("DummyModule.R")                        , "undefined")

  # false factor
  reset()
  expect_error(set_loading_state("DummyModule.R", "nothing")            , "")
  expect_equal(loading_state_of("DummyModule.R")                        , NA)

})



# hamsteR:: reset? --------------------
test_that("hamsteR reset?", {

  reset()
  set_loading_state("DummyModule.R", "loading")
  expect_equal(nrow(status()), 1)

  reset()
  expect_equal(nrow(status()), 0)

  reset()
  set_loading_state("DummyModule.R", "loading")
  set_loading_state("DummyModule.R", "loading")
  expect_equal(nrow(status()), 1)

  reset()
  set_loading_state("DummyModule.R", "loading")
  set_loading_state("DummyModule.R", "loaded")
  expect_equal(nrow(status()), 1)

  reset()
  set_loading_state("DummyModule.R", "loading")
  set_loading_state("DummyModule_02.R", "loading")
  expect_equal(nrow(status()), 2)

  reset()
  set_loading_state("DummyModule.R", "loading")
  set_loading_state("DummyModule_02.R", "loaded")
  expect_equal(nrow(status()), 2)

})


# hamsteR:: source_ifnotloaded --------------------
test_that("source_ifnotloaded", {

  # work with TEST_01.R
  reset()
  #print(getwd())
  TEST_01_cnt <<- 0
  source_ifnotloaded(file = "TEST_01.R") # TEST_01 will finally be marked as "loaded"
  expect_equal(loading_state_of("TEST_01.R")                           , "loaded") # see file "TEST_01.R", line 9
  expect_equal(TEST_01_cnt                                             , 1)
  expect_error(loading_state_of("TEST_01")                             , "current file extension '' not allowed.")
  expect_error(is_managed("TEST_01")                                   , "current file extension '' not allowed.")
  expect_equal(is_managed("TEST_01.R")                                 , TRUE)
  expect_equal(loading_state_of("TEST_01.R")                           , "loaded")

  # try load again
  source_ifnotloaded(file = "TEST_01.R") # should not load, so TEST_01 counter should not be incresead
  expect_equal(TEST_01_cnt, 1)

  # try force load
  source_ifnotloaded(file = "TEST_01.R", force_load = TRUE) # load anyway
  expect_equal(TEST_01_cnt, 2) # cnt is increased


  # work with TEST_02.R
  reset()
  #print(getwd())
  TEST_02_cnt <<- 0
  source_ifnotloaded(file = "TEST_02.R") # TEST_01 will finally be marked as "undefined"
  #print(loading_state_of("TEST_02.R", "loaded"))
  expect_equal(TEST_02_cnt                                            , 1)
  expect_error(loading_state_of("TEST_02")                         , "current file extension '' not allowed.")
  expect_error(is_managed("TEST_02")                                  , "current file extension '' not allowed.")
  expect_equal(is_managed("TEST_02.R")                                , TRUE)
  expect_equal(loading_state_of("TEST_02.R")                       , "undefined")

  # try load again
  source_ifnotloaded(file = "TEST_02.R") # not marked as loaded, so TEST_02 should be loaded again
  expect_equal(TEST_02_cnt, 2)

  # mark as loaded
  set_loading_state(file = "TEST_02.R", "loaded")
  expect_equal(TEST_02_cnt, 2)

  # try force load
  source_ifnotloaded(file = "TEST_02.R", force_load = TRUE)
  expect_equal(TEST_02_cnt, 3)


  # work with TEST_03.R
  reset()
  #print(getwd())
  TEST_03_cnt <<- 0
  source_ifnotloaded(file = "TEST_03.R") # TEST_01 will finally be marked as "undefined"
  expect_equal(TEST_03_cnt, 1)
  expect_error(loading_state_of("TEST_03")                       , "current file extension '' not allowed.")
  expect_error(is_managed("TEST_03")                                , "current file extension '' not allowed.")
  expect_equal(is_managed("TEST_03.R")                              , TRUE)
  expect_equal(loading_state_of("TEST_03.R")                     , "loaded") # return statement in TEST_03.R ignored

  # try load again
  source_ifnotloaded(file = "TEST_03.R") # marked as loaded, so TEST_03 should not be loaded again
  expect_equal(TEST_03_cnt, 1)

  # try force load
  source_ifnotloaded(file = "TEST_03.R", force_load = TRUE)
  expect_equal(TEST_03_cnt, 2) # now its loaded again


  # try a false force load parameter
  expect_error(source_ifnotloaded(file = "TEST_03.R",
                                  force_load = "YES")               , "force_load parameter has to be just TRUE or FALSE.")

  expect_error(source_ifnotloaded(file = "TEST_03.R",
                                  force_load = c("TRUE"))           , "force_load parameter has to be just TRUE or FALSE.")

  # try a non-existing file
  expect_error(source_ifnotloaded(file = "TEST_TEST.R")             , "file 'TEST_TEST.R' not found.")


  # loading state unchanged for loaded TEST_03
  loading_state_before <- loading_state_of("TEST_03.R")
  expect_equal(loading_state_before                                 , "loaded")
  expect_equal(is_managed("TEST_03.R")                              , TRUE)
  loading_state_after <- loading_state_of("TEST_03.R")
  expect_equal(loading_state_after                                  , "loaded")
  expect_equal(is_managed("TEST_03.R")                              , TRUE)


  # loading state unchanged for loaded TEST_03
  loading_state_before <- loading_state_of("TEST_03.R")
  expect_equal(loading_state_before                                 , "loaded")
  expect_equal(is_managed("TEST_03.R")                              , TRUE)
  expect_message(source_ifnotloaded(file = "TEST_03.R",
                                    force_load = TRUE)              , "TEST_03 call counter: 3")
  loading_state_after <- loading_state_of("TEST_03.R")
  expect_equal(loading_state_after                                  , "loaded")
  expect_equal(is_managed("TEST_03.R")                              , TRUE)


  # loading state changed for not-loaded TEST_04
  loading_state_before <- loading_state_of("TEST_04.R")
  expect_equal(loading_state_before                                 , NA)
  expect_equal(is_managed("TEST_04.R")                              , FALSE)
  expect_message(source_ifnotloaded(file = "TEST_04.R")             , "TEST_04 call counter: 1")
  loading_state_after <- loading_state_of("TEST_04.R")
  expect_equal(loading_state_after                                  , "undefined")
  expect_equal(is_managed("TEST_04.R")                              , TRUE)


  # try debug switch
  options("hamsteR.debug" = TRUE)
  expect_message(source_ifnotloaded(file = "TEST_05.R")             , "hamsteR: loading 'TEST_05.R' ...")
  expect_message(source_ifnotloaded(file = "TEST_05.R")             , "TEST_05 call counter: 2")
  options("hamsteR.debug" = FALSE)

})


# hamsteR:: .check_parameter_type_chr_single --------------------
test_that(".check_parameter_type_chr_single", {

  expect_equal(.check_parameter_type_chr_single("Value", "Value")       , NULL)
  expect_error(.check_parameter_type_chr_single(1, "One")               , "parameter 'One' has to be a character vector of length 1.")
  expect_error(.check_parameter_type_chr_single(c(1,"Hi"), "1-Hi")      , "parameter '1-Hi' has to be a character vector of length 1.")
  expect_error(.check_parameter_type_chr_single(c(1,2), "1-2")          , "parameter '1-2' has to be a character vector of length 1.")
  expect_error(.check_parameter_type_chr_single(
    data.frame(col1=c(1),col2=c("Hi")), "df")                           , "parameter 'df' has to be a character vector of length 1.")

})
