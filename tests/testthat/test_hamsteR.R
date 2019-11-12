context("hamsteR")


# hamsteR Environment created? --------------------
test_that("private environment created?", {
  try(.env$TEST <- "TEST")
  res <- NULL
  try(res <- .env$TEST)
  expect_equal(res, "TEST")
  }
)


# hamsteR:: 'Modules' data structure automatically created on start? --------------------
test_that("'Modules' data structure automatically created on start?", {
  expect_equal(exists("Modules", envir = .env), TRUE)
  }
)


# hamsteR:: empty 'Modules' structure? --------------------
test_that("empty 'Modules' structure?", {
  expect_equal(status(), data.frame(LoadingState=character(), stringsAsFactors = FALSE))
  expect_equal(nrow(status()), 0)
  expect_equal(dim(status()), c(0,1))
  }
)


# hamsteR:: convert module_file to module_name? --------------------
test_that("convert module_file to module_name?", {

    # test =.get_module_name("DummyModule01")
  # print(test)
  # print(class(test))
  # print(test == c(name="", error="ERROR: file 'DummyModule01' has no '*.R' extension"))

  # missing extension
  expect_equal(all(.get_module_name("DummyModule01")            == c(name="",                         error="ERROR: file 'DummyModule01' has no '*.R' extension")), TRUE)
  expect_equal(all(.get_module_name("DummyModule01")            == c(name="DummyModule01",            error=NULL)), FALSE)
  expect_equal(all(.get_module_name("Transfer/DummyModule01")   == c(name="",                         error="ERROR: file 'Transfer/DummyModule01' has no '*.R' extension")), TRUE)
  expect_equal(all(.get_module_name("Transfer/DummyModule01")   == c(name="Transfer/DummyModule01",   error=NULL)), FALSE)

  # different subdirs and names
  expect_equal(all(.get_module_name("DummyModule01.R")          == c(name="DummyModule01",            error=NULL)), TRUE)
  expect_equal(all(.get_module_name("DummyModule01.R")          == c(name="DummyModule01",            error=NULL)), TRUE)
  expect_equal(all(.get_module_name("DummyModule_01.R")         == c(name="DummyModule_01",           error=NULL)), TRUE)
  expect_equal(all(.get_module_name("Transfer/DummyModule01.R") == c(name="Transfer/DummyModule01",   error=NULL)), TRUE)
  expect_equal(all(.get_module_name("Dir1/dir2/DummyModule.R")  == c(name="Dir1/dir2/DummyModule",    error=NULL)), TRUE)

  # no new entries in module list
  expect_equal(nrow(status()), 0)
  }
)



# hamsteR:: createIfNotFound? --------------------
test_that("createIfNotFound?", {

  reset()
  expect_equal(.contains("DummyModule"), FALSE)
  .createIfNotFound(module_name="DummyModule", column="LoadingState")
  expect_equal(.contains("DummyModule"), TRUE)
  expect_equal(nrow(status()), 1)

  #again
  .createIfNotFound(module_name="DummyModule", column="LoadingState")
  expect_equal(.contains("DummyModule"), TRUE)
  expect_equal(.contains("DummyModule.R"), FALSE)
  expect_equal(nrow(status()), 1)

  #again with extension
  .createIfNotFound(module_name="DummyModule.R", column="LoadingState")
  expect_equal(.contains("DummyModule.R"), TRUE)
  expect_equal(nrow(status()), 2)

}
)


# hamsteR:: loading state 'Modules' readable? --------------------
test_that("loading state 'Modules' readable?", {

  reset()
  expect_equal(nrow(status()), 0)

  nModulesBefore = nrow(status())

  r = set_loading_state("DummyModule", "loading")
  #print(r == "ERROR: file 'DummyModule' has no '*.R' extension")
  expect_equal(r, "ERROR: file 'DummyModule' has no '*.R' extension")
  nModulesAfter = nrow(status())
  #cat("\nbefore: ", nModulesBefore, "\n")
  #print(r)
  #cat("after : ", nModulesAfter, "\n")
  #m = status()
  #print(m)
  expect_equal(nModulesAfter, nModulesBefore)

  nModulesBefore = nrow(status())
  r = set_loading_state("DummyModule.R", "loading")
  #print(r)
  expect_equal(r, "'DummyModule.R' now marked as 'loading'.")
  nModulesAfter = nrow(status())
  # cat("\nbefore: ", nModulesBefore, "\n")
  # print(r)
  # cat("after : ", nModulesAfter, "\n")
  # m = status()
  # print(m)
  expect_equal(nModulesAfter - nModulesBefore, 1)
  }
)



# hamsteR:: loaded or not? --------------------
test_that("loaded or not?", {

  reset()
  r = set_loading_state("DummyModule", "loading")
  #print(r)
  expect_equal(r, "ERROR: file 'DummyModule' has no '*.R' extension")
  # a = substr(is_loading("DummyModule"), 1, 5) == c(err="ERROR")
  # print(is_loading("DummyModule"))
  # print(class(is_loading("DummyModule")))
  # expect_equal(a, TRUE)
  expect_equal(substr(check_loading_state("DummyModule", check_state="loading"), 1, 5), "ERROR")

  reset()
  s = set_loading_state("DummyModule.R", "loading")
  #print(s)
  # cat("\n")
  # print(status(), rownames = TRUE)
  # cat("\n")
  l = check_loading_state("DummyModule.R", check_state="loading")
  #print(l)
  expect_equal(check_loading_state("DummyModule.R", check_state="loading"), TRUE)
  expect_equal(check_loading_state("DummyModule.R", check_state="loaded"), FALSE)
  expect_equal(check_loading_state("DummyModule.R", check_state="undefined"), FALSE)

  reset()
  l = set_loading_state("DummyModule.R", "loaded")
  expect_equal(check_loading_state("DummyModule.R", check_state="loading"), FALSE)
  expect_equal(check_loading_state("DummyModule.R", check_state="loaded"), TRUE)
  expect_equal(check_loading_state("DummyModule.R", check_state="undefined"), FALSE)

  reset()
  l = set_loading_state("DummyModule.R", "undefined")
  expect_equal(check_loading_state("DummyModule.R", check_state="loading"), FALSE)
  expect_equal(check_loading_state("DummyModule.R", check_state="loaded"), FALSE)
  expect_equal(check_loading_state("DummyModule.R", check_state="undefined"), TRUE)

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
})
