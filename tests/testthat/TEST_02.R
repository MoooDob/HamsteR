# TEST script 2

hamsteR::set_loading_state("TEST_02.R", "loading")

TEST_02_cnt <<- get0("TEST_02_cnt", mode="numeric", ifnotfound=0) + 1
message("TEST_02 call counter: ", TEST_02_cnt)

hamsteR::set_loading_state("TEST_02.R", "undefined")
