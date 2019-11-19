# TEST script 1

hamsteR::set_loading_state("TEST_01.R", "loading")

TEST_01_cnt <<- get0("TEST_01_cnt", mode="numeric", ifnotfound=0) + 1
message("TEST_01 call counter: ", TEST_01_cnt)

# set to loaded
hamsteR::set_loading_state("TEST_01.R", "loaded")
