# TEST script 3

hamsteR::set_loading_state("TEST_03.R", "loading")

TEST_03_cnt <<- get0("TEST_03_cnt", mode="numeric", ifnotfound=0) + 1
message("TEST_03 call counter: ", TEST_03_cnt)

# the following line will be evaluated or not?
return ("TEST_03 loaded.")

# this line will be evaluated
hamsteR::set_loading_state("TEST_03.R", "loaded")
