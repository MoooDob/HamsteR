# TEST script 5

TEST_05_cnt <<- get0("TEST_05_cnt", mode="numeric", ifnotfound=0) + 1
message("TEST_05 call counter: ", TEST_05_cnt)

# loading state unchanged
