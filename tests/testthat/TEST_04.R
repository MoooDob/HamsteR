# TEST script 4

TEST_04_cnt <<- get0("TEST_04_cnt", mode="numeric", ifnotfound=0) + 1
message("TEST_04 call counter: ", TEST_04_cnt)

# loading state unchanged
