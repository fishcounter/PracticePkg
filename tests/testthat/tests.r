## tests of FARS functions

test_that("Invalid State",{
	expect_error(fars_map_state(0,2013))
	})

	
test_that("Bad year requests",{
	expect_warning(fars_read_years(2016))	
	})
