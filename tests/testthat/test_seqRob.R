expect_that(length(seqRob(1,0,1)), equals(0))
expect_that(length(seqRob(0,1,-1)), equals(0))
expect_that(seqRob(0,0,0), equals(0))

expect_that(seqRob(0,1), equals(seq(0,1)))
expect_that(seqRob(0,1,1), equals(seq(0,1)))
expect_that(seqRob(1,0), equals(seq(1,0)))
expect_that(seqRob(1,0,-1), equals(seq(1,0)))
