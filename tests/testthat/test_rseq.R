context('rseq')

test_that("empty rseq obtained", {
    expect_that(length(rseq(1,0,1)), equals(0))
    expect_that(length(rseq(0,1,-1)), equals(0))
    expect_that(rseq(0,0,0), equals(0))
})

test_that("valid rseq obtained", {
    expect_that(rseq(0,1), equals(seq(0,1)))
    expect_that(rseq(0,1,1), equals(seq(0,1)))
    expect_that(rseq(1,0), equals(seq(1,0)))
    expect_that(rseq(1,0,-1), equals(seq(1,0)))
})
