expect_that(rmLcPrefix(c('abcd','abef','aqwe')), equals(c('bcd','bef','qwe')))
expect_that(rmLcPrefix(c('abcd','abef','abqwe')), equals(c('cd','ef','qwe')))
expect_that(rmLcPrefix(c('abc','abc')), equals(c('','')))
expect_that(rmLcPrefix(c('abc')), equals(''))
expect_that(rmLcPrefix(''), equals(''))
expect_that(rmLcPrefix(NULL), equals(NULL))

expect_that(rmLcPrefix(factor(c('abc','abd'))), equals(c('c','d')))

