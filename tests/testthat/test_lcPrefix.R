context('lcPrefix')

expect_that(lcPrefix(c('abcd','abef','aqwe')), equals('a'))
expect_that(lcPrefix(c('abcd','abef','abqwe')), equals('ab'))
expect_that(lcPrefix(c('abc','abc')), equals('abc'))
expect_that(lcPrefix(c('abc')), equals('abc'))
expect_that(lcPrefix(''), equals(''))
expect_that(lcPrefix(NULL), equals(character(0)))
expect_that(lcPrefix(factor(c('abc','abd'))), equals('ab'))

expect_that(lcPrefix(c('','')), equals(''))
