# simple cases
expect_that(rgb2hex(RGB(t(col2rgb('yellow')))), equals('#FFFF00'))
expect_that(rgb2hex(RGB(t(col2rgb(c('white','black'))))), equals(c('#FFFFFF','#000000')))
expect_that(rgb2hex(NULL), equals(NULL))

# bad arguments
expect_error(rgb2hex())
expect_warning(rgb2hex('a'))
expect_warning(rgb2hex('a','b'))
expect_error(rgb2hex('a','b','c'))
