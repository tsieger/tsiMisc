# simple cases
expect_that(mixcol('black','white'), equals('#7F7F7F'))
expect_that(mixcol('black','white',.5), equals('#7F7F7F'))
expect_that(mixcol('black','white',0), equals('#000000'))
expect_that(mixcol('black','white',1), equals('#FFFFFF'))

# bad arguments
expect_error(mixcol())
expect_error(mixcol(NULL))
expect_error(mixcol(NULL,NULL))
expect_error(mixcol('a','b'))
expect_error(mixcol('red','red',-1))
expect_error(mixcol('red','red',NULL))
expect_error(mixcol('red','red','red'))

# vector arguments
expect_that(mixcol(c('red','green','blue'),c('black')), equals(c('#7F0000','#007F00','#00007F')))
expect_that(mixcol(c('red','green','blue'),c('black','white')), equals(c('#7F0000','#7FFF7F','#00007F')))
expect_that(mixcol(c('black'),c('white'),c(0,.5,1)), equals(c('#000000','#7F7F7F','#FFFFFF')))
expect_that(mixcol(c('black','black','black'),c('white','white','white'),c(0,.5,1)), equals(c('#000000','#7F7F7F','#FFFFFF')))
expect_that(mixcol(c('black','white'),c('white','black')), equals(c('#7F7F7F','#7F7F7F')))
expect_that(mixcol(c('black','white'),c('white','black'),.5), equals(c('#7F7F7F','#7F7F7F')))
expect_that(mixcol(c('black','white'),c('white','black'),c(0,1/3,2/3,1)), equals(c('#000000','#AAAAAA','#AAAAAA','#000000')))

