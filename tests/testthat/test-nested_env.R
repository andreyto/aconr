context("nested_env")

test_that("nested environment copy works for deep copy",{
    x = list2env(list(a=2,b="env_x"))
    x$y = list2env(list(c=2,d="env_y"),parent = x)
    x$y$z = list2env(list(e=2,f="env_z"),parent = x$y)
    a = copy_env(x,deep = T)
    a$a = 3
    a$y$c = 3
    a$y$z$e = 3
    expect_equal(x$a,2)
    expect_equal(x$y$c,2)
    expect_equal(x$y$z$e,2)
})

