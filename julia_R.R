#install.packages("JuliaCall")

#devtools::install_github("Non-Contradiction/JuliaCall")

library(JuliaCall)

#julia <- julia_setup()

# julia$command("a = sqrt(2);"); julia$eval("a")
julia_command("a = sqrt(2);"); julia_eval("a")
#> [1] 1.414214
julia_eval("sqrt(2)")
#> [1] 1.414214
julia_call("sqrt", 2)
#> [1] 1.414214
julia_eval("sqrt")(2)
#> [1] 1.414214
julia_assign("x", sqrt(2)); julia_eval("x")
#> [1] 1.414214
julia_assign("rsqrt", sqrt); julia_call("rsqrt", 2)
#> [1] 1.414214
2 %>J% sqrt
#> [1] 1.414214

## You can use `julia$exists` as `exists` in R to test
## whether a function or name exists in Julia or not

julia_exists("sqrt")
#> [1] TRUE
julia_exists("c")
#> [1] FALSE

## Functions related to installing and using Julia packages

julia_install_package_if_needed("Optim")
julia_installed_package("Optim")
#> [1] "0.17.2"
julia_library("Optim")
