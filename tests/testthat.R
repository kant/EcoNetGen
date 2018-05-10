library(testthat)
library(EcoNetGen)

## Windows i386 builds always time out when ggraph
## circle layout is present in README images.  CRAZY
skip <- (R.Version()$arch == "i386") &
  "windows" %in% tolower(Sys.info()[["sysname"]])

if(!skip){
  test_check("EcoNetGen")
}
