devtools::document()
devtools::build_vignettes()
devtools::build()
devtools::install(build_vignettes = TRUE)
#install.packages("/media/PhD/GitHub/essexBigdata_0.1.0.tar.gz", repos = NULL, type = "source")
library(essexBigdata)
vignette("practical1", package = "essexBigdata")


data("measure")
head(measure)

?measure
vignette("practical2", package = "essexBigdata")



