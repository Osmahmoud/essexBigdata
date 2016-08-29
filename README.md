
<!-- README.md is generated from README.Rmd. Please edit that file -->
Clustering and Classification in R: Osama Mahmoud and Berthold Lausen
---------------------------------------------------------------------

Notes, practicals and R code for the up coming [course](https://www.essex.ac.uk/iads/events/summer-school.aspx) at the University of Essex Big Data and Analytics Summer School, September 2016.

-   PDF version of course [slides](https://raw.githubusercontent.com/Osmahmoud/essexBigdata/master/slides.pdf).

Installing the `essexBigdata` package
-------------------------------------

The practicals and associated R commands, use a variety of R packages. The easiest way of installing all dependencies is to install `drat` first, as follows:

``` r
install.packages("drat")
```

Then use `drat` to add a new repository

``` r
drat::addRepo("Osmahmoud")
```

The way for installing packages should now work using

``` r
install.packages("essexBigdata")
```

Usage of practicals
-------------------

The `essexBigdata` package includes a number of practical files to guide participants to applications of clustering and classification techniques using R.

-   Practical1 - Clustering

``` r
vignette("practical1", package = "essexBigdata")
```
