
# hamsteR

<!-- badges: start -->
<!-- badges: end -->

The core of hamsteR is to replace base::source with a slightly extended version with a conditional loading feature. That way it is possible to only load a source file under certain conditions. hamsteR predefines three conditions as loading states: 'undefined', 'loading' and 'loaded'. Source files defined as already 'loaded' or currently 'loading' won't be loaded again.

## Installation

You can install the released version of hamsteR from [CRAN](https://github.com/MoooDob/hamsteR) with:

``` r
install.packages("hamsteR")
```

## Background

R provides different solutions for the problems of the 'programming in the big'. 'Programming in the big' means in the case of R not only that the program code measured in lines of code is 'big', more often it means that parts of the R script will take a long time to present the desired results. Especially while developing and testing the script the later meaning is very time consuming. For that, often interim results will be produced by the code parts that were executed before the one under current development. 

base::source is the preferred way to glue the different parts together. base::source will simply load the given file/code and parse it. So if you want to decide if the script really has to be loaded again, this decission has to be done in the script itself. This cant be done by checking the states of some global variables or options or by checking the existence or the content of some files. 

While testing  

The analysis of larger or more complex data sets usually 
takes place in a lot of small steps. Each of these steps uses input data, 
possibly producing intermediate products and finally end products 
(data, statitics, graphics, report,...). 
The end products then go back into subsequent steps. This is commonly 
called the 'workflow' of the analysis.
In a lot of cases it is not necessary to run through all these steps. 
Often it is possible to fall back on previously created products. 

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(hamsteR)
## basic example code
```

