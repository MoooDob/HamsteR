
# hamsteR

<!-- badges: start -->
<!-- badges: end -->

The *hamsteR* provides a conditional `source`. That way it is possible to only load and parse source files under a certain condition. *hamsteR* defines three loading states as conditions: 

* **_undefined_**: the code is not marked as _loading_ or _loaded_ 

* **_loading_**: the code is currently beeing processed (for ) 

* **_loaded_**: the code is already loaded and do not need to be loaded again.

When calling `source_ifnotloaded`, the conditional `source` function, source files marked as  _loading_ or _loaded_ won't be loaded and parsed.

## Installation

You can install the released version of hamsteR from [CRAN](https://github.com/MoooDob/hamsteR) with:

``` r
library(devtools)
install_github("MoooDob/hamsteR")
```

## Background

The general workflow of a data analysis is to _knead_ the basic data until the answer to a predefined question becomes visible. For that, its possible to filter, select, group, arrange, combine, substract, enrich, summarize and analyse the data, just to name the most important.

Data analysis is often not done in one long superduper command, but in a lot of small steps. Developers do a step by step approximation to the desired answer. If each step is one line of code in a so-called script, data analysis projects in R can easily reach tens of thousands of lines of code. 

Compared to classical software development projects, many of the functions used are non-atomic, but comparatively complex and time consuming. So if you are a developer and want to test your code, even for a small script and depending on the size of the data to be processed the runtime can reach a few hours. In the bad case after a few hours you will get the indication that you have made a programming error. 

Unfortunately, this bad case is not at all rare. Even for a very experienced developer, it seems to be impossible to create error-free code without stepwise testing. Often in data analysis, subsequent steps can be condensed to _functional units_. Each of these follow the IPO principle: Input (some data), processing (filtering, selecting, ...) and output (table, plot, report, log, ...). Each unit is, appart from input and output, independed of the others.

R provides different solutions for the problems of 'programming in the big'. 'Programming in the big' means in the case of R not only that the program code measured in lines of code is _'_big_, more often it means that parts of the R script will take a long time to present the desired results. Especially while developing and testing a script or function the later meaning is very time consuming. For that, often interim results will be produced by the code parts that were executed before the one under current development. 

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

