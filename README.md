# Disclaimer

These scripts are made available for reproducibility purpose, and are neither well documented nor maintained. If you find a bug you can let me know, but I do not guarantee that I will take time to fix it.


# Irreducible Label Factorization

This repository contains the R code used to run the benchmark between BR, LP and ILF-Compo in the ICML 2015 paper [*On the Optimality of Multi-Label Classification under Subset Zero-One Loss for Distributions Satisfying the Composition Property*](http://jmlr.org/proceedings/papers/v37/gasse15.html). 

The emotions, scene, yeast, slashdot, genbase, medical, enron, bibtex, and corel5k datasets are from the [Mulan](http://mulan.sourceforge.net/datasets.html) repository.

The image dataset is from [Zhou](http://lamda.nju.edu.cn/data_MIMLimage.ashx).

## Dependencies

A modified version of the [bnlearn](http://www.bnlearn.com/) R package is required for compatibility reasons (and also because it includes the K-IAMB and FDR-IAMB algorithms).

``` R
install.packages("devtools")
library("devtools")
install_github("gasse/bnlearn-clone-3.4")
```

Some packages to read/write Matlab and Weka file formats.

``` R
install.packages("R.matlab")
install.packages("RWeka")
install.packages("XML")
```

Packages to run the benchmark in parallel on a multi-threaded machine:

``` R
install.packages("synchronicity")
install.packages("snow")
``` 

One of the following base learners, depending on which one you choose:

``` R
install.packages("randomForest") # randomForest
install.packages(c("rJava", "extraTrees")) # extraTrees
install.packages("e1071") # svm
install.packages("RWeka") # smo
```

Rgraphviz to visualize ILF factorization graphs (optional):

``` R
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
```

Note that you will also need to install the [graphviz](http://www.graphviz.org/) binaries.


## Run the benchmark

``` R
source("00.main.R")
```

should run the whole thing.


## Description of the scripts

* **00.main.R**
is the main script to configure and run the whole benchmark.

* **01.data.extract.R**
converts each dataset to the R-friendly RDA file format, generates duplicated datasets, and the cross-validation splits (5x2 cv by default)

* **02.extract.mbs.in.x.R**
performs a feature subset selection for each label (with the standard IAMB algorithm by default).

* **03.extract.ilf.R**
performs two pairwise conditional tests of independence between each pair of labels, once with each Markov Boundary (the MB of the first label then the MB of the second label).

* **04.learn.models.R**
runs the base learner (randomForest by default) by following the BR, LP and ILF-Compo schemes.

* **05.results.R**
computes the performance measures on each dataset, namely the accuracy, global accuracy, recall, precision and F1-measure.

``` R
load("results.rda")

# Hamming loss
hloss = aggregate(1 - results[, "acc"], by = results[, c("meta", "db"), drop=F], FUN = mean)
array(hloss$x,
      dim = c(nlevels(hloss$meta), nlevels(hloss$db)),
      dimnames = lapply(hloss[, c("meta", "db")], levels))

# Subset 0-1 loss
hloss = aggregate(1 - results[, "gacc"], by = results[, c("meta", "db"), drop=F], FUN = mean)
array(hloss$x,
      dim = c(nlevels(hloss$meta), nlevels(hloss$db)),
      dimnames = lapply(hloss[, c("meta", "db")], levels))
```
print the mean Hamming loss and Subset 0-1 loss for each method and dataset.

* **11.plot.ilf.graph.R**
plots the factorization graphs produced by ILF-Compo (png by default).

* **12.plot.labels.mb.in.x.size.dist.R**
plots the distribution of the size of the selected feature subsets (Markov boundaries), for each dataset.

* **13.plot.labels.lf.size.dist.R**
plots the distribution of the size of the label factors output by ILF-Compo, for each dataset.


## Parameters

The default parameters to configure the benchmark can be changed in *00.main.R*

``` R
# fixed seed for reproducibility
conf.seed = 1

# number of jobs to run in parallel
conf.nb.cores = 2

# number of cross-validation splits / repetitions
conf.nb.cv = 2 
conf.nb.cv.reps = 5

# FSS method used in "02.extract.mbs.in.x.R"
# (look at the file "00.includes.R" for more details)
conf.fss = "iamb.sp-mi.pr0.a0.0001"

# CI testing method used in "03.extract.ilf.R"
# (look at the file "00.includes.R" for more details)
conf.labels.ci = "ci.sp-mi.pr0.a0.0001"

# base classifier to compare BR, LP and ILF-Compo
# may be one of "rf" (randomForest), "erf" (extraTrees), "svm" (e1071), "smo" (Weka)
conf.base.learner = "rf"

# which datasets to use for the benchmark / the duplicated benchmark
conf.dbs = c("emotions", "image", "scene") 
conf.dbs.x2 = c("emotions", "image", "scene")

# output format for the figures
# may be one of "png" or "eps" 
conf.plot = "png"

# enable / disable log
conf.log = TRUE
```