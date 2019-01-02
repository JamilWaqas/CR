## Proto-type Algorithms

* AAR 
* ORR 
* COIRR 
* OSLOG


## Benchmarks-glmnet

* lm  regression
* L1 regression
* L2 regression

## Grid search tuning 

* tuneAAR
* tuneORR
* tuneCOIRR
* tuneOSLOG

## Time-microbenchmark

* time 

**Note:-** Algorithms are not optimised for performance. If inverse is taken using ```solve()```, then ORR and AAR are quite slower than ORR and AAR. When ```chol2inv(chol())``` is used than they are faster than OSLOG and COIRR. However, COIRR and OSLOG has ability to give sparse solution. Also, there might be room for further optimising multiplication that involve diagonal matrices. Other techniques like Sherman-Morrison can also be used, but it is not clear how to incorporate this practically. Please see [here](https://math.stackexchange.com/questions/3051972/woodbury-matrix-inversion#). Also, the current implementations are just proto-types, which possibly also effect the computationally efficiency. These aspect are still work in progress, so please feel to improve the current implementations.

# Instructions

Open **R** and install the following packages ```microbenchmark, caret, glmnet``` and ```devtools``` by typing ```install.packages("pkg.name")``` followed by ```library(pkg.name)```. Then type ```inatall.gthub("Jamil/CR")```

# Example

Type ```source("https://raw.githubusercontent.com/JamilWaqas/CR/master/demo.R")``` in **R**.
