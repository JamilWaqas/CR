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

**Note:-** Algorithms are not optimised for performance. If inverse is taken using ```solve()```, then ORR and AAR are quite slower than ORR and AAR. When ```chol2inv(chol())``` is used than they are faster than OSLOG and COIRR. However, COIRR and OSLOG has ability to give sparse solution. Also, there might be room for further optimising multiplication that involve diagonal matrices. Other techniques like Sherman-Morrison can also be used. This aspect is still work in progress, so please feel to improve the current implementations.

# Example

In order to run the example mentioned herein ```caret,glmnet``` and ```devtools``` packages are required. The ```devtools``` is required if you want to use all the available functions in the package. You can use the command ```install.packages("pkg.name")``` and ```inatall.gthub("Jamil/CR")``` followed by ```library(pkg.name)```. Please follow the same order. To run the example ```demo.R``` type ```source("https://raw.githubusercontent.com/JamilWaqas/CR/master/demo.R")``` in ```R```. Also, see ```?AAR```,```?ORR```,...
