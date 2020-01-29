Assignment 1
================

# 1 Blood types genetics

### Ex 1.1

\(\Omega\) = {I<sup>A</sup>, I<sup>A</sup>i, I<sup>B</sup>I<sup>B</sup>,
I<sup>B</sup>i, I<sup>A</sup>I<sup>B</sup>, ii}

### Ex 1.2

P(ii) = 0.25  
P(I<sup>A</sup>i, ii) = 0.75

### Ex 1.3

P(C = I<sup>A</sup>I<sup>B</sup>) = 0.25

### Ex 1.4

Phenotypes: AB, A, B  
Distribution: AB = 0.25; A = 0.375; B = 0.375

### Ex 1.5

P(I<sup>B</sup>i, I<sup>B</sup>I<sup>B</sup> | B =
I<sup>A</sup>I<sup>B</sup>) = 0.375

## 1.1 Some simulation in R

### Ex 2.1

``` r
sample(c("ii", "iB", "iA", "BB", "AA", "AB"), size = 1, replace = TRUE)
```

### Ex 2.2

``` r
offspringG <- function(Parent1, Parent2){
  Allele1 <- sample(unlist(strsplit(Parent1, split = "")), replace = TRUE, size=1)
  Allele2 <- sample(unlist(strsplit(Parent2, split = "")), replace = TRUE, size=1)
  return(paste(Allele1, Allele2, sep = ""))
}


offspringP <- function(Genotype){
  Phenotype <- ""
  if (Genotype == "ii"){
    Phenotype <- "Type O"
  }
  if (Genotype == "AB" | Genotype == "BA"){
    Phenotype <- "Type AB"
  }
  if (Genotype == "iB" | Genotype == "Bi" | Genotype == "BB"){
    Phenotype <- "Type B"
  }
  if (Genotype == "iA" | Genotype == "Ai" | Genotype == "AA"){
    Phenotype <- "Type A"
  }
  return(Phenotype)
}
```

### Ex 2.3

``` r
offspingiAiA <- c()
for(i in 1:1000){
  offspingiAiA[i] <- offspringG("iA", "iA")
}
sum(offspingiAiA == "ii")/length(offspingiAiA)
```

    ## [1] 0.253

``` r
sum(offspingiAiA != "AA")/length(offspingiAiA)
```

    ## [1] 0.742

### Ex 2.4

``` r
secondGen <- c()

for(i in 1:1000){
  A <- offspringG("iA", "iB")
  B <- offspringG("AB", "AB")
  C <- offspringP(offspringG(A, B))
  secondGen[i] <- C
}

secondGen
```

    ##    [1] "Type A"  "Type A"  "Type A"  "Type B"  "Type AB" "Type A"  "Type A" 
    ##    [8] "Type A"  "Type B"  "Type B"  "Type AB" "Type A"  "Type A"  "Type A" 
    ##   [15] "Type B"  "Type AB" "Type A"  "Type B"  "Type B"  "Type B"  "Type A" 
    ##   [22] "Type AB" "Type A"  "Type A"  "Type B"  "Type B"  "Type B"  "Type A" 
    ##   [29] "Type A"  "Type A"  "Type B"  "Type B"  "Type B"  "Type B"  "Type A" 
    ##   [36] "Type A"  "Type A"  "Type A"  "Type B"  "Type B"  "Type AB" "Type A" 
    ##   [43] "Type A"  "Type AB" "Type A"  "Type A"  "Type A"  "Type AB" "Type A" 
    ##   [50] "Type A"  "Type AB" "Type A"  "Type B"  "Type A"  "Type A"  "Type A" 
    ##   [57] "Type A"  "Type AB" "Type A"  "Type A"  "Type A"  "Type B"  "Type AB"
    ##   [64] "Type AB" "Type AB" "Type A"  "Type B"  "Type AB" "Type AB" "Type A" 
    ##   [71] "Type B"  "Type B"  "Type A"  "Type B"  "Type AB" "Type A"  "Type A" 
    ##   [78] "Type B"  "Type AB" "Type A"  "Type AB" "Type AB" "Type AB" "Type A" 
    ##   [85] "Type A"  "Type A"  "Type A"  "Type A"  "Type AB" "Type B"  "Type A" 
    ##   [92] "Type A"  "Type AB" "Type A"  "Type A"  "Type A"  "Type A"  "Type B" 
    ##   [99] "Type B"  "Type AB" "Type B"  "Type B"  "Type AB" "Type A"  "Type A" 
    ##  [106] "Type AB" "Type AB" "Type A"  "Type B"  "Type A"  "Type A"  "Type AB"
    ##  [113] "Type AB" "Type AB" "Type B"  "Type B"  "Type A"  "Type A"  "Type B" 
    ##  [120] "Type B"  "Type A"  "Type B"  "Type AB" "Type AB" "Type AB" "Type A" 
    ##  [127] "Type AB" "Type B"  "Type A"  "Type AB" "Type B"  "Type AB" "Type AB"
    ##  [134] "Type AB" "Type AB" "Type AB" "Type AB" "Type B"  "Type B"  "Type A" 
    ##  [141] "Type B"  "Type B"  "Type B"  "Type A"  "Type B"  "Type A"  "Type A" 
    ##  [148] "Type AB" "Type A"  "Type A"  "Type B"  "Type B"  "Type B"  "Type B" 
    ##  [155] "Type A"  "Type A"  "Type B"  "Type A"  "Type AB" "Type AB" "Type AB"
    ##  [162] "Type B"  "Type AB" "Type B"  "Type AB" "Type B"  "Type AB" "Type A" 
    ##  [169] "Type AB" "Type A"  "Type B"  "Type B"  "Type AB" "Type A"  "Type B" 
    ##  [176] "Type B"  "Type A"  "Type B"  "Type AB" "Type A"  "Type B"  "Type AB"
    ##  [183] "Type B"  "Type B"  "Type A"  "Type AB" "Type A"  "Type A"  "Type A" 
    ##  [190] "Type A"  "Type B"  "Type AB" "Type B"  "Type A"  "Type A"  "Type A" 
    ##  [197] "Type B"  "Type B"  "Type AB" "Type A"  "Type A"  "Type B"  "Type B" 
    ##  [204] "Type A"  "Type A"  "Type B"  "Type AB" "Type A"  "Type B"  "Type B" 
    ##  [211] "Type B"  "Type A"  "Type A"  "Type B"  "Type A"  "Type B"  "Type AB"
    ##  [218] "Type AB" "Type A"  "Type B"  "Type A"  "Type A"  "Type AB" "Type AB"
    ##  [225] "Type AB" "Type B"  "Type AB" "Type A"  "Type AB" "Type A"  "Type B" 
    ##  [232] "Type AB" "Type B"  "Type B"  "Type AB" "Type B"  "Type A"  "Type B" 
    ##  [239] "Type A"  "Type B"  "Type B"  "Type AB" "Type B"  "Type B"  "Type AB"
    ##  [246] "Type A"  "Type A"  "Type AB" "Type A"  "Type A"  "Type B"  "Type A" 
    ##  [253] "Type B"  "Type AB" "Type AB" "Type A"  "Type A"  "Type B"  "Type B" 
    ##  [260] "Type A"  "Type A"  "Type AB" "Type A"  "Type B"  "Type B"  "Type AB"
    ##  [267] "Type B"  "Type B"  "Type A"  "Type B"  "Type B"  "Type B"  "Type A" 
    ##  [274] "Type AB" "Type AB" "Type B"  "Type B"  "Type A"  "Type A"  "Type B" 
    ##  [281] "Type AB" "Type A"  "Type AB" "Type B"  "Type B"  "Type A"  "Type A" 
    ##  [288] "Type B"  "Type B"  "Type AB" "Type AB" "Type AB" "Type A"  "Type B" 
    ##  [295] "Type B"  "Type B"  "Type A"  "Type B"  "Type B"  "Type AB" "Type AB"
    ##  [302] "Type A"  "Type B"  "Type B"  "Type A"  "Type B"  "Type AB" "Type A" 
    ##  [309] "Type A"  "Type B"  "Type B"  "Type AB" "Type B"  "Type A"  "Type B" 
    ##  [316] "Type AB" "Type A"  "Type B"  "Type A"  "Type A"  "Type B"  "Type A" 
    ##  [323] "Type A"  "Type AB" "Type B"  "Type AB" "Type A"  "Type A"  "Type AB"
    ##  [330] "Type B"  "Type AB" "Type AB" "Type AB" "Type B"  "Type B"  "Type A" 
    ##  [337] "Type AB" "Type B"  "Type A"  "Type A"  "Type A"  "Type B"  "Type B" 
    ##  [344] "Type AB" "Type B"  "Type B"  "Type A"  "Type B"  "Type AB" "Type A" 
    ##  [351] "Type A"  "Type B"  "Type B"  "Type AB" "Type AB" "Type A"  "Type A" 
    ##  [358] "Type AB" "Type AB" "Type B"  "Type B"  "Type B"  "Type AB" "Type B" 
    ##  [365] "Type A"  "Type B"  "Type AB" "Type A"  "Type AB" "Type AB" "Type B" 
    ##  [372] "Type B"  "Type B"  "Type B"  "Type B"  "Type AB" "Type A"  "Type AB"
    ##  [379] "Type A"  "Type A"  "Type B"  "Type B"  "Type A"  "Type A"  "Type B" 
    ##  [386] "Type B"  "Type A"  "Type A"  "Type B"  "Type B"  "Type B"  "Type B" 
    ##  [393] "Type B"  "Type B"  "Type A"  "Type A"  "Type A"  "Type B"  "Type A" 
    ##  [400] "Type B"  "Type B"  "Type B"  "Type A"  "Type A"  "Type AB" "Type AB"
    ##  [407] "Type A"  "Type AB" "Type B"  "Type AB" "Type B"  "Type A"  "Type B" 
    ##  [414] "Type A"  "Type A"  "Type B"  "Type AB" "Type A"  "Type B"  "Type A" 
    ##  [421] "Type B"  "Type A"  "Type A"  "Type B"  "Type A"  "Type A"  "Type AB"
    ##  [428] "Type A"  "Type A"  "Type B"  "Type B"  "Type A"  "Type A"  "Type A" 
    ##  [435] "Type AB" "Type B"  "Type AB" "Type A"  "Type B"  "Type B"  "Type AB"
    ##  [442] "Type A"  "Type A"  "Type A"  "Type A"  "Type A"  "Type AB" "Type A" 
    ##  [449] "Type A"  "Type AB" "Type AB" "Type B"  "Type AB" "Type A"  "Type B" 
    ##  [456] "Type AB" "Type B"  "Type A"  "Type A"  "Type A"  "Type B"  "Type A" 
    ##  [463] "Type AB" "Type B"  "Type B"  "Type AB" "Type AB" "Type B"  "Type AB"
    ##  [470] "Type AB" "Type A"  "Type B"  "Type B"  "Type AB" "Type A"  "Type A" 
    ##  [477] "Type A"  "Type A"  "Type B"  "Type AB" "Type A"  "Type A"  "Type B" 
    ##  [484] "Type B"  "Type B"  "Type A"  "Type AB" "Type A"  "Type A"  "Type B" 
    ##  [491] "Type AB" "Type B"  "Type B"  "Type B"  "Type B"  "Type B"  "Type AB"
    ##  [498] "Type B"  "Type A"  "Type A"  "Type A"  "Type AB" "Type A"  "Type B" 
    ##  [505] "Type AB" "Type A"  "Type A"  "Type AB" "Type B"  "Type AB" "Type AB"
    ##  [512] "Type A"  "Type A"  "Type AB" "Type B"  "Type B"  "Type AB" "Type B" 
    ##  [519] "Type B"  "Type A"  "Type A"  "Type B"  "Type B"  "Type B"  "Type A" 
    ##  [526] "Type A"  "Type B"  "Type AB" "Type B"  "Type AB" "Type B"  "Type A" 
    ##  [533] "Type A"  "Type AB" "Type AB" "Type A"  "Type A"  "Type B"  "Type B" 
    ##  [540] "Type B"  "Type B"  "Type A"  "Type A"  "Type A"  "Type A"  "Type B" 
    ##  [547] "Type A"  "Type B"  "Type B"  "Type A"  "Type B"  "Type A"  "Type AB"
    ##  [554] "Type B"  "Type B"  "Type B"  "Type AB" "Type AB" "Type AB" "Type B" 
    ##  [561] "Type AB" "Type B"  "Type B"  "Type B"  "Type B"  "Type A"  "Type A" 
    ##  [568] "Type B"  "Type B"  "Type AB" "Type B"  "Type B"  "Type A"  "Type AB"
    ##  [575] "Type A"  "Type AB" "Type AB" "Type B"  "Type A"  "Type A"  "Type B" 
    ##  [582] "Type B"  "Type B"  "Type AB" "Type AB" "Type AB" "Type B"  "Type B" 
    ##  [589] "Type B"  "Type AB" "Type A"  "Type B"  "Type B"  "Type B"  "Type AB"
    ##  [596] "Type B"  "Type A"  "Type AB" "Type A"  "Type A"  "Type AB" "Type B" 
    ##  [603] "Type A"  "Type A"  "Type B"  "Type A"  "Type AB" "Type A"  "Type A" 
    ##  [610] "Type AB" "Type B"  "Type A"  "Type B"  "Type AB" "Type A"  "Type A" 
    ##  [617] "Type B"  "Type A"  "Type AB" "Type AB" "Type A"  "Type B"  "Type AB"
    ##  [624] "Type B"  "Type B"  "Type B"  "Type A"  "Type A"  "Type A"  "Type B" 
    ##  [631] "Type B"  "Type B"  "Type AB" "Type A"  "Type B"  "Type B"  "Type B" 
    ##  [638] "Type B"  "Type B"  "Type B"  "Type A"  "Type B"  "Type A"  "Type A" 
    ##  [645] "Type A"  "Type A"  "Type A"  "Type A"  "Type AB" "Type B"  "Type B" 
    ##  [652] "Type B"  "Type A"  "Type AB" "Type AB" "Type A"  "Type A"  "Type AB"
    ##  [659] "Type AB" "Type B"  "Type A"  "Type B"  "Type A"  "Type A"  "Type A" 
    ##  [666] "Type AB" "Type B"  "Type AB" "Type B"  "Type A"  "Type B"  "Type A" 
    ##  [673] "Type B"  "Type A"  "Type B"  "Type A"  "Type B"  "Type B"  "Type A" 
    ##  [680] "Type A"  "Type A"  "Type A"  "Type A"  "Type A"  "Type B"  "Type A" 
    ##  [687] "Type A"  "Type A"  "Type B"  "Type B"  "Type A"  "Type B"  "Type B" 
    ##  [694] "Type A"  "Type B"  "Type B"  "Type A"  "Type B"  "Type AB" "Type B" 
    ##  [701] "Type B"  "Type B"  "Type B"  "Type AB" "Type AB" "Type B"  "Type AB"
    ##  [708] "Type A"  "Type AB" "Type B"  "Type B"  "Type B"  "Type AB" "Type B" 
    ##  [715] "Type B"  "Type AB" "Type B"  "Type B"  "Type AB" "Type B"  "Type AB"
    ##  [722] "Type B"  "Type A"  "Type B"  "Type AB" "Type A"  "Type B"  "Type B" 
    ##  [729] "Type B"  "Type B"  "Type A"  "Type B"  "Type AB" "Type B"  "Type B" 
    ##  [736] "Type AB" "Type A"  "Type A"  "Type B"  "Type B"  "Type A"  "Type B" 
    ##  [743] "Type B"  "Type B"  "Type B"  "Type A"  "Type B"  "Type A"  "Type AB"
    ##  [750] "Type B"  "Type B"  "Type A"  "Type A"  "Type A"  "Type AB" "Type AB"
    ##  [757] "Type AB" "Type B"  "Type A"  "Type AB" "Type A"  "Type B"  "Type AB"
    ##  [764] "Type AB" "Type B"  "Type A"  "Type AB" "Type A"  "Type AB" "Type AB"
    ##  [771] "Type B"  "Type A"  "Type B"  "Type A"  "Type B"  "Type A"  "Type A" 
    ##  [778] "Type AB" "Type AB" "Type B"  "Type B"  "Type B"  "Type B"  "Type A" 
    ##  [785] "Type AB" "Type A"  "Type A"  "Type A"  "Type A"  "Type A"  "Type B" 
    ##  [792] "Type A"  "Type B"  "Type B"  "Type AB" "Type B"  "Type B"  "Type B" 
    ##  [799] "Type B"  "Type AB" "Type A"  "Type B"  "Type AB" "Type A"  "Type B" 
    ##  [806] "Type AB" "Type B"  "Type A"  "Type A"  "Type B"  "Type AB" "Type A" 
    ##  [813] "Type AB" "Type A"  "Type AB" "Type AB" "Type AB" "Type B"  "Type B" 
    ##  [820] "Type A"  "Type A"  "Type AB" "Type B"  "Type AB" "Type B"  "Type A" 
    ##  [827] "Type A"  "Type A"  "Type AB" "Type A"  "Type B"  "Type A"  "Type B" 
    ##  [834] "Type A"  "Type A"  "Type B"  "Type A"  "Type AB" "Type AB" "Type B" 
    ##  [841] "Type B"  "Type AB" "Type B"  "Type AB" "Type A"  "Type A"  "Type AB"
    ##  [848] "Type B"  "Type A"  "Type AB" "Type AB" "Type AB" "Type A"  "Type AB"
    ##  [855] "Type B"  "Type A"  "Type A"  "Type B"  "Type B"  "Type B"  "Type A" 
    ##  [862] "Type AB" "Type B"  "Type A"  "Type B"  "Type AB" "Type A"  "Type AB"
    ##  [869] "Type A"  "Type B"  "Type A"  "Type A"  "Type B"  "Type A"  "Type A" 
    ##  [876] "Type AB" "Type AB" "Type AB" "Type A"  "Type B"  "Type A"  "Type AB"
    ##  [883] "Type B"  "Type A"  "Type AB" "Type A"  "Type B"  "Type AB" "Type AB"
    ##  [890] "Type AB" "Type A"  "Type AB" "Type B"  "Type AB" "Type B"  "Type A" 
    ##  [897] "Type A"  "Type A"  "Type B"  "Type A"  "Type A"  "Type A"  "Type B" 
    ##  [904] "Type B"  "Type B"  "Type AB" "Type AB" "Type A"  "Type A"  "Type AB"
    ##  [911] "Type B"  "Type A"  "Type B"  "Type A"  "Type A"  "Type B"  "Type AB"
    ##  [918] "Type B"  "Type B"  "Type AB" "Type A"  "Type B"  "Type A"  "Type A" 
    ##  [925] "Type B"  "Type A"  "Type B"  "Type B"  "Type A"  "Type B"  "Type B" 
    ##  [932] "Type A"  "Type A"  "Type A"  "Type AB" "Type B"  "Type B"  "Type B" 
    ##  [939] "Type B"  "Type A"  "Type AB" "Type AB" "Type AB" "Type B"  "Type B" 
    ##  [946] "Type B"  "Type A"  "Type AB" "Type AB" "Type AB" "Type B"  "Type AB"
    ##  [953] "Type B"  "Type AB" "Type B"  "Type A"  "Type AB" "Type A"  "Type AB"
    ##  [960] "Type B"  "Type AB" "Type B"  "Type A"  "Type AB" "Type AB" "Type B" 
    ##  [967] "Type A"  "Type B"  "Type B"  "Type A"  "Type A"  "Type AB" "Type B" 
    ##  [974] "Type A"  "Type B"  "Type A"  "Type B"  "Type A"  "Type AB" "Type B" 
    ##  [981] "Type AB" "Type A"  "Type B"  "Type A"  "Type B"  "Type A"  "Type AB"
    ##  [988] "Type A"  "Type B"  "Type A"  "Type A"  "Type B"  "Type AB" "Type A" 
    ##  [995] "Type AB" "Type B"  "Type A"  "Type AB" "Type B"  "Type B"

# 2 A case study of DNA sequence

### Ex 3.1

\(\Omega\) = {AA, AT, AC, AG, TA, TT, TC, TG, CA, CT, CC, CG, GA, GT,
GC, GG}

### Ex 3.2

P(ACG) = (0.25 \(\times\) 0.25 \(\times\) 0.25) \(\times\) 3 = 0.047

### Ex 3.3

\(\Omega\) = {Ø, A, T, C, AA, AT, AC, AAA, …}  
P(length 10 G<sup>C</sup>) = (0.75)<sup>10</sup> = 0.05

### Ex 3.4

1: P(CG|C) = 0.5 \(\times\) 0.1 = 0.05  
2: P(CG|C) = 0.3 \(\times\) 0.4 = 0.12

## 2.1 Some simulations in R

### Ex 4.1

``` r
ACGsim <- function(reps){
  count = 0
  
  for(i in 1:reps){
    sqce_5 <- paste(sample(c("A", "T", "G", "C"), replace = TRUE, size = 5), collapse = "")
    
    if(grepl("ACG", sqce_5, fixed = TRUE)){
      count <- count + 1
    }
  }
  return(count/reps)
}
ACGsim(10000)
```

    ## [1] 0.0443

### Ex 4.2

``` r
Gsim <- function(reps, length = 10){
  sqces_with_G <- 0
  for(i in 1:reps){
    seq <- sample(c("A", "C", "T", "G"), replace =TRUE, size = length)
    if ("G" %in% seq){
      sqces_with_G <- sqces_with_G + 1
    }
  }
  sqces_with_no_G <- reps - sqces_with_G
  return(sqces_with_no_G/reps)
}

Gsim(10000)
```

    ## [1] 0.0561

### Ex 4.3

``` r
simG <- c()
pG <- c()
for(i in 1:20){
  simG[i] <- Gsim(10000, length = i)
  pG[i] <- dbinom(i, size = i, prob = 0.75)
}

plot(1:20, simG, col = "blue", pch = 0, xlab = "number of reps")
points(1:20, pG, col = "red", pch = 1)
legend("topright",legend = c("simulation", "probability"), 
       col = c("blue", "red"), pch = c(0, 1))
```

![](assignment1_rmd_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# 3 A case study of neuronal data

### Ex 5.1 and 5.2

``` r
isidata <- read.table("neuronspikes.txt", col.names = "isi")
hist(isidata$isi, breaks = 50, probability = TRUE)

for(i in seq(0.5, 1.5, 0.1)){
  curve(dexp((x), rate = i), from = 0, to = 5, col= (i*10)-3, add = TRUE)
}

legend("topright", legend=c(seq(0.5,1.5,0.1)), 
       col=c(2:length(seq(0.5,1.5,0.1))), lty = 1)
```

![](assignment1_rmd_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

A \(\lambda\) value of 1.5 fits the data the best.

### Ex 5.3

``` r
pexp(1, rate = 1.5)
```

    ## [1] 0.7768698

``` r
pexp(1.5, rate = 1.5) - pexp(0.5, rate = 1.5)
```

    ## [1] 0.3669673

# 4 Brain cell database

### Ex 6.1

The option na.strings converts empty strings to NA values.

### Ex 6.2

The donor species column indicates which values are humans and which are
mice.

``` r
sum(cells$donor__species == "Homo Sapiens") / length(cells[,1])
```

    ## [1] 0.1770253

### Ex 6.3

``` r
hist(cells$ef__peak_t_ramp[cells$donor__species == "Homo Sapiens"], 
     proba = TRUE,  xlab = "Rank Spike Time (s)", 
     breaks = 50, main = "Rank Spike Time frequencies for Homo Sapiens")
```

![](assignment1_rmd_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
hist(cells$ef__peak_t_ramp[cells$donor__species == "Mus musculus"], 
     proba = TRUE, xlab = "Rank Spike Time (s)", 
     breaks = 50, main = "Rank Spike Time frequencies for Mus musculus")
```

![](assignment1_rmd_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

### Ex 6.4

``` r
first_plot = FALSE

for(i in 1:3){
  curve(dlnorm(x, sdlog = 0.6, meanlog = i), from = 0, to = 40, 
        add = first_plot, col = i+1, ylab = "log density function")
  first_plot = TRUE
}

legend("topright", legend = 1:3, col = 2:4, lty = 1)
```

![](assignment1_rmd_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### Ex 6.5

**meanlog = 2** is best, based on the fit of the corresponding density
function in the plot.

### Ex 6.6

``` r
hist(cells$ef__peak_t_ramp[cells$donor__species == "Homo Sapiens"], 
     proba = TRUE,  xlab = "Rank Spike Time (s)", 
     breaks = 50, main = "Rank Spike Time frequencies for Homo Sapiens")

curve(dlnorm(x, sdlog = 0.6, meanlog = 2), 
      from = 0, to = 40, add = TRUE, 
      col = 3, ylab = "log density function")
```

![](assignment1_rmd_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Yes, the density fits the
data.

### Ex 6.7

``` r
sum(cells$donor__species == "Homo Sapiens" & cells$donor__sex == "Male")
```

    ## [1] 234

``` r
sum(cells$donor__species == "Homo Sapiens" & cells$donor__sex == "Female")
```

    ## [1] 179
