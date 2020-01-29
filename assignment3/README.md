Assignment 3
================

\#1 A Trignonometric Density \#\#\#Ex 1.0 How can you compute the
normalization of
![C\_{k}](https://latex.codecogs.com/png.latex?C_%7Bk%7D "C_{k}")?

  
![f(x|k) =
C\_{k}sin(x)^{k}](https://latex.codecogs.com/png.latex?f%28x%7Ck%29%20%3D%20C_%7Bk%7Dsin%28x%29%5E%7Bk%7D
"f(x|k) = C_{k}sin(x)^{k}")  
  
![C\_{k} = \\frac{f(x|k)}{sin(x)^{k}}
](https://latex.codecogs.com/png.latex?C_%7Bk%7D%20%3D%20%5Cfrac%7Bf%28x%7Ck%29%7D%7Bsin%28x%29%5E%7Bk%7D%7D%20
"C_{k} = \\frac{f(x|k)}{sin(x)^{k}} ")  

We can use this equation to caclulate the value of
![C\_{k}](https://latex.codecogs.com/png.latex?C_%7Bk%7D "C_{k}"). But
to solve this equation, we need to find the value of
![f(x|k)](https://latex.codecogs.com/png.latex?f%28x%7Ck%29 "f(x|k)")
first.

We know that ![f(x|k)](https://latex.codecogs.com/png.latex?f%28x%7Ck%29
"f(x|k)") is porportional to the probability densiy of
![sin(x)^{k}](https://latex.codecogs.com/png.latex?sin%28x%29%5E%7Bk%7D
"sin(x)^{k}"), which just means
![f(x|k)](https://latex.codecogs.com/png.latex?f%28x%7Ck%29 "f(x|k)") is
a probability of
![sin(x)^{k}](https://latex.codecogs.com/png.latex?sin%28x%29%5E%7Bk%7D
"sin(x)^{k}"). In order to calculate the probability of something, we
just need to divide it by the total number of possibilities, which in
this case is the area under the curve
![sin(x)^{k}](https://latex.codecogs.com/png.latex?sin%28x%29%5E%7Bk%7D
"sin(x)^{k}"). Therefore:

  
![f(x|k) = \\frac{sin(x)^{k}}{\\int\_0^\\pi\\\!sin(x)^{k} dx}
](https://latex.codecogs.com/png.latex?f%28x%7Ck%29%20%3D%20%5Cfrac%7Bsin%28x%29%5E%7Bk%7D%7D%7B%5Cint_0%5E%5Cpi%5C%21sin%28x%29%5E%7Bk%7D%20dx%7D%20
"f(x|k) = \\frac{sin(x)^{k}}{\\int_0^\\pi\\!sin(x)^{k} dx} ")  

We can substitute this value into our previous equation for
![C\_{k}](https://latex.codecogs.com/png.latex?C_%7Bk%7D "C_{k}") to
get:

  
![C\_{k} = \\frac{\\frac{sin(x)^{k}}{\\int\_0^\\pi\\\!sin(x)^{k}
dx}}{sin(x)^{k}}
](https://latex.codecogs.com/png.latex?C_%7Bk%7D%20%3D%20%5Cfrac%7B%5Cfrac%7Bsin%28x%29%5E%7Bk%7D%7D%7B%5Cint_0%5E%5Cpi%5C%21sin%28x%29%5E%7Bk%7D%20dx%7D%7D%7Bsin%28x%29%5E%7Bk%7D%7D%20
"C_{k} = \\frac{\\frac{sin(x)^{k}}{\\int_0^\\pi\\!sin(x)^{k} dx}}{sin(x)^{k}} ")  
  
![C\_{k} = \\frac{1}{\\int\_0^\\pi\\\!sin(x)^{k} dx}
](https://latex.codecogs.com/png.latex?C_%7Bk%7D%20%3D%20%5Cfrac%7B1%7D%7B%5Cint_0%5E%5Cpi%5C%21sin%28x%29%5E%7Bk%7D%20dx%7D%20
"C_{k} = \\frac{1}{\\int_0^\\pi\\!sin(x)^{k} dx} ")  

\#\#\#Ex 1.1 The model is parametric? Which are the parameters of the
model?

Yes, the model is parametric. It has one parameter which is
![k](https://latex.codecogs.com/png.latex?k
"k").

### Ex 1.2 Write the minus log-likelihood function of the model and implement it in an R function.

The minus log likelihood function is:   
![-\\ell(k) =
-\\sum\_{i=1}^nlog\\frac{sin(X\_{i})^{k}}{\\int\_0^\\pi\\\!sin(x)^{k}
dx}](https://latex.codecogs.com/png.latex?-%5Cell%28k%29%20%3D%20-%5Csum_%7Bi%3D1%7D%5Enlog%5Cfrac%7Bsin%28X_%7Bi%7D%29%5E%7Bk%7D%7D%7B%5Cint_0%5E%5Cpi%5C%21sin%28x%29%5E%7Bk%7D%20dx%7D
"-\\ell(k) = -\\sum_{i=1}^nlog\\frac{sin(X_{i})^{k}}{\\int_0^\\pi\\!sin(x)^{k} dx}")  

The R implementation of this minus log likelihood function:

``` r
dSinK <- function(x, k = 1, lg = FALSE) {
  #returns a vector of sinx^k or log(sin^k)
  sinintegral <- integrate(function(x) sin(x) ^ k, lower = 0, upper = pi)$value
  if (lg == FALSE) {
    return(sin(x) ^ k / sinintegral)
  }
  else{
    return(log(sin(x) ^ k / sinintegral))
  }
}

dSinKMLL <- function(k, xvals) {
  return(-sum(dSinK(xvals, k, lg=TRUE)))
}
```

\#\#\#Ex 1.3 Use numerical optimization method to find the maximum
likelihood estimator.

``` r
optimize(f = dSinKMLL, xvals = angles, interval = c(0, 50))
```

    ## $minimum
    ## [1] 11.40039
    ## 
    ## $objective
    ## [1] 158.3675

\#\#\#Ex 1.4 Plot the histogram of the data and the density
corresponding to the MLE.

``` r
hist(
  angles,
  xlim = c(0.5, 2.5),
  ylim = c(0, 1.4),
  probability = TRUE
)
minK <- optimize(f = dSinKMLL, xvals = angles, interval = c(0, 50))$minimum
curve(dSinK(x, k = minK), add = TRUE, col = "blue")
```

![](assignment3_rmd_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

\#2 A case study of neuronal data \#\#\#Ex 2.1 If we assume the ISI
observations are i.i.d. following an exponential distribution with
parameter ![\\lambda](https://latex.codecogs.com/png.latex?%5Clambda
"\\lambda"). Compute the maximum likelihood estimate of
![\\lambda](https://latex.codecogs.com/png.latex?%5Clambda "\\lambda").

If we assume the ISI data follows an exponential distribution with
parameter ![\\lambda](https://latex.codecogs.com/png.latex?%5Clambda
"\\lambda"), then
![\\ell(\\lambda)](https://latex.codecogs.com/png.latex?%5Cell%28%5Clambda%29
"\\ell(\\lambda)") is given by the following equation:   
![\\ell(\\lambda) =
nlog(\\lambda)-\\lambda\\sum\_{i=1}^nX\_i](https://latex.codecogs.com/png.latex?%5Cell%28%5Clambda%29%20%3D%20nlog%28%5Clambda%29-%5Clambda%5Csum_%7Bi%3D1%7D%5EnX_i
"\\ell(\\lambda) = nlog(\\lambda)-\\lambda\\sum_{i=1}^nX_i")  
The first derivative of
![\\ell(\\lambda)](https://latex.codecogs.com/png.latex?%5Cell%28%5Clambda%29
"\\ell(\\lambda)") is given by the following equation:   
![\\ell^\\prime(\\lambda) =
\\frac{n}{\\lambda}-\\sum\_{i=1}^{n}X\_i](https://latex.codecogs.com/png.latex?%5Cell%5E%5Cprime%28%5Clambda%29%20%3D%20%5Cfrac%7Bn%7D%7B%5Clambda%7D-%5Csum_%7Bi%3D1%7D%5E%7Bn%7DX_i
"\\ell^\\prime(\\lambda) = \\frac{n}{\\lambda}-\\sum_{i=1}^{n}X_i")  
We know that the maximum value of
![\\lambda](https://latex.codecogs.com/png.latex?%5Clambda "\\lambda")
will occur when ![\\ell^\\prime(\\lambda)
= 0](https://latex.codecogs.com/png.latex?%5Cell%5E%5Cprime%28%5Clambda%29%20%3D%200
"\\ell^\\prime(\\lambda) = 0"). Therefore:   
![\\frac{n}{\\lambda}-\\sum\_{i=1}^{n}X\_i
= 0](https://latex.codecogs.com/png.latex?%5Cfrac%7Bn%7D%7B%5Clambda%7D-%5Csum_%7Bi%3D1%7D%5E%7Bn%7DX_i%20%3D%200
"\\frac{n}{\\lambda}-\\sum_{i=1}^{n}X_i = 0")  
  
![\\frac{n}{\\lambda}=\\sum\_{i=1}^{n}X\_i](https://latex.codecogs.com/png.latex?%5Cfrac%7Bn%7D%7B%5Clambda%7D%3D%5Csum_%7Bi%3D1%7D%5E%7Bn%7DX_i
"\\frac{n}{\\lambda}=\\sum_{i=1}^{n}X_i")  
  
![\\lambda=\\frac{n}{\\sum\_{i=1}^{n}X\_i}](https://latex.codecogs.com/png.latex?%5Clambda%3D%5Cfrac%7Bn%7D%7B%5Csum_%7Bi%3D1%7D%5E%7Bn%7DX_i%7D
"\\lambda=\\frac{n}{\\sum_{i=1}^{n}X_i}")  
  
![\\lambda =
\\frac{1}{\\bar{X}}](https://latex.codecogs.com/png.latex?%5Clambda%20%3D%20%5Cfrac%7B1%7D%7B%5Cbar%7BX%7D%7D
"\\lambda = \\frac{1}{\\bar{X}}")  

``` r
ISImean <- mean(isidata)
lambda <- 1/ISImean
```

    ## [1] "Lambda = 1.14689142797908"

\#\#\#Ex 2.2 Assume now that the ISI observations are i.i.d. following a
gamma distribution with parameters
![\\alpha](https://latex.codecogs.com/png.latex?%5Calpha "\\alpha")
(shape) and ![\\beta](https://latex.codecogs.com/png.latex?%5Cbeta
"\\beta") (rate), find the MLE estimates of the parameters
![\\alpha](https://latex.codecogs.com/png.latex?%5Calpha "\\alpha") and
![\\beta](https://latex.codecogs.com/png.latex?%5Cbeta "\\beta").

``` r
gammaMLH <- function(parameters, xvals){
  return(-sum(dgamma(xvals, shape = parameters[1], rate = parameters[2], log = TRUE)))
}

parametersGamma <- optim(par = c(1, 1), fn = gammaMLH, xvals = isidata)$par
```

    ## [1] "Alpha = 1.56231848686039"

    ## [1] "Beta = 1.79165490720006"

\#\#\#Ex 2.3 Try to find the method of moments estimator of
![\\alpha](https://latex.codecogs.com/png.latex?%5Calpha "\\alpha") and
![\\beta](https://latex.codecogs.com/png.latex?%5Cbeta "\\beta"). The
method of moments can be used to find the first estimation to initialize
the MLE iterative algorithm. We know that:   
![E(X)=\\frac{\\alpha}{\\beta}\\ \\text{and} \\
V(X)=\\frac{\\alpha}{\\beta^2}](https://latex.codecogs.com/png.latex?E%28X%29%3D%5Cfrac%7B%5Calpha%7D%7B%5Cbeta%7D%5C%20%5Ctext%7Band%7D%20%5C%20V%28X%29%3D%5Cfrac%7B%5Calpha%7D%7B%5Cbeta%5E2%7D
"E(X)=\\frac{\\alpha}{\\beta}\\ \\text{and} \\ V(X)=\\frac{\\alpha}{\\beta^2}")  
Therefore:
  
![V(X)=\\frac{\\alpha}{\\beta}\\times\\frac{1}{\\beta}](https://latex.codecogs.com/png.latex?V%28X%29%3D%5Cfrac%7B%5Calpha%7D%7B%5Cbeta%7D%5Ctimes%5Cfrac%7B1%7D%7B%5Cbeta%7D
"V(X)=\\frac{\\alpha}{\\beta}\\times\\frac{1}{\\beta}")  
  
![V(X)=E(X)\\times\\frac{1}{\\beta}](https://latex.codecogs.com/png.latex?V%28X%29%3DE%28X%29%5Ctimes%5Cfrac%7B1%7D%7B%5Cbeta%7D
"V(X)=E(X)\\times\\frac{1}{\\beta}")  
  
![\\beta=E(X)/V(X)](https://latex.codecogs.com/png.latex?%5Cbeta%3DE%28X%29%2FV%28X%29
"\\beta=E(X)/V(X)")  
We can caclulate the value of
![\\alpha](https://latex.codecogs.com/png.latex?%5Calpha "\\alpha") by
subsituting this value of
![\\beta](https://latex.codecogs.com/png.latex?%5Cbeta "\\beta") into
the equation for ![E(X)](https://latex.codecogs.com/png.latex?E%28X%29
"E(X)"):   
![\\alpha=
E(X)\\times\\beta](https://latex.codecogs.com/png.latex?%5Calpha%3D%20E%28X%29%5Ctimes%5Cbeta
"\\alpha= E(X)\\times\\beta")  

``` r
beta_estimator <- mean(isidata)/var(isidata)
```

    ## [1] 1.472556

``` r
alpha_estimator <- mean(isidata)*beta_estimator
```

    ## [1] 1.283954

\#\#Ex 3 \#\#\#Ex 3.1 Write (analytically) the formula for the
log-likelihood given n i.i.d. observations.   
![f(x|\\mu,\\lambda)=\\frac{\\lambda}{2\\pi
x^3}exp(\\frac{-\\lambda(x-\\mu)^2}{2\\mu^2x})](https://latex.codecogs.com/png.latex?f%28x%7C%5Cmu%2C%5Clambda%29%3D%5Cfrac%7B%5Clambda%7D%7B2%5Cpi%20x%5E3%7Dexp%28%5Cfrac%7B-%5Clambda%28x-%5Cmu%29%5E2%7D%7B2%5Cmu%5E2x%7D%29
"f(x|\\mu,\\lambda)=\\frac{\\lambda}{2\\pi x^3}exp(\\frac{-\\lambda(x-\\mu)^2}{2\\mu^2x})")  
  
![\\mathcal{L}\_n(\\mu,\\lambda)=\\prod\_{i=1}^n(\\frac{\\lambda}{2\\pi
X\_i^3})^\\frac{1}{2}exp(\\frac{-\\lambda(x-\\mu)^2}{2\\mu^2x})](https://latex.codecogs.com/png.latex?%5Cmathcal%7BL%7D_n%28%5Cmu%2C%5Clambda%29%3D%5Cprod_%7Bi%3D1%7D%5En%28%5Cfrac%7B%5Clambda%7D%7B2%5Cpi%20X_i%5E3%7D%29%5E%5Cfrac%7B1%7D%7B2%7Dexp%28%5Cfrac%7B-%5Clambda%28x-%5Cmu%29%5E2%7D%7B2%5Cmu%5E2x%7D%29
"\\mathcal{L}_n(\\mu,\\lambda)=\\prod_{i=1}^n(\\frac{\\lambda}{2\\pi X_i^3})^\\frac{1}{2}exp(\\frac{-\\lambda(x-\\mu)^2}{2\\mu^2x})")  
  
![\\ell\_n(\\mu,\\lambda)=\\sum\_{i=1}^nln(\\frac{\\lambda}{2\\pi
X\_i^3})^\\frac{1}{2}+\\sum\_{i=1}^nln(exp(\\frac{-\\lambda(x-\\mu)^2}{2\\mu^2x}))](https://latex.codecogs.com/png.latex?%5Cell_n%28%5Cmu%2C%5Clambda%29%3D%5Csum_%7Bi%3D1%7D%5Enln%28%5Cfrac%7B%5Clambda%7D%7B2%5Cpi%20X_i%5E3%7D%29%5E%5Cfrac%7B1%7D%7B2%7D%2B%5Csum_%7Bi%3D1%7D%5Enln%28exp%28%5Cfrac%7B-%5Clambda%28x-%5Cmu%29%5E2%7D%7B2%5Cmu%5E2x%7D%29%29
"\\ell_n(\\mu,\\lambda)=\\sum_{i=1}^nln(\\frac{\\lambda}{2\\pi X_i^3})^\\frac{1}{2}+\\sum_{i=1}^nln(exp(\\frac{-\\lambda(x-\\mu)^2}{2\\mu^2x}))")  
  
![\\ell\_n(\\mu,\\lambda)=\\sum\_{i=1}^nln\\lambda^\\frac{1}{2}-\\sum\_{i=1}^nln2\\pi
X\_i^3+\\sum\_{i=1}^n\\frac{-\\lambda(x-\\mu)^2}{2\\mu^2x}](https://latex.codecogs.com/png.latex?%5Cell_n%28%5Cmu%2C%5Clambda%29%3D%5Csum_%7Bi%3D1%7D%5Enln%5Clambda%5E%5Cfrac%7B1%7D%7B2%7D-%5Csum_%7Bi%3D1%7D%5Enln2%5Cpi%20X_i%5E3%2B%5Csum_%7Bi%3D1%7D%5En%5Cfrac%7B-%5Clambda%28x-%5Cmu%29%5E2%7D%7B2%5Cmu%5E2x%7D
"\\ell_n(\\mu,\\lambda)=\\sum_{i=1}^nln\\lambda^\\frac{1}{2}-\\sum_{i=1}^nln2\\pi X_i^3+\\sum_{i=1}^n\\frac{-\\lambda(x-\\mu)^2}{2\\mu^2x}")  
  
![\\ell\_n(\\mu,\\lambda)=\\frac{n}{2}ln\\lambda-\\sum\_{i=1}^nln2\\pi
X\_i^3+\\frac{\\lambda}{2}\\sum\_{i=1}^n\\frac{(X\_i-\\mu)^2}{\\mu^2X\_i}](https://latex.codecogs.com/png.latex?%5Cell_n%28%5Cmu%2C%5Clambda%29%3D%5Cfrac%7Bn%7D%7B2%7Dln%5Clambda-%5Csum_%7Bi%3D1%7D%5Enln2%5Cpi%20X_i%5E3%2B%5Cfrac%7B%5Clambda%7D%7B2%7D%5Csum_%7Bi%3D1%7D%5En%5Cfrac%7B%28X_i-%5Cmu%29%5E2%7D%7B%5Cmu%5E2X_i%7D
"\\ell_n(\\mu,\\lambda)=\\frac{n}{2}ln\\lambda-\\sum_{i=1}^nln2\\pi X_i^3+\\frac{\\lambda}{2}\\sum_{i=1}^n\\frac{(X_i-\\mu)^2}{\\mu^2X_i}")  

\#\#\#Ex 3.2 Try to derive the formula for the maximum likelihood
estimators for ![\\mu](https://latex.codecogs.com/png.latex?%5Cmu
"\\mu") and ![\\lambda](https://latex.codecogs.com/png.latex?%5Clambda
"\\lambda")

\#\#\#Ex 3.3 Apply the MLE estimators in the previous step to the
experimental ISI data, that is calculate the theoretical estimates of
![\\mu](https://latex.codecogs.com/png.latex?%5Cmu "\\mu") and
![\\lambda](https://latex.codecogs.com/png.latex?%5Clambda "\\lambda")
for the ISI data.

\#\#\#Ex3.4 Find the maximum likelihood estimators using numerical
methods.

``` r
dInvNorm <- function(x, mu, lambda, lg = FALSE){
  if(lg == TRUE){
    return(log(sqrt(lambda/(2*pi*(x^3)))*exp(-lambda*((x-mu)^2)/(2*(mu^2)*x))))
  }
  else{
    return(sqrt(lambda/(2*pi*(x^3)))*exp(-lambda*((x-mu)^2)/(2*(mu^2)*x)))
  }
}

invNormMLH <- function(parameters, xvals){
  return(-sum(dInvNorm(xvals, mu=parameters[1], lambda = parameters[2], lg = TRUE)))
}

parametersinvNorm <- optim(par = c(1, 1), fn = invNormMLH, xvals = isidata)$par
```

    ## [1] "Mu =  0.871873219725603"

    ## [1] "Lambda = 0.867973616707968"

\#Ex 3.5 Plot the estimated inverse Gaussian density on top of the
histogram of the ISI data and with the kernel density estimation.

``` r
hist(
  isidata,
  xlim = c(0, 6),
  ylim = c(0, 1.2),
  probability = TRUE
)
curve(dInvNorm(x, parametersinvNorm[1], parametersinvNorm[2]), col = "blue", add = TRUE)
```

![](assignment3_rmd_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

\#3 Brain cell dataset \#\#\#Ex 4.1 Find numerically the MLE estimates
of the parameters of the log-normal distribution for the ramp spike time
observations.

``` r
dlnormMLL <- function(parameters, xvals){
  return(-sum(dlnorm(xvals, meanlog = parameters[1], sdlog = parameters[2], log = TRUE)))
}

parametersDlnorm <- optim(par = c(1, 1), fn = dlnormMLL, xvals = rampSpike)$par
```

    ## [1] "Meanlog =  1.66883532639348"

    ## [1] "SDlog =  0.605661842011454"

\#\#\#Ex 4.2 Transform the ramp spike time observations using the
logarithm and then obtain the MLE of the parameters for a Gaussian
distribution using the transformed data. Check that the results you
obtain are equal to the MLE estimates obtained numerically in point 4.1.

``` r
logRampSpike <- log(rampSpike)

dnormMLL <- function(parameters, xvals){
  return(-sum(dnorm(xvals, mean = parameters[1], sd = parameters[2], log = TRUE)))
}

parametersDnorm <- optim(par = c(1, 1), fn = dnormMLL, xvals = logRampSpike)$par
```

    ## [1] "Mean =  1.66895647611618"

    ## [1] "SD = 0.605623564290239"

\#\#\#Ex 4.3 Find now the MLE estimates for the parameters of the
log-normal distribution using only the male human observations and the
female human observations. Plot the two obtained log-normal densities in
the same plot.

``` r
parametersDlnormMale <- optim(par = c(1, 1), fn = dlnormMLL, 
                              xvals = maleRampSpike)$par
parametersDlnormFemale <- optim(par = c(1, 1), fn = dlnormMLL, 
                                xvals = femaleRampSpike)$par

curve(dInvNorm(x, parametersDlnormMale[1], parametersDlnormMale[2]), 
      col = "blue", ylab = "Density")
curve(dInvNorm(x, parametersDlnormFemale[1], parametersDlnormFemale[2]), 
      col = "red", add = TRUE)
legend("topright",legend = c("Male", "Female"), col = c("blue", "red"), lty = c(1, 1))
```

![](assignment3_rmd_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

\#4 Molecular evolution, Jukes-Cantor model \#\#\#Ex 5.1

``` r
JKprob <- function(X, Y, alpha, t=1000){
  if(X==Y){
    return(0.25+0.75*exp(-4*alpha*t))
  }
  else{
    return(0.25-0.25*exp(-4*alpha*t))
  }
}

plot(
  1,  type = "n",  xlab = "time",  xlim = c(0, 10),  ylim = c(0, 1),
  ylab = "Mutation probability",
  main = "Behavior of Jukes-Cantor model"
)
for (i in-1:1) {
  curve(
    JKprob(X = "A", Y = "A", alpha = 10 ^ i, t=x),
    from = 0, to = 10, add = TRUE, col = i + 3, lty = 1
  )
  curve(
    JKprob(X = "A", Y = "C", alpha = 10 ^ i, t=x),
    from = 0, to = 10, add = TRUE, col = i + 3, lty = 2
  )
}
legend(
  "topright", col = rep(2:4, each=2), lty = rep(1:2, 3),
  legend = c(
    "alpha = 0.1, X=Y",
    "alpha = 0.1, X!=Y",
    "alpha = 1, X=Y",
    "alpha = 1, X!=Y",
    "alpha = 10, X=Y",
    "alpha = 10, X!=Y"
  )
)
```

![](assignment3_rmd_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

\#\#\#Ex 5.2 Write the log-likelihood function for the observations as a
function of ![\\alpha](https://latex.codecogs.com/png.latex?%5Calpha
"\\alpha"). Our observations are
![n](https://latex.codecogs.com/png.latex?n "n") pairs of i.i.d
nucleotides:   
![(X\_i,Y\_i),...,(X\_n,
Y\_n)](https://latex.codecogs.com/png.latex?%28X_i%2CY_i%29%2C...%2C%28X_n%2C%20Y_n%29
"(X_i,Y_i),...,(X_n, Y_n)")  
Therefore our likelihood function should be:   
![\\mathcal{L}\_n(\\alpha)=\\prod\_{i=1}^nP(X=
X\_i,Y=Y\_i)](https://latex.codecogs.com/png.latex?%5Cmathcal%7BL%7D_n%28%5Calpha%29%3D%5Cprod_%7Bi%3D1%7D%5EnP%28X%3D%20X_i%2CY%3DY_i%29
"\\mathcal{L}_n(\\alpha)=\\prod_{i=1}^nP(X= X_i,Y=Y_i)")  
We know that the joint probability of observing ![(X = x; Y =
y)](https://latex.codecogs.com/png.latex?%28X%20%3D%20x%3B%20Y%20%3D%20y%29
"(X = x; Y = y)") is:
  
![P(X=x,Y=y)=P(Y=y|X=x)P(X=x)](https://latex.codecogs.com/png.latex?P%28X%3Dx%2CY%3Dy%29%3DP%28Y%3Dy%7CX%3Dx%29P%28X%3Dx%29
"P(X=x,Y=y)=P(Y=y|X=x)P(X=x)")  
And we assume all nucleotides have equal marginal probabilities, such
that:   
![P(X = x) = 0.25\\quad x\\in
\\{A,T,C,G\\}](https://latex.codecogs.com/png.latex?P%28X%20%3D%20x%29%20%3D%200.25%5Cquad%20x%5Cin%20%5C%7BA%2CT%2CC%2CG%5C%7D
"P(X = x) = 0.25\\quad x\\in \\{A,T,C,G\\}")  
Therefore our likelihood function becomes:
  
![\\mathcal{L}\_n(\\alpha)=\\prod\_{i=1}^nP(Y=Y\_i|X=X\_i)P(X=x)](https://latex.codecogs.com/png.latex?%5Cmathcal%7BL%7D_n%28%5Calpha%29%3D%5Cprod_%7Bi%3D1%7D%5EnP%28Y%3DY_i%7CX%3DX_i%29P%28X%3Dx%29
"\\mathcal{L}_n(\\alpha)=\\prod_{i=1}^nP(Y=Y_i|X=X_i)P(X=x)")  
  
![\\mathcal{L}\_n(\\alpha)=\\prod\_{i=1}^n0.25P(Y=Y\_i|X=X\_i)](https://latex.codecogs.com/png.latex?%5Cmathcal%7BL%7D_n%28%5Calpha%29%3D%5Cprod_%7Bi%3D1%7D%5En0.25P%28Y%3DY_i%7CX%3DX_i%29
"\\mathcal{L}_n(\\alpha)=\\prod_{i=1}^n0.25P(Y=Y_i|X=X_i)")  
The Jukes-Cantor model tells us:   
![P(Y=y|X=x)=\\begin{cases}
0.25 + 0.75 exp(-4\\alpha t),\\quad \\text{if } x = y \\\\
0.25 - 0.25 exp(-4\\alpha t),\\quad \\text{if } x \\neq y
\\end{cases}](https://latex.codecogs.com/png.latex?P%28Y%3Dy%7CX%3Dx%29%3D%5Cbegin%7Bcases%7D%0A%20%200.25%20%2B%200.75%20exp%28-4%5Calpha%20t%29%2C%5Cquad%20%5Ctext%7Bif%20%7D%20x%20%3D%20y%20%5C%5C%0A%20%200.25%20-%200.25%20exp%28-4%5Calpha%20t%29%2C%5Cquad%20%5Ctext%7Bif%20%7D%20x%20%5Cneq%20y%0A%5Cend%7Bcases%7D
"P(Y=y|X=x)=\\begin{cases}
  0.25 + 0.75 exp(-4\\alpha t),\\quad \\text{if } x = y \\\\
  0.25 - 0.25 exp(-4\\alpha t),\\quad \\text{if } x \\neq y
\\end{cases}")  
Since the Jukes-Cantor model only depends on whether ![x =
y](https://latex.codecogs.com/png.latex?x%20%3D%20y "x = y") or not, let
![n\_1](https://latex.codecogs.com/png.latex?n_1 "n_1") be the number of
times ![X\_i =
Y\_i](https://latex.codecogs.com/png.latex?X_i%20%3D%20Y_i "X_i = Y_i")
in our data and ![n\_2](https://latex.codecogs.com/png.latex?n_2 "n_2")
be the number of times ![X\_i \\neq
Y\_i](https://latex.codecogs.com/png.latex?X_i%20%5Cneq%20Y_i
"X_i \\neq Y_i") in our data. Therefore our likelihood function becomes:
  
![\\mathcal{L}\_n(\\alpha)=(0.25(0.25+0.75exp(-4\\alpha
t)))^{n\_1}(0.25(0.25-0.25exp(-4\\alpha
t)))^{n\_2}](https://latex.codecogs.com/png.latex?%5Cmathcal%7BL%7D_n%28%5Calpha%29%3D%280.25%280.25%2B0.75exp%28-4%5Calpha%20t%29%29%29%5E%7Bn_1%7D%280.25%280.25-0.25exp%28-4%5Calpha%20t%29%29%29%5E%7Bn_2%7D
"\\mathcal{L}_n(\\alpha)=(0.25(0.25+0.75exp(-4\\alpha t)))^{n_1}(0.25(0.25-0.25exp(-4\\alpha t)))^{n_2}")  
  
![\\ell\_n(\\alpha)=log((0.25(0.25+0.75exp(-4\\alpha
t)))^{n\_1}(0.25(0.25-0.25exp(-4\\alpha
t)))^{n\_2})](https://latex.codecogs.com/png.latex?%5Cell_n%28%5Calpha%29%3Dlog%28%280.25%280.25%2B0.75exp%28-4%5Calpha%20t%29%29%29%5E%7Bn_1%7D%280.25%280.25-0.25exp%28-4%5Calpha%20t%29%29%29%5E%7Bn_2%7D%29
"\\ell_n(\\alpha)=log((0.25(0.25+0.75exp(-4\\alpha t)))^{n_1}(0.25(0.25-0.25exp(-4\\alpha t)))^{n_2})")  
  
![\\ell\_n(\\alpha)=log(0.25(0.25+0.75exp(-4\\alpha
t)))^{n\_1}+log(0.25(0.25-0.25exp(-4\\alpha
t)))^{n\_2})](https://latex.codecogs.com/png.latex?%5Cell_n%28%5Calpha%29%3Dlog%280.25%280.25%2B0.75exp%28-4%5Calpha%20t%29%29%29%5E%7Bn_1%7D%2Blog%280.25%280.25-0.25exp%28-4%5Calpha%20t%29%29%29%5E%7Bn_2%7D%29
"\\ell_n(\\alpha)=log(0.25(0.25+0.75exp(-4\\alpha t)))^{n_1}+log(0.25(0.25-0.25exp(-4\\alpha t)))^{n_2})")  
  
![\\ell\_n(\\alpha)=n\_1log(0.25(0.25+0.75exp(-4\\alpha
t))+n\_2log(0.25(0.25-0.25exp(-4\\alpha
t))](https://latex.codecogs.com/png.latex?%5Cell_n%28%5Calpha%29%3Dn_1log%280.25%280.25%2B0.75exp%28-4%5Calpha%20t%29%29%2Bn_2log%280.25%280.25-0.25exp%28-4%5Calpha%20t%29%29
"\\ell_n(\\alpha)=n_1log(0.25(0.25+0.75exp(-4\\alpha t))+n_2log(0.25(0.25-0.25exp(-4\\alpha t))")  

\#\#\#Ex 5.3 Try to find the theoretical maximum likelihood estimator
for ![\\alpha](https://latex.codecogs.com/png.latex?%5Calpha "\\alpha").

\#\#\#Ex 5.4 Try to implement the Jukes-Cantor model in R:

``` r
#Implement probability function
distJK <- function(pair, alpha){
  A <- sapply(JK_pairs,function(x) x[1]) #select first element of each vector inside list
  B <- sapply(JK_pairs,function(x) x[2])#select second element (list of letters)
  n <- length(pair)
  n1 <- sum(A == B)
  n2 <- n - n1
  match <- n1*log(JKprob(X="X", Y="X", alpha=alpha)*0.25)
  mismatch <- n2*log(JKprob(X="X", Y="Y", alpha=alpha)*0.25)
  return(match+mismatch)
}

#Implement sampling procedure
simulate_JKpair <- function(start="A", alpha=1e-5, t = 1000){
  mutations <- rpois(1, lambda = 4*alpha*t)
  if(mutations == 0){
    end <- start
    return(c(start, end))
  } 
  else {
    for (i in 1:mutations){
      end <- sample(c("A", "C", "T", "G"), size = 1)
    }
  }
  return(c(start, end))
}

JK_pairs <- list()
for (i in 1:1000){
  JK_pairs[[i]] <- simulate_JKpair(t=1000) 
}

#Minus log likelihood function
distJKMLL <- function(alpha, xvals){
  return(-sum(distJK(xvals, alpha)))
}

#Solving numerically
optimize(f = distJKMLL, xvals = JK_pairs, interval = c(-100, 100))
```

    ## Warning in log(JKprob(X = "X", Y = "Y", alpha = alpha) * 0.25): NaNs produced

    ## Warning in optimize(f = distJKMLL, xvals = JK_pairs, interval = c(-100, : NA/Inf
    ## replaced by maximum positive value

    ## Warning in log(JKprob(X = "X", Y = "Y", alpha = alpha) * 0.25): NaNs produced

    ## Warning in optimize(f = distJKMLL, xvals = JK_pairs, interval = c(-100, : NA/Inf
    ## replaced by maximum positive value

    ## $minimum
    ## [1] 23.60665
    ## 
    ## $objective
    ## [1] 2772.589
