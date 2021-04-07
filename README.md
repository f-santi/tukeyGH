
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Tukey’s g-and-h probability distribution

## Presentation

The package provides distribution, density and quantile functions of the
Tukey’s g-and-h probability distribution (Tukey 1977), as well as
functions for random number generation, parameter estimation and
testing.

In the current version of the package, the g-and-h distribution can be
fitted through:

  - indirect inference (Bee, Hambuckers, and Trapin 2019b)
  - the quantile estimator (Hoaglin 1985)
  - maximum likelihood (Bee, Hambuckers, and Trapin 2019a)

whereas the hypothesis \(h=0\) (which makes the g-and-h distribution, a
g distribution) is tested by means of the log-likelihood ratio test
procedure proposed in Bee et al. (2021).

## An example

Fit the g-and-h distribution to dataset `EPWS2014` on operational losses
by means of indirect inference (Bee, Hambuckers, and Trapin 2019b), and
quantile estimator (Hoaglin 1985):

``` r
library(tukeyGH)
data("EPWS2014")

modII <- fitGH(EPWS2014, method = "iinference")
summary(modII)

modQu <- fitGH(EPWS2014, method = "quantile")
summary(modQu)
```

## References

<div id="refs" class="references">

<div id="ref-bee2021b">

Bee, M., J. Hambuckers, F. Santi, and L. Trapin. 2021. “Testing a
Parameter Restriction on the Boundary for the G-and-H Distribution: A
Simulated Approach.” *Computational Statistics*.
<https://doi.org/10.1007/s00180-021-01078-3>.

</div>

<div id="ref-bee2019b">

Bee, M., J. Hambuckers, and L. Trapin. 2019a. “An Improved Approach for
Estimating Large Losses in Insurance Analytics and Operational Risk
Using the G-and-H Distribution.” DEM Working papers 2019/11. Department
of Economics; Management, University of Trento.
<https://bit.ly/3cbccu2>.

</div>

<div id="ref-bee2019a">

———. 2019b. “Estimating Value-at-Risk for the G-and-H Distribution: An
Indirect Inference Approach.” *Quantitative Finance* 19 (8): 1255–66.
<https://doi.org/10.1080/14697688.2019.1580762>.

</div>

<div id="ref-hoaglin1985">

Hoaglin, D. C. 1985. “Exploring Data Tables, Trends, and Shapes.” In,
edited by D. C. Hoaglin, F. Mosteller, and J. W. Tukey, 461–513. Wiley.

</div>

<div id="ref-tukey1977">

Tukey, J. W. 1977. “Modern Techniques in Data Analysis.”

</div>

</div>
