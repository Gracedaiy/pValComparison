# pValComparison
Shiny app comparing p-values using binomial distribution and normal approximation

In real world, when the sample size is larger enough, people start to forget that we develop proportional test from binomial distribution. The blind spot -- the large sample size
could solve everything -- will somethimes cause problems. 

One example is that if we are trying to test if $p = 0.0001$, which is a very small number, and let's say we have 100 as our sample size. Even though it is a large sample size, but
because of the samll proprotion that we want to test, $n \times p = 0.01 < 5$, then, we should use binomial distribution as reference distribution to calculate the p-value instead
of using normal approximation.

It is implemented on Shiny app, check the [link](https://yingdai1130.shinyapps.io/pValComp/) out.
