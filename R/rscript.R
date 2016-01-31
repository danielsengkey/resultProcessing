#' This R script is a companion to a sample project of OMNeT++
#' result processing example. To use this script, make sure you
#' have OMNeT++'s R package installed your system.
#' This script should located in a directory named R within the project
#' directory.
#' @author Daniel Febrian Sengkey <danielfebrian015@gmail.com>

# Loading OMNeT++'s R package
library(omnetpp)

#' Load the needed results
#' Variable normal represents the results from 'normal' TicToc
#' example, which used random gate to forward message
#' with no prevention of sending back message
#' through incoming gate.
#' As you can see, the dataset is loaded by using
#' loadDataset() function, available from OMNeT++'s R package.
#' Filter expression used in OMNeT++ result processing
#' interface also used here.
#' In this example, the result files are named after
#' the configuration and its run number. Wildcard
#' can be used to select multiple files.
#' The selection of the module comes from the simulation
#' design, and so the measurement/scalar name.
normal <- loadDataset(
  files = "../simulations/results/TicToc16-*.sca",
  add(
    type ='scalar',
    select = 'module(Tictoc16.tic[5]) AND name(hopCount:last)'
  )
)

#' We do the same for config "NoSendBack". Notice
#' the file name is differ.
#' NOTE: Using filtering on the fly as we do now
#' will require more disk reading activity since we
#' have to done it several times. Howevever (in my
#' personal experience), using filter while loading
#' a very huge simulation results (size in GBs) require
#' less memory.
noSendBack <- loadDataset(
  files = "../simulations/results/NoSendBack-*.sca",
  add(
    type ='scalar',
    select = 'module(Tictoc16.tic[5]) AND name(hopCount:last)'
  )
)

#' If you already view the contents of 'normal' and 'noSendBack'
#' then you should notice that they are quite long.
#' The measurements (in this case the hopCount) is available
#' in x$scalars, for x is the variable name of loaded dataset.
normal$scalars
noSendBack$scalars

#' The x$scalars$name contains the names of the scalars.
#' The x$scalars$module contains the module names.
#' You may notice there is only "hopCount:last"  and
#' TicToc[5].tic module available
#' since the other scalars and modules (if any) have been
#' filtered out when we loaded the datasets.
#' We can directly get the values by accessing respective column.
normal$scalars$value
noSendBack$scalars$value

#' We can assign the values to new variables.
hopCount.normal <- normal$scalars$value
hopCount.nsb <- noSendBack$scalars$value # Short for noSendBack

#' Please note that the dot '.' operator has no meaning in R,
#' while it means accessing class member in other language such as C++.
#' 
#' Now we can see the summary of each configuration results.
summary(hopCount.normal)
summary(hopCount.nsb)

#' Histograms
hist(
  hopCount.normal,
  col = "red"
  )

hist(
  hopCount.nsb,
  col = "blue"
)

#' By default, histogram shows the frequency of data.
#' We can change it to density by adding parameter
#' 'probability = TRUE'.
hist(
  hopCount.normal,
  col = "red",
  probability = TRUE
)

hist(
  hopCount.nsb,
  col = "blue",
  probability = T # R also understands this
)

#' We can have two histograms in a single plot.
#' To keep its readability, we change the alpha
#' (opacity) of the color using the 'rgb()' function.

hist(
  hopCount.normal,
  col = rgb(
    1, # RED
    0, # GREEN
    0, # BLUE
    0.2 #ALPHA
  )
)

hist(
  hopCount.nsb,
  col = rgb(
    0, # RED
    0, # GREEN
    1, # BLUE
    0.2 #ALPHA
  ),
  add = TRUE
)

#' Since the y-axises are different, therefore we
#' need to define it manually. Choose the number that
#' covers both frequencies.

hist(
  hopCount.normal,
  col = rgb(
    1, # RED
    0, # GREEN
    0, # BLUE
    0.2 #ALPHA
  ),
  ylim = c(0,50)
)

hist(
  hopCount.nsb,
  col = rgb(
    0, # RED
    0, # GREEN
    1, # BLUE
    0.2 #ALPHA
  ),
  add = TRUE
)

#' A more sophisticated plot with titles, etc.
hist(
  hopCount.normal,
  col = rgb(
    1, # RED
    0, # GREEN
    0, # BLUE
    0.2 #ALPHA
  ),
  ylim = c(0,50),
  main = "Comparison of Frequencies",
  xlab = "Hop Counts",
  labels = TRUE
)

hist(
  hopCount.nsb,
  col = rgb(
    0, # RED
    0, # GREEN
    1, # BLUE
    0.2 #ALPHA
  ),
  add = TRUE,
  labels = TRUE
)
#' Add legend
legend(
  x = "topright",
  legend = c(
    "Pure Random",
    "No Send Back"
    ),
  fill = c(
    rgb(
      1, # RED
      0, # GREEN
      0, # BLUE
      0.2 #ALPHA
    ),
    rgb(
      0, # RED
      0, # GREEN
      1, # BLUE
      0.2 #ALPHA
    )
  ),
  cex = 0.8
)

#' We can do similar things with densities of the data,
#' instead of frequencies. Note that the density is a
#' probability of the data. Therefore the maximum value
#' of density is 1.

hist(
  hopCount.normal,
  col = rgb(
    1, # RED
    0, # GREEN
    0, # BLUE
    0.2 #ALPHA
  ),
  ylim = c(0, 0.1),
  main = "Comparison of Densities",
  xlab = "Hop Counts",
  probability = TRUE
)

hist(
  hopCount.nsb,
  col = rgb(
    0, # RED
    0, # GREEN
    1, # BLUE
    0.2 #ALPHA
  ),
  add = TRUE,
  probability = TRUE
)

#' Add the density plot
lines(density(hopCount.normal), col="red", lwd = 3)
lines(density(hopCount.nsb), col="blue", lwd=3)

#' Draw lines showing position of means and medians.
#' First we need to set the values on the y-axis.
#' seq(a, b, c) will produce a vector from a to b
#' with c step.
#' rep(d, e) will repeat the value of d as much as n.
y <- seq(0, 0.1, 0.01)
mean.normal <- rep(mean(hopCount.normal), length(y))
mean.nsb <- rep(mean(hopCount.nsb), length(y))

lines(mean.normal, y, col="red", lty=2, lwd = 2)
lines(mean.nsb, y, col="blue", lty=2, lwd = 2)

median.normal <- rep(median(hopCount.normal), length(y))
median.nsb <- rep(median(hopCount.nsb), length(y))

lines(median.normal, y, col="red", lty=3, lwd = 2)
lines(median.nsb, y, col="blue", lty=3, lwd = 2)

#' Locations of data points
rug(hopCount.normal, col = "red")
rug(hopCount.nsb, col = "blue")

#' Altough it is possible to plot multiple plots
#' and provide as many graphical information in R,
#' the readability of the the plot should also be
#' concerned.

#' There are many aspects can be defined when plotting
#' histogram. Use '?hist' to see the help for the function.
#' More examples are available on the internet.

#' ECDF
#' Using stacked histograms, even it possible, will
#' lead to too many histograms when plotting more datasets.
#' Another approach is using the Empirical Cumulative
#' Distribution Function (ECDF) plot.

plot(
  ecdf(hopCount.normal),
  col = "red",
  main = NULL
  )

plot(
  ecdf(hopCount.nsb),
  col = "blue",
  add = T
)

legend(
  x = "bottomright",
  legend = c("Pure Random", "No Send Back"),
  pch = 19,
  col = c("red", "blue"),
  bty = "n"
)

#' Why using graphs?
#' By using them we can get some ideas or insights
#' about the data.

#' Applying Statistical Test
#' By now we can compare the data using statistical
#' test. We can see if the change in routing/forwarding
#' decision significantly different or not. Further, we
#' can check which one gives fewer hop count.

#' Normality test.
#' There are 2 groups of statistical tests: parametric
#' and non-parametric. Parametric methods assume the
#' data are normally distributed. On the other hand,
#' non-parametric methods do not require such thing.
#' Before applying any statistical test to our hop count
#' data, it is better to check their distribution pattern
#' first. We can check it using the Shapiro-Wilk test.

shapiro.test(hopCount.normal)
shapiro.test(hopCount.nsb)

#' There are two values for each test. The test statistics
#' W and the p-value. Assuming we have 95% confidence level,
#' then our alpha value is 0.05. The test's null hypothesis
#' is the data normally distributed. Referring to the literatures
#' we know that the null hypothesis can be rejected if p-value
#' less than the chosen alpha.
#' The test to both data show the p-values are less than 0.05.
#' Therfore we can reject the null hypothesis and safely assume
#' the data are not normally distributed.

#' Comparing 2 datasets
#' To compare between 2 datasets we can use t-test or the
#' Wilcoxon-Rank Sum test. Since t-test is a parametric
#' method, therefore we could not use it because our data
#' are not normally distributed. We will use Wilcoxon test here.

#' The null hypothesis of Wilcoxon Rank Sum test is both
#' data are equal. We can run the test like this:
wilcox.test(hopCount.normal, hopCount.nsb, paired = T)

#' By default it run two-tail test.
#' The very small p-value (3.242e-05) indicates
#' we can safely reject the null hypothesis.
#' To find out which method is better (by giving lower
#' hop count) we should run one-tail test.
#' To do so, we should set the alternate hypothesis
#' explicitly.
wilcox.test(
  hopCount.normal,
  hopCount.nsb,
  alternative = "greater",
  paired = T
  )

#' We used the alternate hypothesis that the first dataset
#' is greater than the second one. It means our argumentation
#' is: pure random forwarding can lead to more hop count.
#' You can check the inverse of the test by using "less", means
#' the alternate hypothesis the first dataset is less than
#' the second one.
wilcox.test(
  hopCount.normal,
  hopCount.nsb,
  alternative = "less",
  paired = T
)
#' Which gives p-value = 1.

#' Making decision.
#' From the statistical test of the data we can conclude
#' that applying selective gate for forwarding (by preventing
#' the message sent back through incoming gate) resulting in 
#' fewer hop count. Based on the computer network literatures,
#' then we can say the method is better.
