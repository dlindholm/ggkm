# ggkm

Kaplan-Meier survival curves are commonly used in the medical literature. R is quite capable of drawing such curves even in base plot, but in most cases you also want a table with the number of subjects at risk at different time points. 

The ggkm function:
* draws survival curves from a survfit object
* uses colors from the HCL color space (see http://hclwizard.org/hcl-color-scheme/)
* draws a number at risk table

This work builds upon work of Abhijit Dasgupta, Gil Tomas, Dieter Menne (https://github.com/dmenne/dmisc2). Some major changes include:
* drawing of cumulative incidence plots
* the handling of colors
* updated code to use dplyr instead of plyr
* changes the theme of the plot
* some additional customization options to the axes

