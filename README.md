# ggvenn

An [R](https://www.r-project.org) package that draws modifiable venn diagrams in [ggplot2](https://ggplot2.tidyverse.org/). The venn diagrams can easily be modified and added to an existing ggplot object. ggvenn uses base R functions to convert the data into a format that is than vizualized using ggplot2's geom_polygon layer.

# Installation
Install the package from the git repository:
``` r
devtools::install_github("nicolash2/ggvenn")
```

# Create a venn diagram
Load the package, create your venn diagrams. ggvenn can create venn diagrams with up to 5 sets. A theme_void normally goes will with venn diagrams.
``` r
library(ggvenn)
library(ggplot2)

#to showcase ggvenn, let's create a test set list. It is simply a list of vectors.
set1 <- rownames(subset(mtcars, mpg>18))
set2 <- rownames(subset(mtcars, qsec>18))
set3 <- rownames(subset(mtcars, cyl<5))
set4 <- rownames(subset(mtcars, wt>2.5))
set5 <- rownames(subset(mtcars, gear==4))
carset <- list(mpg=set1, qsec=set2, cyl=set3, wt=set4, gear=set5)

ggplot() + 
  geom_venn(carset) +
  theme_void()
```
<img src="readme_files/venn2-5.png"/>

You can change all the usual ggplot parameters and add additional layers. You can also change the position of the venn diagram with the xlim and ylim arguements.
Note, that by default, geom_venn will force the graph to be a square. If you don't want that, specify fixedCoords=FALSE.

Modifying the colors is done in the same way you would do normally:
``` r
ggplot() + geom_venn(setlist[1:3]) +
  scale_fill_manual(values=c("red","blue","yellow")) +
  theme_void()
```
<img src="readme_files/venn3_fill.png"/>

The geom_venn function will take all parameters valid for geom_polygon and geom_text and apply them accordingly, with 2 exceptions: the size and color arguements of the text will be set via the geom_venn arguements textsize and textcolor.
``` r
ggplot() + geom_venn(setlist[1:3], color="red", size=4, textsize=10, textcolor="blue") +
  theme_void()
```

<img src="readme_files/venn3_text.png"/>
