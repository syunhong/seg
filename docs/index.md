# R package 'seg'

The `seg` package is a collection of segregation measures developed in the geographical and sociological literature. Implemented measures range from simple, traditional methods like the index of dissimilarity (1955) to more sophisticated, spatial approaches such as Reardon and O'Sullivan's spatial segregation measures (2004). 

If you think I have missed any important methods in the field, or if you have any suggestions or new ideas to make existing features better, please do not hesitate to email me at hong.seongyun [at] gmail.com. Reported bugs will be fixed as soon as possible. All other suggestions will be considered when preparing for the next update. Please note however that I work on this package mainly in my spare time, so updates may not be as frequent as I would like. My aim is to have at least two updates every year (one in May and the other in November).

This page demonstrates the use of dissim(), isp() and spseg() for computing Duncan & Duncan's D, Morrill's D(adj), Wong's D(w) and D(s), White's SP, and Reardon and O'Sullivan's spatial segregation measures. The latest version of seg is available to download from CRAN: http://cran.r-project.org/web/packages/seg/

## Hypothetical patterns of segregation

The seg package contains a sample data set of eight different distributions of the population for testing purpose. The data set itself is a simple data frame but it can be displayed on a 10-by-10 grid to reproduce the hypothetical segregation patterns used by Morrill (1991) and Wong (1993).

```r
library(seg)
# Load the sample data set into the current workspace
data(segdata)

# Create a 10-by-10 grid to display the data set
grd <- GridTopology(cellcentre.offset=c(0.5,0.5),
                    cellsize=c(1,1), cells.dim=c(10,10))
grd.sp <- as.SpatialPolygons.GridTopology(grd)

# Display the eight different patterns on one page
par(mfrow = c(2, 4), mar = c(0, 1, 0, 1))
for (i in 1:8) {
  idx <- 2 * i
  full <- segdata[,(idx-1)] == 100
  half <- segdata[,(idx-1)] == 50
  plot(grd.sp)
  plot(grd.sp[full,], col = "Black", add = TRUE)
  if (any(half))
    plot(grd.sp[half,], col = "Grey", add = TRUE)
  text(5, 11.5, labels = paste("segdata[,", idx-1, ":", idx, "]", sep = ""))
}
```

## Index of dissimilarity, D, and its spatial counterparts

- Bulleted
- List

1. Numbered
2. List

| `seg` D       | `seg` D(adj)  | D(w)          | D(s)          | Content Cell  | Content Cell  | 
| ------------- | ------------- | ------------- | ------------- | ------------- | ------------- |
| Content Cell  | Content Cell  | Content Cell  | Content Cell  | Content Cell  | Content Cell  | 
| Content Cell  | Content Cell  | Content Cell  | Content Cell  | Content Cell  | Content Cell  | 

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [Basic writing and formatting syntax](https://docs.github.com/en/github/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax).

# Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/syunhong/seg/settings/pages). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

# Support or Contact

Having trouble with Pages? Check out our [documentation](https://docs.github.com/categories/github-pages-basics/) or [contact support](https://support.github.com/contact) and we’ll help you sort it out.
