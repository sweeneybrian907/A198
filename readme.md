# DWA-A 198

This repo includes a R shiny app and R functions for the analysis
of sewer treatment plants according to DWA-A 198. These fucntions
are by no means exhaustive and represent only a subset of the analysis
for a full evaluation of a STP.

## Getting setup

### Easy setup

Clone this reposistory to your system with

```
git clone https://github.com/sweeneybrian907/A198.git
```

Run in R-Studio making sure to download all package dependencies.

### points to address
1. Date format must be in order: ddmmyy or ddmmyyyy any delimiter is possble. It mustn't contain time information ie: 02.05.2018 14:00
2. Does not deal with UTF; if problems arise delete non utf8 charachters in the table no ü,ö,ä, °, $,€,@.... all special charachters