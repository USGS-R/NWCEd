# NWCEd

<br>

## Purpose and Content Overview

The purpose of the NWCEd package is to introduce users to the tools and functionalities of the [National Water Census Data Portal](http://cida.usgs.gov/nwc/) (NWC-DP).  The materials found in the package include custom functions, which allow the user to download hydrologic datasets and HUC information from the NWC-DP.  The materials also include a series of stand-alone labs which instruct users on how to download and analyze NWC-DP datasets numerically and graphically in R.  It is recommended that the labs be used as supplemental material in hydroscience courses at the college level.

<br>

## Getting Started

#### Step 1
Install and load devtools package in your environment.
```{r}
install.packages("devtools")
library(devtools)
```
#### Step 2
Install and load NWCEd package in your environment.
```{r}
install_github("dblodgett-usgs/NWCEd")
library(NWCEd)
```
#### Step 3
Install and load the following packages to run in the respective labs using the same process as *Step 1*.

| Lab # | Additional Required Packages |
| --- | --- |
| Lab 3 | <ul><li>"datasets"</li><li>"ggplot2"</li><li>"gridExtra"</li><li>"psych"</li></ul> |
| Lab 4 | <ul><li>"dplyr"</li><li>"ggplot2"</li><li>"leaflet"</li><li>"scales"</li><li>"stats"</li></ul> |
| Lab 5 | <ul><li>"foreach"</li><li>"ggplot2"</li><li>"iterators"</li></ul> |

<br>

[![Travis](https://travis-ci.org/dblodgett-usgs/NWCEd.svg?branch=master)](https://travis-ci.org/dblodgett-usgs/NWCEd)

<br>

## Learning Objectives
<div style="text-align:left">

Below is a table which lists the learning objectives for the the lab materials as a whole.  Individual labs may not meet all the learning objectives.  For material description, please see the Materials Description table below.

| Learning Objective | Objective Description |
| --- | --- |
| Access | Learn how to access hydrologic data and associated metatdata from the NWC-DP |
| Analyze | Learn how to analyze NWC-DP data using numerical and graphical methods in R |
| communicate | Practice how to communicate analysis through the use of graphs |

<br>

## Materials Description
<div style="text-align:left">
The description of each of the lab materials is provided in the table below as well as a link to each of the respective labs.

| Lab # | Material Description | Link |
| --- | --- | --- |
| Lab 1 | <ul><li>Accessing the NWC-DP</li><li>NWC-DP water Budget tool</li><li>Introduction to HUC's</li></ul> | [Lab 1](https://cdn.rawgit.com/NWCEd/NWCEd/master/inst/Lab_1.html) |
| Lab 2 | <ul><li>Accessing the NWC-DP</li><li>NWC-DP Streamflow Stats tool</li><li>Downloading stats results</li></ul> | [Lab 2](https://cdn.rawgit.com/NWCEd/NWCEd/master/inst/Lab_2.html) |
| Lab 3 | <ul><li>Accessing ET/ precipitation data from NWC-DP</li><li>Analyzing and plotting box plots, histograms, and density curve plots in R</li><li>Writing exercises</li></ul> | [Lab 3](https://cdn.rawgit.com/NWCEd/NWCEd/master/inst/Lab_3.html) |
| Lab 4 | <ul><li>Analyzing time series and bar plots in R using NWC-DP data</li><li>Water balances</li><li>Double-mass curve analysis</li></ul> | [Lab 4](https://cdn.rawgit.com/NWCEd/NWCEd/master/inst/Lab_4.html) |
| Lab 5 | <ul><li>Using a Log Pearson Type III model to analyze NWC-DP data in Excel</li><li>Using a Log Pearson Type III model to analyze NWC-DP data in R</li></ul> | [Lab 5](https://cdn.rawgit.com/jnelson7/NWCEd/master/inst/Lab_5.html) |

<br>

## Function Names and Descriptions
<div style="text-align:left">


| Function Name | Function Description |
| --- | --- |
| getNWCData() | Downloads hydrologic datasets associated with a user-specified HUC ID |
| getNWCWatershed() | Downloads HUC spacial data for user selected HUC ID |
| annualize() | Converts daily time series datasets to annual time series datasets |
| getNWISSite() | Verifies the information exists for user-selected NWIS gage  |
| getSWECSVBlock() | Accepts a url and returns a data frame of data for that SOS request |
| Lp3 | Accepts a variable containing a NWC-DP dataset, applies a Log-Pearson Type III distribution, and returns a plot |

<br>

## Additional Reading Links

[Double-Mass Curves](http://pubs.usgs.gov/wsp/1541b/report.pdf)<br>
[Statistical Methods in Water Resources](http://pubs.usgs.gov/twri/twri4a3/pdf/twri4a3-new.pdf)<br>
[Daymet Model](https://daymet.ornl.gov/)<br>
[CIDA USGS THREDDS Data Server](http://cida.usgs.gov/thredds/catalog.html?dataset=cida.usgs.gov/ssebopeta/monthly)<br>
[Log-Pearson Type III Distribution](http://ascelibrary.org/doi/pdf/10.1061/(ASCE)1084-0699(2007)12%3A5(482))

<br>

Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)


Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)
  




