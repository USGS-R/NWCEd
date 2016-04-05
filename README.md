# NWCEd
Get up and running: 
```{r}
library(devtools)
install_github("USGS-R/NWCCompare")
dataOut<-getNWCData(dataSource="http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_daymet.nc",huc="031401020800")
summary(dataOut)
```

The following are URL addresses used to access water budget data:

1. http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_eta.nc
2. http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_daymet.nc
3. http://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_Q.nc


Daily Streamflow data is obtained from http://waterservices.usgs.gov/rest/DV-Service.html

# Package Vignette 

Problem statement, how its relevant, example water budget data--example watersheds showing different hydrologic settings.
Discuss learning objectives and how they relate to the problem statement (expectations of what the user is going to learn)

## Canned examples that illustrate the problem


Need function to plot raw time series of water budget data.
	Will require convert from monthly or daily to annual or longer.
	
#### Function1 (Annual Time Series Plot Function)
    *store a list of values in a variable
    **convert values to annual 
    ***check the heading of the column from which the list is being pulled
    *store a list of respective values in another variable
    **convert values to annual
    *function should plot the defined variables
    **predefined plot size and plot format

Link to NWC Portal for more.

Additional reading links.

# What are approaches to address/understand/solve the problem?

## Static examples that illustrate different ways to think about uncertainty in the data.

Need a way to combine raw water budget data into a 'closed' water budget.
Need a way to modify time series according to different types of uncertainty and generate plots.
	End up with a plot showing original data along with adjusted data according to assumed uncertainty.
	Also show new 'balanced' water budget based on assumed uncertainty.

#### Function2 (Closed Annual Water Budget Plotting Function)	
	*Function should be modified from function 1
	**Should convert datasets to annual, plotting size and formats should be the same
	*function should include user inputs for precip, ET, runoff
	**function should convert all datasets to annual
	**function should calculate the closed water budget by adding/subtracting datasets
	*function should allow user option of inputting in types of uncertainty
	**uncertainty inputs should be defaulted to none
	
## Objectives of the exercise

Determine what the substance of the lesson plan is and summarize. 
Show how a user could step through a solution.

# Custom Report and Exercise 

User creates their own report. 
	Will need a function to generate the actual markdown report akin to the Vignette.
	
#### Function3 (Markdown Report Function)

  *

## Base case - User's (or a specific chosen) Water Shed Without Changes.

## Come up with a proposed solution or to the base case.

## Compare how their solution compares to the original.

## Justify why their solution makes hydrologic sense.

Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)


Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)
