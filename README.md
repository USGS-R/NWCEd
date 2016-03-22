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



Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)


Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)
