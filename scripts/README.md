## Scripts Description

- **0_readPrezzoZonale**  
  Exploratory script used to generate random plots for the midterm presentation.

- **1.1_accessData**  
  Builds the cumulative curves and step functions for **OFF (supply)** and **BID (demand)** quantities.

- **1.2_obtainPrezzoZonale**  
  Iterative procedure to extract the **Prezzo Zonale (zonal electricity price)** from the XML files.

- **1.2_obtainQuantity**  
  Iterative procedure to extract the **cumulative quantities of OFF and BID** from the XML files.

- **1.3_df_PrezzoZonale**  
  Creates the final **datasets** from the various `.txt` files.

- **2_exploration**  
  Exploratory analysis of the relationship between **Prezzo Zonale**, **total OFF quantity**, and **total BID quantity**.

- **3.1_smoothingChoice**  
  Comparison of different smoothing approaches to identify the most appropriate one for the curves.

- **3.1_smoothing**  
  Implementation of **B-spline smoothing**, including the study and tuning of smoothing parameters.

- **3.2_fPCA**  
  Functional Principal Component Analysis (**fPCA**) applied to **OFF and BID curves**.

- **3.3_KmeansAlignment**  
  Functional k-means clustering (**fKMA**) applied to the curves.

- **4.1_processingOilPrice**  
  Extraction and preprocessing of **external variables** from the data frames.

- **4.2_Correlation**  
  Computation of the **covariance kernel** between the curves and the external variables.

- **5.1_FAR**  
  Implementation of a **Functional Autoregressive Model FAR(1)** used in the presentation.

- **5.2_FAR**  
  Alternative **FAR(1)** approach where missing observations are replaced with the previous available curve and the model is fitted using only the **last 30 days of data**.

- **5.3_Extension**  
  Study of the **delayed covariance kernel** to determine the appropriate order of the FAR model.
