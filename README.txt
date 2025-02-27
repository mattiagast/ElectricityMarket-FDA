###### FOLDER 
0.Presentation: Correct final presentations [Midterm and Final]
2022_DemandQuantity: .txt of Total Quantity BID processed 2022
2022_OfferQuantity: .txt of Total Quantity OFF processed 2022
2022_PrezzoZonale: .txt of PrezzoZonale processed 2022
2023_DemandQuantity: .txt of Total Quantity BID processed 2023
2023_OfferQuantity: .txt of Total Quantity OFF processed 2023
2023_PrezzoZonale: .txt PrezzoZonale processed 2023
Dataframes: Data frames of OFF, BID, PrezzoZonale in 2022 and 2023
Dataset: ++ PUT THE DATASET .xlm HERE ++
FunctionalDataObject: Sampled Curves of OFF and BID used for the Smoothing
Gas: Data frame of Gas (external variables)
RE: Dataframes of Renewable Energy Producted


####### R SCRIPT
0_readPrezzoZonale: random script of plot for the Midterm presentation
1.1_accessData: build the cumulative curves and step-funtions of OFF & BID
1.2_obtainPrezzoZonale: iterative method to extract PrezzoZonale from the XML files
1.2_obtainQuantity: iterative method to extract cumulative quantity of OFF & BID the XML files
1.3_df_PrezzoZonale: Obtain the Datasets from the various .txt files
2_exploration: explain the relationtship between PrezzoZonale, Total quantity OFF & BID
3.1_smoothingChioce: different tentatives of smoothing in order to achive the better one
3.1_smoothing: Using B-Spline method, study the parameters and way to smooth the curves
3.2_fPCA: fPCA for OFF and BID
3.3_KmeansAligment: fKMA of the curves
4.1_processingOilPrice: obtain from the data frames the External Variables
4.2_Correlation: Computing the Covariance Kernel between curves and external variables
5.1_FAR: FAR(1) used in the presentation
5.2_FAR: FAR(1) with assuming where we dont have observation copy the previous and considering just the latest 30 days.
5.3_Extention: Study of the delayed covariance kernel to obtain the order of the FAR model




