# Analysis of Order Dynamics in the Electricity Spot Market

This repository contains a statistical and inferential analysis of the Italian electricity market trends, focusing on Zonal Price data recorded in 2022 and 2023.

## Abstract
This project investigates the complex dynamics of the Italian electricity market (2023-2024) by shifting the analytical focus from scalar prices to the functional structure of supply and demand curves. Utilizing a robust framework of Functional Data Analysis (FDA)—including Smoothing, fPCA, and fK-Means clustering—the study identifies significant temporal and structural patterns, such as the distinct day-of-the-week clustering in demand bids. The research further explores the correlation between energy curves and external drivers like gas prices and renewable production, culminating in a predictive approach using Functional Autoregressive (FAR) models. The findings reveal how supply bid curves maintain high correlation and sensitivity to external variables, while demand curves exhibit more complex, volatile behaviors.
The project has been presented on the 12th of July 2024 in an open workshop @ Politecnico di Milano.

## Preview
![Electricity Market Analysis Poster](https://raw.githubusercontent.com/mattiagast/ElectricityMarket-FDA/main/docs/Poster.jpg)

## WARNING
> **IMPORTANT:** The folders `DatasetXML` and `DatasetCSV` are not present in this repository due to privacy policy.

## Repository Structure
* **docs/**: Full documentation (Poster.pdf) of results, methodology, and statistical conclusions.
* **scripts/**: R source code used for data processing, curve reconstruction, and functional time series modeling.

## License and Usage Policy
All files in this repository, including the report text and the images in the `results` folder, are subject to the following terms:

1. **Academic & Personal Use**: You are free to use, copy, and distribute these files for non-commercial purposes, provided that appropriate credit is given to the original author.
2. **Commercial Use**: Reproduction or use of the contents for profit is strictly prohibited without explicit written consent.
3. **Disclaimer**: The data and analyses are provided "as is." The author assumes no responsibility for financial or operational decisions made based on the findings of this study.



