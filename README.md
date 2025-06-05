# README: Analysis of Factors Influencing International Mathematical Olympiad Rankings Using Regression Models in R

## Project Overview

This project investigates the factors influencing national performance in the **2021 International Mathematical Olympiad (IMO)** using regression analysis in R. The study aims to identify which country-level variables significantly impact IMO rankings and to build an optimal predictive model.

## Key Features

- **Data Sources**: 
  - IMO rankings from [imo-official.org](https://www.imo-official.org/)
  - Demographic and socio-economic data from [World Population Review](https://worldpopulationreview.com/) and [UNDP Human Development Reports](https://hdr.undp.org/)
  
- **Variables Analyzed**:
  - **Dependent Variable**: `ranking2021` (IMO 2021 ranking)
  - **Independent Variables**: Population (`pop2023`), growth rate (`growthRate`), land area (`landArea`), education years (`education`), quality of life (`qol`), crime rate (`crime`), average IQ (`iq`), and others.

- **Methodology**:
  - Initial linear regression model with 11 predictors.
  - Diagnostic checks for multicollinearity, normality, heteroscedasticity, and linearity.
  - Model refinement using transformations (log, Box-Cox) and variable selection via AIC.
  - Final model validation through cross-validation.

## Results

- **Final Model**:
  - Transformed response: `(ranking2021^λ - 1)/λ` where λ = 0.25 (Box-Cox).
  - Key predictors: `log(pop2023)`, `education`, `qol`, `qol^2`, `crime`, `growthRate`, and `iq`.
  - **R²**: 0.753 (75.3% variance explained).
  - **RMSE**: Improved from 41.93 (initial) to 16.99 (final).

- **Key Findings**:
  1. **Population size** (log-transformed) is the most significant predictor.
  2. **Education duration** positively correlates with better rankings.
  3. **Quality of life** (`qol`) has a U-shaped effect: initial increases lower rankings, but beyond a threshold, further improvements boost performance.
  4. **Crime rates** and **growth rates** negatively impact rankings.
  5. Higher **average IQ** correlates with better performance.

## Repository Structure

- **`Project_Report.pdf`**: Detailed analysis (in Polish) with methodology, diagnostics, and conclusions.
- **`Project_Script.R`**: R script for data cleaning, modeling, and visualization.
- **Data**: Original and cleaned datasets (sourced from public databases).

## Dependencies

- R packages: `ggplot2`, `dplyr`, `car`, `lmtest`, `MASS`, `leaps`, `nortest`.

## Author

**Inga Dylag**  
Faculty of Mathematics and Computer Science, Kraków (2023).

## License

Open-source for academic use. Data sourced from public databases.
