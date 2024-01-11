# GeneExpressR Shiny App

## Overview
This Shiny app, titled "Gene Expression Calculator", is designed for analyzing gene expression data using a user-friendly web interface. It allows users to upload Excel files (.xlsx) containing gene expression data, view the data in various formats, perform calculations, and download the results.

## Features
- **Data Upload**: Users can upload Excel files and specify the number of rows to display.
- **Delta Ct Calculation**: The app calculates Delta Ct values from the provided data.
- **Gene-Specific Analysis**: Users can perform calculations specific to two genes (Gene_1 and Gene_2), including average expression calculation and relative expression analysis.
- **Downloadable Results**: The app provides functionality to download the calculated results as Excel files.

## Dependencies
To run this app, you need to have the following R libraries installed:
- `shiny`
- `shinyjs`
- `shinythemes`
- `readxl`
- `dplyr`
- `DT`
- `openxlsx`

## App Structure
The app's UI is constructed using `fluidPage` with a `navbarPage` layout. It contains three main sections:
1. **Data Upload**: For uploading and displaying the data.
2. **Delta Ct**: To view the Delta Ct calculations.
3. **Gene-Specific Analysis**: Separate tabs for Gene_1 and Gene_2 for detailed analysis.

The server function handles the backend logic, including file reading, data processing, and rendering tables.

## Usage
1. **Start the App**: Run the app in R.
2. **Upload Data**: Use the "Choose Excel File" button to upload your data.
3. **View and Analyze Data**: Navigate through the tabs to view data and perform analyses.
4. **Download Results**: Use the download buttons in the Gene-specific tabs to save your results.

## Note
This app is designed for specific gene expression analysis tasks. Ensure your Excel file format is compatible with the app's expected input structure for proper functionality.

## Contributions
Feel free to contribute to this project by suggesting improvements or reporting issues.

## License
This project is licensed under the MIT License - see the LICENSE file for details.
