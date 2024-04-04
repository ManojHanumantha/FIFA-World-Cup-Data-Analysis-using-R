# FIFA World Cup Data Analysis

This repository contains the code and analysis for the FIFA World Cup Data Analysis project conducted as part of the MS5130 course assignment.

## Overview

The FIFA World Cup Data Analysis project aims to analyze and gain insights from FIFA World Cup match data using various data analysis techniques and machine learning models. The analysis covers aspects such as match statistics, goal distribution, player nationalities, player positions, and more.

## Project Structure

- **Data:** Contains the datasets used for analysis, including `matches.csv`, `team_appearances.csv`, `player_appearances.csv`, and `goals.csv`.
- **Code:** Contains R scripts and qmd files for data import, data cleaning, exploratory data analysis, quantitative analysis (modeling), and qualitative analysis (text mining).
- **Images:** Contains images used in the project, including the FIFA World Cup image.
- **Output:** Contains output files generated during the analysis, such as HTML widgets for interactive plots and word cloud images.
- **README.md:** Provides an overview of the project and instructions for running the code.

## How to Run the Code

To run the code and reproduce the analysis:

1. Clone or download the repository to your local machine.
2. Ensure you have R and RStudio installed.
3. Open RStudio and set the working directory to the location of the downloaded repository.
4. Run the R scripts in the `Code` folder in the following order:
   - `01_load_data.R`: Load and preprocess the datasets.
   - `02_exploratory_data_analysis.R`: Perform exploratory data analysis.
   - `03_modeling.R`: Train and test the multinomial logistic regression model.
   - `04_text_mining.R`: Analyze player nationalities and generate word clouds.
5. Review the output files in the `Output` folder for visualizations and analysis results.

## Technologies Used

- R
- R packages: tidyverse, ggplot2, plotly, gam, interactions, tm, wordcloud, RColorBrewer

## Contributors

- Manoj Hanumantha

## Acknowledgments

The project was completed as part of the MS5130 course assignment. Special thanks to the course instructors for their guidance and support.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
