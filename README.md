# Copa America 2023-24 Analysis Dashboard

This project provides a comprehensive data-driven analysis of the Copa America 2023-24 tournament, focusing on individual player performances, team statistics, and key event visualizations. The project utilizes StatsBomb data and various R packages to create an interactive Shiny dashboard where users can explore detailed insights into the tournament.

## Features

- **Shots and Goals Analysis**: Compare the number of shots and goals across different teams in the tournament.
- **Top Players Analysis**: Visualize key performance metrics for standout players, such as James Rodríguez's passes and assists, as well as Lautaro Martínez's shot map.
- **Scouting Reports**: Generate radar plots to compare selected players' performance across various metrics. This is broken down by attackers, midfielders, and defenders.
- **Interactive Visualizations**: The dashboard includes interactive plots created using `plotly` and `ggplot2`, allowing for dynamic exploration of the data.

## Data

The analysis is based on data obtained from the StatsBomb API using the `StatsBombR` package. The data is filtered for the Copa America 2023-24 tournament, with specific focus on:
- Match events (shots, passes, assists, dribbles, etc.)
- Player performance metrics (xG, key passes, assists, duels won, etc.)
- Team statistics (total shots, goals, passes in the final third, etc.)

## Installation

1. **Clone the repository:**
    ```bash
    git clone https://github.com/yourusername/socceranalytics.git
    cd socceranalytics
    ```

2. **Install the required packages:**
    The project requires several R packages. You can install them using `install.packages()` for CRAN packages and `devtools::install_github()` for GitHub packages.

    ```r
    install.packages(c("shiny", "ggplot2", "dplyr", "plotly", "ggthemes", "ggrepel", "ggsoccer", "SBpitch", "tidyverse"))
    devtools::install_github("statsbomb/StatsBombR")
    ```

3. **Run the Shiny App:**
    You can run the Shiny app locally by opening `app.R` or using the command:
    ```r
    shiny::runApp("ShinyVanilla.R")
    ```

## Usage

### Shots and Goals Analysis

- **Select a Team**: Choose one or more teams from the dropdown to compare their total shots and goals.
- **Visualize**: The bar plot will dynamically update to show the selected teams' performance.

### Top Players Analysis

- **James Rodríguez**: Explore the passes and assists made by James Rodríguez, the Golden Ball winner.
- **Lautaro Martínez**: Visualize the shot map for Lautaro Martínez, focusing on the expected goals (xG) and shot locations.

### Scouting Reports

- **Select Players**: Choose attackers, midfielders, and defenders from the dropdowns to compare their percentile ranks across key performance metrics.
- **Radar Plot**: View the radar plots for a graphical comparison of selected players.

## File Structure

- `ShinyVanilla.R`: Main file to run the Shiny application.
- `README.md`: This file, providing an overview and instructions for the project.

## Contributing

Contributions are welcome! If you have suggestions for improvements or new features, feel free to create an issue or submit a pull request.

## License


## Acknowledgements

- **StatsBomb**: Special thanks to StatsBomb for providing open access to their detailed football event data.
- **R Packages**: This project leverages several R packages, including `StatsBombR`, `ggplot2`, `plotly`, and `shiny`.

