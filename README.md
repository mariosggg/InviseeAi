## InviseeAi App

By Utilizing a healthcare data set stored in a PostgreSQL database, this InviseeAi app provides a series of data anonymization techniques, visualization, and machine learning simulations. The application simulates Federated Learning and Secure Multiparty Computation and enables users to apply K-anonymity, I-diversity, T-closeness, and Differential Privacy to the data.

## Overview 

InviseeAi makes it possible for researchers and analysts to anonymize health data and try out multiple data privacy methods. It even supports federated learning libraries and secure multiparty computation, thus sealing the bargain for data privacy and security.


## Requirements

- R (version 4.0 or higher)
- RStudio (optional but recommended)
- PostgreSQL database with a `health_data` table
- Required R packages: `shiny`, `dplyr`, `tidyverse`, `DT`, `caret`, `plotly`, `RPostgres`, `shinydashboard`, `shinythemes`



## Setup

Install R and RStudio:
- Download R from [CRAN](https://cran.r-project.org/) and install it.
- Go to [RStudio](https://www.rstudio.com/products/rstudio/download/) and download and install RStudio.

Install PostgreSQL:
- PostgreSQL can be installed by downloading it from [PostgreSQL](https://www.postgresql.org/download/).


## Set up the PostgreSQL database:
- Create a table named "health_data" and a database named "Invisee" with your health data.
- Check if the PostgreSQL server is running and accessible.
   
## Install the required R packages:

If the required packages are not yet installed, open R or RStudio and run the following commands to install them:

   ```R
   if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
   if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
   if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
   if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
   if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
   if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
   if (!requireNamespace("RPostgres", quietly = TRUE)) install.packages("RPostgres")
   if (!requireNamespace("shinydashboard", quietly = TRUE)) install.packages("shinydashboard")
   if (!requireNamespace("shinythemes", quietly = TRUE)) install.packages("shinythemes")
   ```
   
## Configuration Guidelines

-Database connection:
  To synchronize your PostgreSQL configuration, change the database connection settings in the `app.R` file:
```R
con <- dbConnect(RPostgres::Postgres(), dbname = 'Invisee', host = 'localhost', port = 5433, user = 'postgres', password = '12345')
```   

Create the table `health_data`:


In your PostgreSQL database, make sure that there is a `health_data` table with the required schema. Here is an example SQL schema:

   ```sql
   CREATE TABLE health_data (
       id SERIAL PRIMARY KEY,
       age INTEGER,
       gender VARCHAR(10),
       bmi FLOAT,
       bloodpressure INTEGER,
       cholesterol INTEGER,
       diabetes BOOLEAN,
       heartdisease BOOLEAN
   );
   ```
Insert the sample data health_data:

Insert sample data into the `health_data` table for testing purposes.Here is an example SQL schema:

```sql
-- Step 3: Generate Synthetic Data
INSERT INTO health_data (Age, Gender, BMI, BloodPressure, Cholesterol, Diabetes, HeartDisease)
SELECT
    (RANDOM() * 60 + 20)::INT AS Age,
    CASE WHEN RANDOM() > 0.5 THEN 'Male' ELSE 'Female' END AS Gender,
    ROUND((RANDOM() * 10 + 20)::NUMERIC, 1) AS BMI,
    (RANDOM() * 90 + 90)::INT AS BloodPressure,
    (RANDOM() * 150 + 150)::INT AS Cholesterol,
    CASE WHEN RANDOM() > 0.5 THEN 'Yes' ELSE 'No' END AS Diabetes,
    CASE WHEN RANDOM() > 0.5 THEN 'Yes' ELSE 'No' END AS HeartDisease
FROM generate_series(1, 100) s;
   ```
## Application Structure

- `app.R`: Main application file containing the UI and server logic.
- `README.md`: Documentation of the application.
- `data/`: Directory to store any additional data files (if needed).

## Usage Guide

Run the Shiny Application:
- Open `app.R` in RStudio or execute the following command in your R console:

```R
shiny::runApp('path/to/your/app')
```
## Usage

Clone the repository:
```bash
git clone https://github.com/yourusername/inviseeai.git
cd inviseeai
```
## Run the Shiny Application:
- Open `app.R` in RStudio or execute the following command in your R console:
```R
shiny::runApp('path/to/your/app')
   ```
## Navigating the App:

- The interface of the program consists of several tabs:
- Original Data: Presents the original health data of the database.
- K-Anonymized Data: Work with and analyze K-anonymized data.
- The I-Diversity Data is shown and worked on.
- T-Closeness Data: Work with and analyze T-closeness data.
- Differential Privacy Data: Work with and use the data.
- The Federated Learning Model is shown and simulated.
- Secure Computation: Show and simulate the outcome of a secure computation.
- Represent various elements of the health information through data visualization.

## Features

- K-Anonymity: Anonymizes the data by generalizing attributes to ensure each record is indistinguishable from at least `k` other records.
- I-Diversity: Ensures that each group of anonymized records contains at least `i` different values for a sensitive attribute.
- T-Closeness: Measures the distribution of sensitive attributes within each group to ensure it is within `t` distance of the overall distribution.
- Differential Privacy: Adds Laplace noise to the data to ensure privacy while allowing statistical analysis.
- Federated Learning: Simulates decentralized machine learning where models are trained locally on client data and aggregated into a global model.
- Secure Multiparty Computation: Simulates secure computation by distributing data among parties and aggregating results securely.
- Data Visualization: Provides various plots and charts for visualizing the health data.
```{r}
```

## Configuration Options

- Database Connection: As mentioned in the installation instructions, set the PostgreSQL connection information.

Anonymization Parameters: Use the sidebar sliders and text boxes to set various anonymization and privacy techniques.

- Secure Computation and Federated Learning: Set the number of parties for secure computation and the number of clients and rounds for federated learning.

## Troubleshooting

- Database Connection Issues:
  Ensure the PostgreSQL server is running and the connection details are correct.

- Missing Packages:
- Ensure all required R packages are installed. Use the installation commands provided in the prerequisites section.

- Data Issues:
- Ensure the `health_data` table has the correct schema and contains valid data.

## Contributing

- Fork the repository.
- Create a new branch for your feature or bugfix.
- Make your changes and commit them with clear messages.
- Push your branch to your forked repository.
- Create a Pull Request to merge your changes into the main repository.


## Licence

This project is licensed under the secured open called Project . More Info for this Project can be found at https://secured-project.eu/secured_open_call/.

## Acknowledgements

The Shiny framework is used to build the application. Package developers `shiny`, `dplyr`, `tidyverse`, `DT`, `caret`, `plotly`, `RPostgres`, `shinydashboard`, and `shinythemes` must be identified particularly.
