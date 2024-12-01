# ADS-Planner

A tool to distribute INSA students into ADS sessions.

## Installing & Running

To install (or update) the ADS-Planner R package, simply clone this repo, start an interactive R session from the root folder and run :

```R
install.packages(".",repos=NULL)
```

And to run the app :

```R
ADSPlanner::run()
```

The web interface should open automatically. If it doesn't, you can open the link next to `Browsing` in the command output.

For development purposes, the app can be started more simply by running `shiny::runApp("R")` from the root of the repo.

## ADS

ADS (Action Découverte des Spécialités) is a teaching module aimed at presenting the specialties of the engineering program at INSA to students in the preparatory program. This module consists of three sessions. Each student is asked to rank the specialties by preference to assign them to the three half-day sessions.

The students concerned by ADS are the following :
- 2nd-year students 
- Repeating students
- 2/3 accomodations
- EMIR and MICA students

Each group is treated differently (details below).

## Inputs (spreadsheets)

- Students' choices (Rank / Last name / First name / ID / E-mail address / Choices)
- Traditionnal track students list
- MICA students list
- EMIR students list

## Outputs (spreadsheets)

Students' choices with an additional column for the final results, and a new sheet containing the list of students who have not responded (Last name / First name / ID / Email address / Reason). Each student must be color-coded based on the reason for not answering.

## Students' Distribution

The distribution of the students is determined by the following conditions :

#### Traditional track students

Based on first-year rank

#### EMIR and MICA students

Based on first-year rank (two different rankings)
Limited to 4 choices (only 4 possible departments)

#### Repeating students

Based on second-year rank

## Features

The application must include :
- Fields for input files
- Field for output file
- Tabs to visualize :
  - Students' choices
  - Students who have not responded
  - Assignments

The tab showing students who have not responded must allow the user to edit the reason for the lack of response.

## Technology

### Why R ?

- **Coherence** : Integration with existing code already written in R
  - Generation of jury files
  - Assignment to departments
  - Generation of repeat contracts
  - ADS assignment

- **Maintenance** : Simplified with a single programming language

- **Ease of manipulation** : R facilitates data frame manipulation

- **Longevity** : R code is likely to remain stable over time, compared to Python

- **Statistics** : R is the preferred language for statistics and data visualization, with statistical operations performed on other parts of the code

- **Ease of use of R-Shiny** : Enables web application development and graphical interface creation
