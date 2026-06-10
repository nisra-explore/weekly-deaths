# Weekly Deaths in Northern Ireland

This repository contains the code used to build the **Weekly Deaths in Northern Ireland** dashboard. The dashboard is a Quarto website that presents provisional weekly death registration statistics for Northern Ireland, including comparisons with expected deaths and breakdowns by sex, age, Local Government District, place of death, and deaths registered versus deaths occurred.

The project is designed to support reproducible analytical publication by keeping the data preparation, dashboard code, styling, and rendered website outputs together in one version-controlled repository.

## Contents

* [What the dashboard shows](#what-the-dashboard-shows)
* [Repository structure](#repository-structure)
* [Prerequisites](#prerequisites)
* [Getting started](#getting-started)
* [Restoring the R environment](#restoring-the-r-environment)
* [Running the dashboard locally](#running-the-dashboard-locally)
* [Updating the dashboard](#updating-the-dashboard)
* [Rendering the published site](#rendering-the-published-site)
* [Outputs and downloadable data](#outputs-and-downloadable-data)
* [Troubleshooting](#troubleshooting)
* [Contributing](#contributing)

## What the dashboard shows

The dashboard provides a weekly summary of deaths registered in Northern Ireland. It includes:

* latest provisional weekly registered deaths
* comparison with the previous week
* comparison with expected weekly deaths
* flu/pneumonia and COVID-19 related weekly deaths
* sex breakdown for the latest week and year to date
* age breakdown for the latest week and year to date
* Local Government District breakdown for the latest week and year to date
* place of death breakdown
* comparison between deaths registered and deaths occurred

The dashboard is built from data retrieved from NISRA public data APIs and rendered as a Quarto website.

## Repository structure

```text
weekly-deaths/
├── .github/workflows/       # GitHub Actions workflow for rendering the Quarto site
├── _includes/               # HTML includes used by the Quarto site
├── code/
│   ├── config.R             # Project settings, package loading, colours, logos and functions
│   ├── data_portal_prep.R   # Data portal preparation script
│   └── functions/           # Reusable helper functions
├── data/images/             # Logos and image assets
├── docs/                    # Rendered website output
├── images/                  # Additional image assets
├── maps/lgd/                # Local Government District map shapefiles
├── renv/                    # renv project infrastructure
├── _quarto.yml              # Quarto website configuration
├── about.qmd                # About page
├── dashboard_data_prep.R    # Main data preparation script used by the dashboard
├── index.qmd                # Main dashboard page
├── nisra-styles.css         # Dashboard styling
├── renv.lock                # Locked R package versions
├── renv_setup.R             # Helper script for restoring and checking renv
└── weekly_deaths.Rproj      # RStudio project file
```

## Prerequisites

Before running the project, make sure you have:

* R installed
* RStudio installed
* Quarto installed
* Git installed, if you are cloning or contributing through Git
* access to the internet, because the dashboard reads data from NISRA public data APIs
* access to any internal package sources required by the `renv.lock` file, if running inside the NISRA environment

The project uses `renv` to manage package versions. This helps ensure that the dashboard can be rebuilt using the same package versions recorded in `renv.lock`.

## Getting started

Clone the repository or download it from GitHub.

Using Git:

```bash
git clone https://github.com/nisra-explore/weekly-deaths.git
```

Then open the project in RStudio by opening:

```text
weekly_deaths.Rproj
```

Opening the `.Rproj` file ensures that paths resolve correctly through the `here` package.

## Restoring the R environment

The project uses `renv`. When opening the project for the first time, restore the package environment from the lockfile.

In the R console, run:

```r
renv::restore()
```

When prompted, confirm that you want to install the required packages.

After restore has completed, check the project status:

```r
renv::status()
```

A successful setup should report that the project is consistent, or show no outstanding package issues.

You can also open and run the helper script:

```text
renv_setup.R
```

This script contains the key `renv::restore()`, `renv::status()`, `renv::install()` and `renv::snapshot()` commands used for the project.

## Running the dashboard locally

The main dashboard file is:

```text
index.qmd
```

To preview the dashboard locally, use one of the following methods.

From RStudio:

1. Open `index.qmd`.
2. Select **Render** or **Preview**, depending on your RStudio/Quarto setup.

From the R console:

```r
quarto::quarto_preview()
```

Or from the terminal:

```bash
quarto preview
```

The dashboard sources `dashboard_data_prep.R`, which loads configuration from `code/config.R`, reads the required datasets from the NISRA public data APIs, prepares the dashboard tables and chart data, and creates the objects used in `index.qmd`.

## Updating the dashboard

Use this workflow when updating the dashboard for a new publication cycle.

### 1. Pull the latest code

Before making changes, update your local copy:

```bash
git pull
```

### 2. Open the RStudio project

Open:

```text
weekly_deaths.Rproj
```

### 3. Restore or check packages

Run:

```r
renv::status()
```

If package changes are required, run:

```r
renv::restore()
```

### 4. Check configuration

Review:

```text
code/config.R
```

This file contains project-level settings, package loading, NISRA colours, logo configuration, output folder creation, and function sourcing.

Check that settings such as the title, statistics type, theme, logos and any project paths are correct.

### 5. Run data preparation

Run:

```r
source("dashboard_data_prep.R")
```

This will retrieve the current API datasets and prepare the dashboard objects.

### 6. Render the dashboard

Render the Quarto website:

```r
quarto::quarto_render()
```

Or from the terminal:

```bash
quarto render
```

### 7. Review the output

The rendered site is written to:

```text
docs/
```

Open `docs/index.html` and check that:

* the latest week is correct
* summary values update as expected
* charts render correctly
* maps render correctly
* download buttons work
* accessible alternative text is still appropriate
* page layout works on desktop and mobile widths
* there are no warnings or errors that need investigation

## Rendering the published site

The Quarto configuration is stored in:

```text
_quarto.yml
```

The project is configured as a Quarto website, with rendered output written to:

```text
docs/
```

This means the `docs/` folder contains the static HTML files and supporting assets generated from the source files.

To render the full website, run:

```bash
quarto render
```

After rendering, commit both source changes and any updated files in `docs/` if the published site is served from the repository’s `docs/` folder.

## Outputs and downloadable data

The project creates output folders used by the dashboard download buttons. These are configured in `code/config.R`.

Key output locations include:

```text
outputs/
outputs/figdata/
```

The dashboard uses helper functions in `code/functions/`, including functions for generating downloadable chart data tables.

When reviewing outputs, check that downloadable CSV or Excel files contain the correct columns, labels, and latest values.

## Data sources

The dashboard reads data from NISRA public API endpoints. The main datasets are configured in `dashboard_data_prep.R`.

Current datasets include:

* weekly deaths summary
* weekly deaths by sex and age
* weekly deaths by place of death
* weekly deaths by Local Government District
* weekly deaths occurred

The dashboard should be treated as provisional where source data are provisional.

## Map data

Local Government District map files are stored in:

```text
maps/lgd/
```

The dashboard uses these files to create the LGD maps. Do not remove or rename these files unless the map loading code is updated at the same time.

## Package management

Use `renv` when adding or updating R packages.

To add a package:

```r
renv::install("package_name")
```

Then update the lockfile:

```r
renv::snapshot()
```

Commit the updated `renv.lock` file so that other users can restore the same package environment.

When receiving package changes from another contributor, run:

```r
renv::restore()
```

## Troubleshooting

### The project cannot find files

Open the project through `weekly_deaths.Rproj` rather than opening individual files directly. The project uses `here`, so working directory context matters.

### Packages are missing

Run:

```r
renv::restore()
```

Then check:

```r
renv::status()
```

### The dashboard does not update

Check that:

* you have internet access
* the NISRA public API endpoints are available
* `dashboard_data_prep.R` runs without errors
* the expected latest week is present in the API data
* the Quarto site has been re-rendered after running the data preparation

### Maps do not render

Check that the LGD shapefiles are present in:

```text
maps/lgd/
```

Also check that the `sf` package is installed correctly through `renv`.

### Download buttons do not work

Check that the output folders exist:

```text
outputs/
outputs/figdata/
```

If needed, rerun `code/config.R` or render the dashboard again. The configuration script creates these folders if they are missing.

### Quarto render fails

Try rendering from the terminal to see the full error:

```bash
quarto render
```

Common causes include missing packages, unavailable API data, missing image assets, or missing map files.

## Contributing

When making changes:

1. Create a new branch.
2. Make the required edits.
3. Run `renv::status()`.
4. Render the site locally.
5. Review `docs/index.html`.
6. Commit source changes and rendered output where required.
7. Open a pull request.

Use clear commit messages, for example:

```bash
git commit -m "Update weekly deaths dashboard for latest data"
```

## Suggested quality checks before publishing

Before publishing or merging changes, check:

* the latest week ending date is correct
* headline figures match the source data
* all charts and maps display correctly
* all download files generate correctly
* page text is clear and accessible
* chart alt text remains accurate
* no development-only files have been committed
* `renv::status()` reports no unexpected issues
* the rendered `docs/` output is up to date

