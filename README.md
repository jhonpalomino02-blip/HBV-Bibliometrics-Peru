# Reproducibility Guide

This repository contains the complete, reproducible R pipeline and curated datasets for this bibliometric analysis. To ensure seamless execution and exact replication of the high-resolution figures and summary tables, please follow these instructions carefully.

## 1. Download and Directory Setup
1. Download this repository as a `.zip` file from GitHub.
2. Extract the unzipped folder and place it directly into your computer's **Documents** folder. 
3. Ensure the internal folder structure remains intact (the `data/` folder must be in the same directory as the master script).

## 2. Environment Setup in RStudio
1. Open **RStudio**.
2. Open the master script file: `HBV_bibliometrics_MASTER.R`.

## 3. Executing the Master Pipeline
* ⚠️ **DO NOT** select all the code and click the "Run" button. This line-by-line execution can cause memory carry-over issues and interrupt the automated environment setup.
* ✅ **INSTEAD, use the "Source" button:** Click the **Source** button (the icon with the blue arrow pointing down, located at the top right of the script panel), or use the keyboard shortcut `Ctrl + Shift + Enter` (`Cmd + Shift + Enter` on Mac).

### What to expect:
Upon clicking **Source**, the script will autonomously:
* Install and load any missing required R packages.
* Process the curated datasets (`base_datos_hbv.xlsx`, `burden_by_department.csv`, etc.).
* Generate all main and supplementary figures (in high-resolution 600 DPI PNG and PDF formats) and summary tables.
* Output everything systematically into a newly created `outputs/` folder within the same directory.