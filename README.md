# Golden Retriever Lifetime Study Dataset Tidying

## Description

This repository was created to tidy and explore datasets collected from the Golden Retriever Lifetime Study (https://www.morrisanimalfoundation.org/golden-retriever-lifetime-study).

This project is exploring cancers in golden retriever dogs and examines frequency and factors associated with diagnosis and survival, as part of a post-doctoral research project I am undertaking at the RVC. The main cancers of interest are: haemangiosarcoma, lymphoma and mast cell tumours.

The datasets to be used in these analyses come from csv files from the database manager for GRLS. The datasets consist of owner and veterinarian annual questionnaire information. Broadly, the information relates to: dog demographics, dog lifestyle, dog environment, dog health, dog medications. In future there will also be data on diet.

Seperate repositories will be created for specific analyses.

## Features

There are several main files in this repository:
- Functions file (GRLS_functions.R) - where all functions used for data manipulation and tidying are stored. There is a .qmd also with further information
- GRLS_repro.qmd = file generating variables associated with reproductive information
- GRLS_medications.qmd = file exploring and tidying medication use
- GRLS comorbidities dataset.qmd = file exploring the comorbidities data and tidying for analysis
- GRLS activity classification method.qmd = file exploring dog exercise frequency, intensity, duration from owner questionaires to generate variables for analysis
- 240103 GRLS enviro_lifestyle.qmd = file exploring home and environmental data from owner questionaires and creating variables for analysis

