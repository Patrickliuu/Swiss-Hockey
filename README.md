# Hockey Analytics drafting a RAPM-Model

The data we used was provided by 49ing.ch is accessible on the following poolserver if you are in the ZHAW-Network: \
\ \\shared.zhaw.ch\pools\t\T-IDP-49ing
\
Currently we started to keep track of our efforts in our [shared](https://www.overleaf.com/9672456868jstptpdfmggs) document.

To clone the repo including all the lfs files use the following commands:
```
gh repo clone github.zhaw.ch/zumbueti/HockeyAnalytics
```
```
git lfs fetch --all
```
```
git lfs pull
```

Once this is done, the dive into RAPM can begin:
To get started open the rapm_calculator.Rmd in the **scripts** folder to calculate the RAPM coefficients.
All the generated output will be saved in the folder called **data/output**.

To run the RAPM-Model you have to start with the **rapm_calculator.Rmd** markdown file. This make the heavy calculation.
In a later step you could use the **rapm_validator.Rmd** to validate your models outcome.
Bear in mind this was run with the original data from 49ing for the seasons 2020/21 and 2021/22. Future data might include unprecedented changes. 


In the **report.R** the plots and illustrations for the PA were created.
The files **rapm_calculator.R** and **linear_regression_validation.R** are auxillary files to run the models in a loop.



Datastructure is as following:
```bash

├── HockeyAnalytics.Rproj
├── README.md
├── data
│   ├── input
│   │   ├── defenders_L1_S2021.csv
│   │   ├── defenders_L1_S2022.csv
│   │   ├── defenders_L2_S2021.csv
│   │   ├── defenders_L2_S2022.csv
│   │   ├── pbp_events_L1_S2021_STRegular_fixed_coords.csv
│   │   ├── pbp_events_L1_S2022_STRegular_fixed_coords.csv
│   │   ├── pbp_events_L2_S2021_STRegular_fixed_coords.csv
│   │   ├── pbp_events_L2_S2022_STRegular_fixed_coords.csv
│   │   ├── players_L1_S2021.csv
│   │   ├── players_L1_S2022.csv
│   │   ├── players_L2_S2021.csv
│   │   ├── players_L2_S2022.csv
│   │   ├── strikers_L1_S2021.csv
│   │   ├── strikers_L1_S2022.csv
│   │   ├── strikers_L2_S2021.csv
│   │   └── strikers_L2_S2022.csv
│   ├── output
│   │   ├── RapmCoefficients_L1_S2021.csv
│   │   ├── RapmCoefficients_L1_S2022.csv
│   │   ├── RapmCoefficients_L2_S2021.csv
│   │   ├── RapmCoefficients_L2_S2022.csv
│   │   ├── RapmEvenStrengthDefender_L1_S2021.csv
│   │   ├── RapmEvenStrengthDefender_L1_S2022.csv
│   │   ├── RapmEvenStrengthDefender_L2_S2021.csv
│   │   ├── RapmEvenStrengthDefender_L2_S2022.csv
│   │   ├── RapmEvenStrengthStriker_L1_S2021.csv
│   │   ├── RapmEvenStrengthStriker_L1_S2022.csv
│   │   ├── RapmEvenStrengthStriker_L2_S2021.csv
│   │   ├── RapmEvenStrengthStriker_L2_S2022.csv
│   │   ├── filtered_coefficients
│   │   ├── filtered_coefficients_L1_S2021.csv
│   │   ├── filtered_coefficients_L1_S2022.csv
│   │   ├── filtered_coefficients_L2_S2021.csv
│   │   ├── filtered_coefficients_L2_S2022.csv
│   │   ├── xGF_60_lin.png
│   │   ├── xGF_60_lin_black.png
│   │   ├── xGF_60_ridge.png
│   │   ├── xGF_60_ridge_all.png
│   │   └── xGF_60_ridge_all_black.png
│   └── temp
│       ├── RAPM_xGF_L1_S2021.RData
│       ├── RAPM_xGF_L1_S2022.RData
│       ├── RAPM_xGF_L2_S2021.RData
│       ├── RAPM_xGF_L2_S2022.RData
│       ├── Run_RAPM_L1_S2021.RData
│       ├── Run_RAPM_L1_S2022.RData
│       ├── Run_RAPM_L2_S2021.RData
│       ├── Run_RAPM_L2_S2022.RData
│       ├── shift_length_L1_S2021.RData
│       ├── shift_length_L1_S2022.RData
│       ├── shift_length_L2_S2021.RData
│       ├── shift_length_L2_S2022.RData
│       ├── shifts_grouped_ev.RData
│       ├── xGF_60_L1_S2021.RData
│       ├── xGF_60_L1_S2022.RData
│       ├── xGF_60_L2_S2021.RData
│       └── xGF_60_L2_S2022.RData
├── docs
│   ├── 49ING-Documentationpbpfields-180822-1617.pdf
│   ├── datenFehlerListe.txt
│   └── technisches-reglement.pdf
└── scripts
    ├── Funktion_Residuenanalyse.R
    ├── linear_regression_validation.R
    ├── rapm_calculator.R
    ├── rapm_calculator.Rmd
    ├── rapm_validator.Rmd
    └── report.R

```


Our first approach was to follow the guideline from Patrick Bacon [see here](https://github.com/TopDownHockey/RAPM/blob/main/RAPM_1819_Example_Code.R)
