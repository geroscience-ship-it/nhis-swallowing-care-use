# Infrequent health care use for swallowing problems among community-dwelling older US adults

Analysis code and computed results for a pooled cross-sectional analysis of the
2022 and 2025 National Health Interview Survey (NHIS).

## Contents

| Path | Content |
| --- | --- |
| `R/NHIS_care_use_tables_v3.R` | data preparation and all estimates; writes every file in `results/` |
| `R/Figure_2_forest_plot.R` | draws Figure 2 from `results/Figure_2.csv` and `results/Table_S7.csv` |
| `results/` | every estimate reported in the manuscript, exactly as written by the scripts |

`results/` is included so that each number in the manuscript can be checked
without access to the raw data or a working R installation.

| File | Content |
| --- | --- |
| `Figure_1.csv` | selection of the study population |
| `Figure_2.csv` | prevalence ratios, entry model of each block |
| `Table_1.csv` | sample characteristics and weighted proportion with care use |
| `Table_2.csv` | swallowing-specific outcome |
| `Table_S1.csv` – `Table_S8.csv` | supplementary tables |
| `Check_*.csv` | estimates quoted only in the running text, and missing-value counts |
| `sessionInfo.txt` | R session of the run that produced these files |

## Data

The NHIS public-use files are not redistributed here. They are works of the US
federal government, are in the public domain, and can be downloaded from the
National Center for Health Statistics at
<https://www.cdc.gov/nchs/nhis/documentation/>.

The scripts expect this layout:

```
<NHIS_DIR>/NHIS 2022/adult22.csv
<NHIS_DIR>/NHIS 2022/adultinc22.csv
<NHIS_DIR>/NHIS 2025/adult25.csv
<NHIS_DIR>/NHIS 2025/adultinc25.csv
```

NCHS publishes a checksum list with each survey year; the downloaded files were
verified against it before analysis.

## Reproducing the analysis

```sh
export NHIS_DIR=/path/to/your/NHIS/folder
Rscript R/NHIS_care_use_tables_v3.R
Rscript R/Figure_2_forest_plot.R
```

`NHIS_DIR` defaults to `~/Desktop/NHAT` if unset. Output is written to
`<NHIS_DIR>/Draft final/Results` and `<NHIS_DIR>/Draft final/Tabellen und
Abbildungen`, not into this repository.

## Software

R 4.6.1 with survey 4.5, mitools 2.7, survival 3.8-6 and Matrix 1.7-5. The
forest plot uses base graphics only. Full session information is in
`results/sessionInfo.txt`.

## Methods in brief

Both survey years are pooled and analysed with the final adult weight
(`WTFA_A`), the public-use stratum (`PSTRAT`) and the primary sampling unit
(`PPSU`), using Taylor linearization. Estimates for adults with swallowing
problems are obtained by domain analysis on the complete design object.
Prevalence ratios come from survey-weighted modified Poisson regression with a
log link, fitted separately in each of the ten official NCHS income imputations
and pooled by Rubin's rules with Barnard-Rubin degrees of freedom. Adjusted
proportions are predictive margins. Proportions in Table 1 additionally carry
effective sample size, Korn-Graubard intervals and the NCHS presentation flag.

## License

Code is released under the MIT License, see `LICENSE`. The NHIS public-use data
are in the public domain.
