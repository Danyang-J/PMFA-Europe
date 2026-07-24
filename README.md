# PMFA Plastic Release Model for Europe

## Overview

This repository provides the R code and input data for a probabilistic material flow analysis (PMFA) model of plastic flows and environmental releases in Europe. The model follows plastic flows through the anthroposphere from a whole-life-cycle perspective and quantifies polymer-specific macroplastic and microplastic releases to soil and water.

The PMFA requires two main categories of input data:

1.  material inputs entering the modeled system; and
2.  transfer coefficients (TCs) describing the fraction of mass transferred from one model compartment to the next.

Probability distributions can be assigned to material inputs and transfer coefficients to represent their uncertainty. During the Monte Carlo simulation, each parameter is sampled from its assigned distribution and the resulting system of mass flows is solved repeatedly.

The model includes seven commodity polymers:

-   low-density polyethylene (LDPE);
-   high-density polyethylene (HDPE);
-   polypropylene (PP);
-   polystyrene (PS);
-   expanded polystyrene (EPS);
-   polyvinyl chloride (PVC); and
-   polyethylene terephthalate (PET).

## Scope of this repository

The model code is based on the modeling approach presented by Jiang and Nowack (2025) and the model framework available in [PMFA-Plastic-release-model](https://github.com/Danyang-J/PMFA-Plastic-release-model).

The framework repository contains a simplified model and a synthetic input dataset intended to demonstrate the required data structure, probabilistic parameterization, and calculation workflow. This repository contains the complete European model and the country-specific input data used for the European application.

The geographical scope comprises the 27 Member States of the European Union, Norway, Switzerland, and the United Kingdom. Results are calculated for each of the 30 modeled countries and are additionally aggregated across the complete modeled region. The regional aggregate is identified as `EU` in the model outputs.

The numerical values in `Input/20260618_FeedData.xlsx` are the model inputs used to generate the results provided in this repository. The data sources, proxy assumptions, geographical adaptations, and uncertainty distributions should be interpreted together with the methods and supporting information of the associated study.

## Repository structure

``` text
.
├── Code/
│   ├── 00_MasterScript.R
│   ├── 01_InputFormatting.R
│   ├── 02_SpecialFlowsMod1.R
│   ├── 03_SpecialFlowsMod2.R
│   ├── 04_Merging.R
│   ├── 05_CalculationScript.R
│   ├── 06.*_ExportResults*.R
│   ├── 07_ExportFlows.R
│   ├── 08.*_ExportResultsEOL*.R
│   ├── 09.*_*.R
│   ├── 10_DataForWriting.R
│   ├── 11.*_TikZ*.R
│   ├── TC_Hygiene.R
│   ├── TC_Textiles.R
│   └── functions.needed*.R
├── Input/
│   └── 20260618_FeedData.xlsx
├── Results/
│   ├── Graphs/
│   └── Tables/
├── Charts/
│   ├── Chart_*.pdf
│   ├── Chart_*.tex
│   └── flow_*.txt
├── LICENSE
└── README.md
```

## Model workflow

The model is run through `Code/00_MasterScript.R`, which executes the following steps:

1.  `01_InputFormatting.R` imports the material inputs and transfer coefficients, assigns their probability distributions, and normalizes outgoing flows.
2.  `02_SpecialFlowsMod1.R` and `03_SpecialFlowsMod2.R` calculate transfer coefficients for selected release pathways requiring additional parameterization, including textiles and flushing.
3.  `04_Merging.R` combines transfer coefficients within the anthroposphere with those for environmental release flows and prepares the complete transfer coefficient matrix.
4.  `05_CalculationScript.R` constructs and solves the material flow system for every country and polymer using Monte Carlo simulation.
5.  The scripts numbered `06` to `08` aggregate and export emission factors, environmental releases, end-of-life flows, and regional results.
6.  The scripts numbered `09` generate the emission maps (`09.1`), polymer- and product-specific heatmaps (`09.2`), country-level comparisons (`09.3`), and country heterogeneity figures (`09.4`).
7.  `10_DataForWriting.R` prepares selected aggregated results for reporting.
8.  The scripts numbered `11` prepare and generate the flow charts in `Charts/`.

The default configuration uses 10,000 Monte Carlo iterations:

``` r
SIM <- 10^4
```

Results are therefore distributions that capture the uncertainty propagated from the model inputs.

## Requirements

The model requires R and the following R packages:

``` r
install.packages(c(
  "openxlsx",
  "trapezoid",
  "mc2d",
  "xlsx",
  "dplyr",
  "readr",
  "writexl",
  "stringr",
  "purrr",
  "tidyr",
  "ggplot2",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "viridis",
  "patchwork",
  "scales",
  "ggpattern",
  "gridExtra",
  "ggrepel",
  "cowplot",
  "sm"
))
```

The `xlsx` package requires a working Java installation. Generation of the PDF flow charts from the exported TeX files also requires a LaTeX installation.

## Quick start

Clone the repository and run the master script from the repository root:

``` bash
git clone https://github.com/Danyang-J/PMFA-Europe.git
cd PMFA-Europe
Rscript Code/00_MasterScript.R
```

The relative file paths used by the scripts assume that the current working directory is the repository root. The master script creates the required output directories automatically. Runtime and memory use depend on the computer and the number of Monte Carlo iterations.

To use another workbook, place it in `Input/` and change the following setting in `Code/00_MasterScript.R`:

``` r
excel.file <- "20260618_FeedData.xlsx"
```

## Input data

`Input/20260618_FeedData.xlsx` contains the input data used for the European model application.

| Worksheet | Content |
|----|----|
| `ReadMe` | Description of the workbook and its worksheets |
| `Input` | Polymer-specific material inputs entering the modeled systems |
| `MFA1` | Transfer coefficients for pre-consumer production and allocation from product sectors to product subcategories |
| `MFA2` | Transfer coefficients for post-consumer collection and recycling |
| `EM1` | Transfer coefficients for release flows from pre- and post-consumer processes |
| `EM2` | Transfer coefficients for release flows during the use stage, including littering and dumping |
| `EM3` | Transfer coefficients for release flows during the end-of-life stage, including composting, wastewater treatment, sludge application, and air deposition |
| `Textiles` | Special parameters defining textile releases |
| `Flushing` | Special parameters defining releases through flushing |
| `Label` | Labels for model compartments and graphical outputs |
| `GeoCode` | Country and region names, abbreviations, population, and geographical information |

Material inputs and transfer coefficients use consistent mass units and model compartment names. For each source compartment, outgoing transfer coefficients describe the fractions transferred to destination compartments. The scripts combine and normalize these coefficients to satisfy the mass-balance requirements of the model.

## Outputs

Running the complete workflow produces or updates three main categories of outputs:

| Output | Description |
|----|----|
| `Results/Emissions/OutputMass_<country>_<polymer>.Rdata` | Monte Carlo results for each country and polymer |
| `Results/Tables/` | Aggregated emission factors, environmental releases, end-of-life results, and flow tables |
| `Results/Graphs/` | Maps and statistical graphics generated from the aggregated model results |
| `Charts/` | Country-, polymer-, and region-level flow charts in PDF and editable TeX formats |

The large intermediate `.Rdata` files generated in `Results/Emissions/` are not included in the repository. They are recreated when the complete model workflow is run.

The `Charts/` directory contains the complete set of flow charts generated from the model results. The PDF files provide the rendered charts, while the corresponding TeX and text files retain the editable chart definitions and underlying formatted flow data.

An aggregated flow chart for the complete modeled region is available here:

[Aggregated plastic flow chart for Europe](Charts/Chart_Aggregated_Plastic_EU.pdf)

## Citation

This repository accompanies a study of plastic flows and environmental releases in Europe. Citation information for the European model application will be added when the associated publication becomes available.

For the underlying plastic release model and modeling approach, please cite:

> Jiang, D., & Nowack, B. (2025). Reconciling plastic release: Comprehensive modeling of macro- and microplastic flows to the environment. *Environmental Pollution, 383*, 126800. <https://doi.org/10.1016/j.envpol.2025.126800>

``` bibtex
@article{jiang2025reconciling,
  title   = {Reconciling plastic release: Comprehensive modeling of macro- and microplastic flows to the environment},
  author  = {Jiang, Danyang and Nowack, Bernd},
  journal = {Environmental Pollution},
  volume  = {383},
  pages   = {126800},
  year    = {2025},
  doi     = {10.1016/j.envpol.2025.126800}
}
```

## License

This repository is licensed under the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0). See `LICENSE.txt` for the full license text.
