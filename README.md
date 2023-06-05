# Supplementary Material for "Relating the PDP and PFI to the Data Generating Process"
by Christoph Molnar, Timo Freiesleben, Gunnar König, Julia Herbinger, Tim Reisinger, Giuseppe Casalicchio, Marvin Wright, Bernd Bischl; submitted at the XAI Conference 2023.

The full paper can be accessed via [TBD].

This repository contains:
- The appendix (appendix.pdf).
- The code for the application (code-application.zip)
- The code for the model-based uncertainty quantification experiments (code_modelbased.zip)
- Code for the confidence interval experiment (ci-experiment.R)
- Code for the coverage plot (plot_coverage.R)
- Code for PDP uncertainty (pdp-uncertainty.R)
- Code for the PDP biases visualization (pdp-biases-visualization.R)
- Helper functions used in the scripts (folder ./R/)
- DESCRIPTION file describing the package dependencies


## How to run the code

Assuming that R is installed, the dependencies for the R scripts can be installed by typing in the R console `devtools::install_dev_deps()` while in the folder of the repository.
The DESCRIPTION file contains the dependencies.
First, make sure to have R installed.

Each .R file in the code folder can be run by calling the 

```{bash}
Rscript code/pdp-biases-visualiation.R
```

For the application and model-based uncertainty quantification more detailed instructions can be found in the respective zip files.
