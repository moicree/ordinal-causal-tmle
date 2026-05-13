# ordinal_causal_tmle

This project aims to estimate causal effects for ordinal outcomes using methods such as G-computation, IPW, and TMLE.

## Project Structure

### analysis/
Code for real data analysis
- R/ : function definitions
- scripts/ : scripts to run analysis(make sure to set the correct paths before running)
- results/ : stores analysis outputs

### simulation/
Code for simulation studies
- R/ : function definitions
- scripts/ : scripts to run simulations(make sure to set the correct paths before running)
- results/ : stores simulation outputs

### Python visualization code

# notes
Each R script in the `scripts/` folders includes a predefined random seed, and users should ensure that file paths are properly configured before execution.

Updated analysis scripts and added Python visualization code and supplementary result files. Clarified Wald versus bootstrap inference reporting and improved reproducibility documentation.

