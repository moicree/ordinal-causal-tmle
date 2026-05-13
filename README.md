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

# notes
Each R script in the `scripts/` folders includes a predefined random seed, and users should ensure that file paths are properly configured before execution.
For transparency, the GitHub repository reports both Wald-type and bootstrap-based inference results. 
The manuscript tables present the primary inference method selected for each estimator.

