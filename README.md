# Homoacetogen_WY4_Supplementary
Homoacetogenic Clostridium magnum WY-4 as a novel bio-stabilizing agent in anaerobic digestion

# Supplementary R scripts for WY-4 study

This repository contains supplementary R scripts for the article  
Homoacetogenic *Clostridium magnum* WY-4 as a novel bio-stabilizing agent in anaerobic digestion: enhanced methane yield and metabolic modulation.

## Scripts
- **Gompertz_model.R**  
  Fits methane production data to the modified Gompertz model and estimates kinetic parameters:  
  - Gm (potential methane yield)  
  - Rm (maximum methane production rate)  
  - λ (lag phase)  
  The script requires input data in a CSV format with methane production values over time.

- **Correlation_network.R**  
  Performs Pearson correlation analysis among AHL concentrations, microbial community composition, and methane yield.  
  Outputs:  
  - a correlation matrix (CSV)  
  - a co-occurrence network plot (PDF/PNG)  
  Requires as input a CSV table with quantitative data of AHLs, microbial genera abundances, and methane yields.

## Notes
- Original experimental datasets are **not included** in this repository.  
- Users can adapt their own datasets to the required format to run the scripts.  
- The code is provided as supplementary material to ensure reproducibility of the analysis described in the manuscript.

## Citation
If you use these scripts, please cite the article
