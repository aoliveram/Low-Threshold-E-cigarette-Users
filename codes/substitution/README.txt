# Social-media substitution analysis (2026-10-21)
Run from codes/, in order:
  Rscript substitution/01-nominations-distribution.R   # friends named per wave; P25/P75
  Rscript substitution/02-build-panel.R                # student x wave panel W4-W10
  Rscript substitution/03-substitution-analysis.R      # groups, first differences, FE, figures
Inputs: ../Cleaned_Data/ADVANCE_W1-W8.xlsx and ../Cleaned_Data/ADVANCE_W9-W10.xlsx.
Outputs: ../outputs_substitution/ (git-ignored); the figures and substitution_estimates.csv are copied in results/substitution/.
00-check-friend-cells.R and 04-check-nominations.R are one-off checks (they never print typed names).
Definitions: friends = roster + typed names in own-school slots; few <= P25, several >= P75;
zero nominations with no social-media answers dropped as incomplete surveys;
SM use = number of 11 core platforms used daily or more (socialmed_*); vaping content = paper's sm_post mean.
