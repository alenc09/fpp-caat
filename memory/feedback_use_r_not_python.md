---
name: Use R for all data analysis
description: User wants all data analysis done in R, not Python/pandas, to match existing project scripts
type: feedback
---

Always use R (not Python/pandas) for data analysis in this project. All existing scripts are in R and the user wants to maintain that pattern.

**Why:** The project is entirely R-based; mixing Python would break the workflow consistency.

**How to apply:** Write R scripts or use Rscript in the terminal for any data reading, wrangling, or statistics. Never use Python pandas as an alternative.
