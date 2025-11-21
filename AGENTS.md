# Build & Test
- Run all tests: `R -e "devtools::test()"`
- Run single test file: `R -e "testthat::test_file('tests/testthat/test-name.R')"`
- Load package: `R -e "devtools::load_all()"`
- Document: `R -e "devtools::document()"`

# Architecture
- **surveydatar**: R package for market research survey data processing/analysis.
- **Structure**: Standard R package (`R/` source, `tests/testthat/` tests).
- **Key Components**: Tabulation (`tab.R`), Weighting (`weighting.R`), Data Processing (`dataprocessingfunctions.R`).
- **Docs**: See `design_notes.md` for design philosophy and `todo.md` for features.

# Code Style & Conventions
- **Base R Preference**: Use Base R where possible; Tidyverse only when strongly justified (though existing code uses it).
- **Design**: Prioritize modularity, generalization, and readability over concision. Avoid defensive coding.
- **Errors**: Provide informative error messages.
- **Testing**: Use `testthat`. extensive coverage required (but wait for user request to add tests).
- **Process**: For complex tasks, ask questions -> propose design -> implement.
- **Naming**: Check before changing variable names.
- **Workflow**: See `.cursorrules` for detailed interaction guidelines.
