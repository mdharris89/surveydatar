
Surveydatar provides a suite of tools designed to streamline the processing, management and analysis of survey data in R.

The core of the package revolves around the survey_data object and the associated metadata dictionary (dpdict). Functions are provided to automatically infer metadata from data structure and labels (create_dict_with_metadata()) and view metadata (datamap()), alongside various functions to make wrangling survey data faster and easier, including weighting.

Tab then builds on the dpdict as a foundation to provide a single and intuitive but highly flexible function to cover most crosstab needs for a market research context. Key features include:

- Composable in-line expressions: define complex row and column definitions in-line rather than needing to pre-process the data, enabling faster, frictionless analysis
- Extendable, modular design: ‘helpers’ make common row and column expressions like ‘top box’ simple to use, while ‘statistics’ determine how data is aggregated at row and column intersections in the table. New helpers and statistics can be easily created and plugged in, making tab extendable to any functionality that fits within the framework of array-based calculations followed by aggregation.
- Fuzzy variable matching and question group expansion: easily navigate around large datasets without needing to remember the exact variable name in every case. Tab resolves variable specification based on fuzzy matching to names or labels, and entire question groups can be easily added to a tab with a single reference.
- Convenient clipboard export for pasting into sheets or charts
- Convert tab results directly into Flourish-ready format with tab_to_flourish() for fast, beautiful visualisation within R

The package is still in development with some common survey data processing needs planned but not yet implemented, e.g. stacking, merging.
