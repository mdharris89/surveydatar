This package provides a suite of tools designed to streamline the processing and management of complex survey data in R. Survey data often involves dealing with complex metadata and question structures. This package aims to simplify tasks such as:

-   Viewing and understanding data structure
-   Creating and enriching metadata dictionaries (`dpdict`)
-   Standardizing separator patterns in names and labels
-   Automatically grouping variables into logical questions
-   Updating survey data based on metadata changes
-   Validating the consistency of data and metadata

The core of the package revolves around the `survey_data` object and the associated metadata dictionary (`dpdict`).

This package is still in active development but is fully functional as an MVP. Core features are working, but you may encounter some bugs or incomplete documentation. Feedback and contributions are welcome.

Other functions in-development but not yet committed include functionality for:
- Restructuring grid-style questions
- Converting between long and wide format
- Stacking
- Creating common derived quantitative variables (e.g. top 2 box, numeric bands)
- Creating quantitative variables from open-ended text responses
- Merging datasets including harmonisation of names and labels
