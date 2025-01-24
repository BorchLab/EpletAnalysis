# Eplet Analysis

Code repository for the analysis of commerical assays for HLA allele and eplet coverage.

## Reproducible Code

Available as a [pdf](https://github.com/BorchLab/EpletAnalysis/blob/main/EpletWorkflow.pdf) or via the [markdown](https://github.com/BorchLab/EpletAnalysis/blob/main/EpletWorkflow.qmd).

The most important step before running the analysis is to ensure that the data is consistently formatted. Much of the functions are specific to the column names 
and structure of the individual cells within the excel file or the document.

## Folder Structure
```
├── data                 collection of tables for assays and eplets
├── EpletWorkflow_files/ Generated files for the rendered `EpletWorkflow.qmd` document
├── EpletWorkflow.qmd    Quarto markdown source for the main workflow
├── EpletWorkflow.html   Rendered HTML output of the main workflow
├── outputs/             Table and visualizations
├── R/                   Custom R functions to perform the analysis
├── SupplMaterial.qmd    Supplementary material source in Quarto markdown
└── README.md            Description and setup instructions for the repository
```

## Citation

Will be updated after publication.

## Questions

If you run into any issues or need help modifying the code to your data, please submit a [GitHub issue](https://github.com/BorchLab/EpletAnalysis/issues) with details and the output of `sessionInfo()`.
