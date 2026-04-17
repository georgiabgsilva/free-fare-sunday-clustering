# free-fare-sunday-clustering

R pipeline for user profiling, k-means clustering, and pre/post transition analysis using anonymized transit validation data.

## overview

This project builds behavioral profiles from transaction-level data, applies k-means clustering to the post-period dataset, and analyzes transitions between pre and post periods.

The pipeline uses:

- DuckDB for data processing
- data.table for data manipulation
- ggplot2 for the elbow plot
- networkD3 for the Sankey diagram

## repository structure

```text
.
├─ README.md
├─ .gitignore
├─ config/
│  └─ example.env
├─ scripts/
│  └─ script_free_fare_clustering.R
├─ data/
│  └─ .gitkeep
└─ outputs/
   └─ .gitkeep
