# Pregnancy Immunology Dashboard

Interactive analysis of flow cytometry data from pregnancy immunology research at the Kedzierska Lab, University of Melbourne.

## Research Overview

This dashboard provides comprehensive analysis of immune cell populations and gene expression patterns across different pregnancy-related groups, focusing on:

- **84 samples** across 4 donor groups (Pregnant, Non-pregnant, Decidua, Cord)
- **339 gene/protein markers** measured via flow cytometry
- **6 immune cell types**: B cells, CD4 T cells, CD8 T cells, Monocytes, NK cells, γδ T cells

## Dashboard Features

### Interactive Visualizations
- **Dynamic boxplots** showing gene expression levels
- **Multi-dimensional filtering** by cell types
- **Real-time plot updates** based on selections
- **Sample size annotations** for statistical context

### Search & Filter Capabilities
- **Searchable gene dropdown** (339 markers available)
- **Cell type checkboxes** for selective analysis
- **Responsive visualizations** with Bootstrap styling

## Quick Start

### Interactive Web Application (Shinylive)
Visit the live dashboard: [https://shihanl.github.io/Habel_NK/](https://shihanl.github.io/Habel_NK/)

**No installation required** - runs entirely in your browser!

### Local Development Options

#### Option A: Shiny (R) - Recommended
```r
# Install R packages
install.packages(c("shiny", "shinydashboard", "plotly", "dplyr", "readr"))

# Run the dashboard
shiny::runApp("app.R")
```

#### Option B: Dash (Python) - Legacy
```bash
# Install Python dependencies
pip install -r requirements.txt

# Run the dashboard
cd src && python app.py

# Open browser to http://127.0.0.1:8050/
```

## Project Structure

```
dashboard/
├── src/                     # Main application code
│   ├── app.py              # Dashboard entry point
│   ├── components/         # UI components
│   │   ├── filters.py      # Gene/cell type filters
│   │   ├── plots.py        # Boxplot visualizations
│   │   ├── layout.py       # Page layout
│   │   └── callbacks.py    # Interactive functionality
│   └── utils/              # Data processing utilities
├── data/                   # Flow cytometry dataset
├── requirements.txt        # Python dependencies
└── index.html             # GitHub Pages landing page
```

## Data Information

### Dataset Details
- **Expression values**: Percentage of cells expressing each marker
- **Sample groups**: Pregnant vs. Non-pregnant vs. Decidual tissue
- **Cell populations**: 6 major immune cell types
- **Measurement technique**: Flow cytometry analysis

### Available Analyses
- Cross-group expression comparisons
- Cell-type specific patterns
- Multi-dimensional data exploration
- Statistical visualization with sample sizes

## Technical Stack

- **Backend**: Python, Dash, Plotly
- **Frontend**: Bootstrap, HTML5, CSS3
- **Data**: Pandas, NumPy
- **Deployment**: GitHub Pages, GitHub Actions

## Development

### Data Processing
- `data_cleaning.py` - Standardize donor identifiers
- `preview_columns.py` - Preview column name changes
- Built-in utilities for cell type filtering and data preparation

### Adding Features
1. Extend `components/` modules for new UI elements
2. Update `utils/data_processing.py` for data transformations
3. Modify `components/callbacks.py` for interactivity

## Research Context

This dashboard supports ongoing research into:
- Pregnancy-induced immune system changes
- Tissue-specific immune cell characteristics
- Maternal-fetal immune interactions
- Flow cytometry data analysis workflows

## Contributing

For research collaborations or technical contributions, please contact the Kedzierska Lab or submit issues/PRs to this repository.

## License

Research data and code for academic use. Please cite appropriately if using this work.

---

**Kedzierska Lab** | University of Melbourne | Pregnancy Immunology Research