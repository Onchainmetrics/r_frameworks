# Report Generation Workflow

## 📋 Complete Process for Professional PDF Reports

### Step 1: Run Your Analysis
1. Execute `improved_sleuth_fixed.R` (insider clustering)
2. Execute `insider_activity_analysis.R` (behavioral analysis)
3. Execute `save_report_figures.R` (save all plots)

### Step 2: Generate Images
```r
# At the end of your R analysis, run:
source("save_report_figures.R")
```

**This creates:**
- `figures/supply_distribution.png` - Bar chart of insider vs retail control
- `figures/network_clustering.png` - Network graph with clusters
- `figures/behavior_patterns.png` - Trading behavior analysis
- `figures/top_clusters.png` - Top clusters by supply holdings  
- `figures/activity_heatmap.png` - Trading activity over time periods

### Step 3: Customize Report Template
1. Open `TROLL_Analysis_Report_Template.md`
2. Replace all `[BRACKETED_PLACEHOLDERS]` with your actual data
3. Update risk assessments and recommendations

### Step 4: Generate PDF

**Option A: Using Pandoc (Recommended)**
```bash
pandoc TROLL_Analysis_Report_Template.md -o TROLL_Report.pdf --pdf-engine=xelatex
```

**Option B: Using R Markdown**
1. Rename file to `.Rmd` extension
2. Open in RStudio
3. Click "Knit to PDF"

**Option C: Online Converter**
- Upload Markdown file to GitLab/GitHub (renders automatically)
- Use online tools like StackEdit → Export to PDF

### Step 5: Quality Check
- ✅ All figures display correctly
- ✅ Tables are properly formatted  
- ✅ Data placeholders are filled
- ✅ Professional appearance maintained

## 🎨 Figure Best Practices

### Image Requirements for Clean PDF:
- **Resolution**: 300 DPI minimum
- **Background**: White (not transparent)
- **Size**: 10x6 inches for standard plots
- **Format**: PNG (best for reports)

### Markdown Image Syntax:
```markdown
![Description](figures/filename.png)
*Figure X: Caption explaining what the chart shows*
```

## 📊 Data Sources Referenced

The template pulls from these CSV exports:
- `comprehensive_three_tier_analysis.csv`
- `behavioral_analysis_summary.csv`
- `top_clusters_analysis.csv`
- `individuals_vs_clusters_comparison.csv`

## 🔧 Troubleshooting

**Problem**: Images not showing in PDF
- **Solution**: Check file paths are correct, use relative paths

**Problem**: Tables not formatting properly  
- **Solution**: Ensure proper markdown table syntax with | separators

**Problem**: PDF generation fails
- **Solution**: Install required packages: `sudo apt-get install pandoc texlive-xetex`

**Problem**: Figures too large in PDF
- **Solution**: Adjust `width` and `height` parameters in `ggsave()`

## 🚀 Pro Tips

1. **Consistent Styling**: All figures use same color scheme and font sizes
2. **Caption Standards**: Every figure has descriptive caption explaining insights
3. **Professional Layout**: White backgrounds, clean axes, readable text
4. **Version Control**: Keep template and data separate for reusability
5. **Client Customization**: Easy to rebrand with different color schemes

---

**Result**: Professional, client-ready PDF report with embedded visualizations, executive summary, and actionable recommendations.