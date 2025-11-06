# Market Mapping & PCA — Smartphone Data Analysis

This project applies **Principal Component Analysis (PCA)** to visualize how smartphones are positioned in consumers’ minds based on screen size, hand size, and usage behavior.  
It extends the K-means segmentation analysis from the previous module and demonstrates **dimension reduction for market mapping** in R.

---

## Highlights
- **Tools:** R, tidyverse, ggplot2, ggrepel, ggfortify, corrplot  
- **Concepts:** Dimension reduction, customer perception mapping, correlation analysis  
- **Data:** Smartphone customer dataset (UCSD MGT 100)  

---

## Key Insights
- Screen size and hand size are highly correlated → PCA compresses both into a single “size preference” axis.
- Over time, the smartphone market shows **increasing differentiation** (wider spread of models).
- PCA on usage data reveals clusters of user behaviors (e.g., heavy gaming vs heavy reading).

---

## Sample Visual
*(include one plot export — your PCA map or correlation heatmap)*

![Market Map](market_map.png)
