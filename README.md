# Path of Exile: Analysis of Currency Value Fluctuations

This repository contains the final project from the IST 707 Applied Machine Learning course. The project applies data mining techniques to analyze and model the fluctuating values of in-game currencies in Path of Exile (PoE), drawing parallels with real-world economic principles.

## Project Overview
Path of Exile features a complex, player-driven economy. This project aims to analyze and model the fluctuating values of in-game currencies, providing insights into the economic system and drawing parallels with real-world economic principles.

### Objectives
- **Analyze** the economic behavior in PoE’s currency market.
- **Model** the fluctuating values using machine learning techniques.
- **Predict** future currency values to provide strategic insights.
- **Visualize** key trends and patterns in the game’s economy.

## Skills Demonstrated
- **Data Preprocessing**: Cleaning and preparing data for analysis.
- **Machine Learning**: Implementing algorithms such as K-Means, Naïve Bayes, and Random Forest.
- **Data Visualization**: Creating insightful visualizations to highlight trends.
- **Statistical Analysis**: Conducting correlation and clustering analyses.

## Dataset
The dataset, sourced from poe.ninja, includes transactions from the "Crucible" league, with key variables:
- **League**: The specific Path of Exile game league.
- **Date**: Transaction date.
- **Get**: Currency or item received.
- **Pay**: Currency or item used for payment.
- **Value**: Exchange rate or value.
- **Confidence**: Reliability of the transaction record.

## Analysis
### Data Exploration and Preprocessing
![image](https://github.com/artsong4/Path-of-Exile-Currency-Analysis/assets/125407614/5b896f14-97ed-4c2f-9ed7-df56bfb45ab6)

- **League**: All transactions from "Crucible".
- **Date Range**: Data spans from 2023-04-07 to 2023-08-15.
- **Transactions**: 'Get' and 'Pay' variables detail items or currencies exchanged.
- **Value**: Ranges from 0 to 128,002, with a median of 1.00.
- **Confidence**: Indicates reliability of transaction data.

### Outlier Detection
An adapted Interquartile Range (IQR) method with a multiplier of 40 was used to identify significant outliers such as "Mirror Shard" and "Mirror of Kalandra". These outliers were removed to focus on typical transactions and gain deeper insights into common trading patterns.
![image](https://github.com/artsong4/Path-of-Exile-Currency-Analysis/assets/125407614/a69aa906-f592-4541-8796-420a83c9ffba)

![image](https://github.com/artsong4/Path-of-Exile-Currency-Analysis/assets/125407614/ef2d4edf-f535-40bb-9a21-18f6d221e9d7)

### Correlation Analysis
A correlation heatmap was created to visualize the relationships between the top 30 fluctuating currencies. This analysis highlighted significant volatility and revealed which currencies were most influenced by in-game events.

![image](https://github.com/artsong4/Path-of-Exile-Currency-Analysis/assets/125407614/ea61c765-8cb8-47fc-8ed8-d3551acc521c)
![image](https://github.com/artsong4/Path-of-Exile-Currency-Analysis/assets/125407614/ea27921d-1d2f-42fe-a9d4-ccddd7a137e5)

### Clustering
The Elbow Method was used to determine the optimal number of clusters for K-Means clustering, resulting in three distinct clusters of transactions:
- **Cluster 1**: High-value, rare transactions.
- **Cluster 2**: Frequent, low-value transactions.
- **Cluster 3**: Medium-value transactions.
![image](https://github.com/artsong4/Path-of-Exile-Currency-Analysis/assets/125407614/ddd080ff-a609-42c1-9270-368bcbbc3d4a)
![image](https://github.com/artsong4/Path-of-Exile-Currency-Analysis/assets/125407614/e905e5b0-95d7-4fab-a7bd-acd82ede1e9d)

### Classification
#### Naïve Bayes
- Used K-Means clustering to categorize the 'Value' variable into three groups.
- High accuracy in predicting value categories with a confidence interval of 97.4% to 98.04%.
![image](https://github.com/artsong4/Path-of-Exile-Currency-Analysis/assets/125407614/5dfa5f4d-54fc-4d8b-ab38-ff3b8e2ac787)
![image](https://github.com/artsong4/Path-of-Exile-Currency-Analysis/assets/125407614/ec3cf9e3-8ecd-4acb-8d50-2e6a78491d23)
![image](https://github.com/artsong4/Path-of-Exile-Currency-Analysis/assets/125407614/219109bd-0fed-4409-8a59-8ff013efc8de)

#### Random Forest
- Pre-tuned model with ntree = 50 achieved an accuracy of 99.75%.
![image](https://github.com/artsong4/Path-of-Exile-Currency-Analysis/assets/125407614/0f7af9fb-ea6e-4647-85e7-e1a5b9e2ee78)

- Post-tuned model with optimal mtry = 12 and 5-fold cross-validation improved accuracy to 99.85%.
![image](https://github.com/artsong4/Path-of-Exile-Currency-Analysis/assets/125407614/f8cf59bc-36d6-4d1b-b374-266eca3c32ee)

## Key Findings
- **Market Dynamics**: PoE’s economy mirrors real-world principles such as supply and demand, item flipping, and price setting.
- **Player Behavior**: Game mechanics and community consensus significantly influence market trends.
- **Economic Stratification**: Identified clusters of high and low-value transactions, indicating a diverse economy similar to real-world markets.
- **Predictive Insights**: Achieved high accuracy in predicting currency value categories, providing strategic insights for players and developers.

## Implications
- **Players**: Equip players with strategic insights to enhance their gaming experience.
- **Game Developers**: Provide valuable feedback for understanding and balancing the game economy.
- **Company**: Assist Grinding Gear Games in maintaining a sustainable and engaging in-game economy.

## Repository Structure
```plaintext
Path-of-Exile-Currency-Analysis/
├── data/
│   ├── Currency.xlsx
├── PoE_Currency_Analysis.R
├── README.md
├── LICENSE

