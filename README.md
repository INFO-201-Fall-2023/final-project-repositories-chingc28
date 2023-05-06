# final-project-repositories-chingc28
final-project-repositories-chingc28 created by GitHub Classroom

# Story

### Introduction:

With continued greenhouse emissions, the effects of global warming induced climate change can be exhibited all over the world. The impacts of climate change are vast and complex, it is difficult to capture the true extent of its story when the effects can range from crop availability to sex-changing reptiles can be explored. It is well known that climate change can change weather and frequency of natural disasters. Natural disasters such as hurricanes, earthquakes, and wildfires have the potential to cause significant economic damage. The impact of natural disasters on gross domestic product (GDP) can be substantial, as they can disrupt economic activity and infrastructure, causing production and supply chain disruptions. In this project, we aim to investigate the relationship between natural disasters and GDP of each state in the U.S.A from 1997 to 2022

### Our objectives are to:

- Analyze the correlation between wine prices and their country of origin.
- Examine the relationship between wine prices and vintage ratings.
- Investigate the impact of wine ratings on wine prices.
- Provide insights and recommendations for wine enthusiasts and investors.

### Areas of analysis: 
- GDP change from 1997 to 2022 in the US
- The impact of natural disasters on GDP?
- The frequency and intensity of natural disasters changing over time
- Examining whether there is a relationship between the frequency and intensity of natural disasters and changes in GDP over time

### Methodology:
To achieve the project objectives, R will be used to perform exploratory data analysis on the dataset and visualizations will be created to make the results easy to interpret. Regression analysis will be performed controlling for inflation rates and unemployment rates.

### Conclusion:
We expect to find that natural disasters have a significant impact on GDP, particularly in the short term. We also expect to find that the frequency and intensity of natural disasters have increased over time, possibly due to climate change. The results of this study will contribute to the existing literature on the economic impact of natural disasters and can provide insights on how to mitigate their negative effects. The findings can also inform policymakers on the importance of disaster preparedness and response planning, and can contribute to the development of more effective policies to reduce the impact of natural disasters on the economy.

# Data
Dataset 1: [U.S Bureau of Economic Analysis (BEA)] (https://apps.bea.gov/regional/downloadzip.cfm)

- How was it collected or generated? It was collected by the government
- Total **rows** in data: 484 rows
- Total **columns** in data: 34 columns

Dataset 2: [US Natural Disasters (Kaggle)] (https://www.kaggle.com/datasets/headsortails/us-natural-disaster-declarations)
- How was it collected? User on Kaggle got it from the FEMA website and made a few cleaning and formatting adjustments 
- Total **rows** in data: 64,092 rows 
- Total **columns** in data: 23 columns

# Background Research/Inspiration: 

[Economic Recovery after Natural Disasters](https://www.un.org/en/chronicle/article/economic-recovery-after-natural-disasters)
+ "The destructive effects of natural disasters are felt more in poorer countries than in more prosperous ones" This is applicable to states that are less economically propsperous than states with high GDP 
- It talks about how natural disasters impact education, nutrition, and employment. There can be a possibility to do additional analysis with school enrollment and employment rates 

[How can climate change affect natural disasters?](https://www.usgs.gov/faqs/how-can-climate-change-affect-natural-disasters)
+ "As more water vapor is evaporated into the atmosphere it becomes fuel for more powerful storms to develop" This opens possibility to conduct analysis with carbon emissions and water vapor in the air, however it would slightly deviate from economic analysis
- "More heat in the atmosphere and warmer ocean surface temperatures can lead to increased wind speeds in tropical storms". Can include frequency of tropical storms in different state and how the frequency increased over the years 

[Economic effects of natural disasters](https://wol.iza.org/articles/economic-effects-of-natural-disasters/long)
+ "Longer-term economic impacts are more complex and depend on the characteristics of the affected population and the affected area, changes in migration patterns, and public policy" Would be interesting to analyze public policy and its effect on economy and migration policys. However, would be hard to fit into the data scope 
- "More research is needed to understand the long-term effects of natural disasters on individuals, which could differ from their long-term effects on affected areas due to post-disaster migration" Natural disaster and its economic impact is complex and is hard to investigate within a single dataframe 

[Hurricanes](https://climatecenter.fsu.edu/topics/hurricanes)
- "Approximately 90% of all deaths in hurricanes worldwide are caused by drowning in either the storm surges or flooding caused by intense rainfall" Could include deaths in our dataset, but that would deviate from the economic aspects of this analysis 
- This includes hurricane speeds which is irrelevant in this analysis 

[How Climate Change May Affect the U.S. Economy](https://crsreports.congress.gov/product/pdf/R/R47063)
- This is a really detailed report with various methodologies and economic sectors and long/short term effects. Very comprehensive
- "Additionally, there is no consensus on the best way to model the economic effects of climate change" This is a con
