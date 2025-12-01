# BankingPrediction
This project conducts an analysis over a **Bank Marketing dataset**, which compiles call information from a Portuguese banking institution. The main objective of the analysis is predicting **whether a client will subscribe to a term deposit**. This classification task relies on various categorical features, such as the client’s education level, occupation, and marital status. Additionally, numerical features like the client’s age and average account balance are considered in the analysis. This was a semester-long project for the course Multivariate Analysis using **R programming language**. It was conducted in a group of five students with proper task assignments with a Gannt diagram, including risk management. The final report is included along with the code, data and additional files. A summary of the project is shown here:

1. Preprocessing included the following steps. First step was handling missing values using different methods including KNN, MICE and MIMMI. Second step was the outlier detection including univariate and multivariate outliers using boxplots, Mahalanobis distance Q-Q plot and Local Outlier Factor.
2. Descriptive analysis included ploting numerical factors individually but also pairwise to detect any correlation. For categorical factors, each category was plotted against the target variable to try and detect any patterns.
3. PCA analysis has provided us with the insight that the clients with more previous contact and longer calls are more likely to subscribe to a term deposit. Also clients are more likely to subscribe in the winter months.
4. Multiple Correspondence analysis showed us that the people more likely to subscribe are of higher education, without house loans, with jobs either unknown or still students instead of blue-collar or services, not by the end of spring and that already said yes to the previous campaign.
5. Analysis with Association Rules was done using the apriori and ECLAT methods. Both have not given confident answers regarding the positive outcome. However, we can say with high confidence that having a loan is the most significant variable predicting negative outcome. We also see that for the months of May, June, July and may also have a tendency of y=no.
6. Hierarchical clustering gave us the following 5 clusters, which were subsequently profiled:
    1. Cluster: Married professionals
    2. Cluster: Clients with distinctive financial behaviour
    3. Cluster: Age-specific demographic group
    4. Cluster: Higher-education clients
    5. Cluster: Clients characterized by loan-related behaviours
    Statistical tests (ANOVA, Kruskal-Wallis, Chi-squared) confirmed significant variation across clusters for both numerical and categorical variables.
7. Decision Trees has given us an accuracy of 80% on the test set.
8. Discriminant analysis has given us an accuracy of 76%, showing non-linearity in the variable relationships.
