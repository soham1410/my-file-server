###### **1. Implement Descriptive Statistics**

DATASET:-https://www.kaggle.com/datasets/ehababoelnaga/anemia-types-classification



**EXCEL** 

Measures of Central Tendency

Mean: =AVERAGE(A2:A1282)

Median: =MEDIAN(A2:A1282)

Mode: =MODE.SNGL(A2:A1282)



Measures of Dispersion (Variability)

Standard Deviation: =STDEV.S(A2:A1282)

Variance: =VAR.S(A2:A1282)

Range: =MAX(A2:A1282)-MIN(A2:A1282)

Interquartile Range (IQR): =QUARTILE.INC(A2:A1282, 3)-QUARTILE.INC(A2:A1282, 1)



**TABLEAU**

Phase 1: Load the Dataset

1\. Open Tableau and connect to Text file.

2\. Open diagnosed\_cbc\_data\_v4.csv and go to Sheet 1.



Phase 2: Disaggregate the Data (Crucial Step)

1\. Go to the top menu bar and click "Analysis".

2\. Uncheck "Aggregate Measures" to plot individual patient data points instead of sums.



Phase 3: Build Goal 1 (Detailed View - Individual Axes)

1\. Drag "Measure Names" to the Rows shelf.

2\. Drag "Measure Values" to the Columns shelf.

3\. Use the Filter card to keep only blood parameters (WBC, RBC, HGB, PLT, etc.).



Phase 4: Build Goal 2 (Magnitude View - Shared Axis)

1\. Create a New Worksheet.

2\. Ensure "Analysis" > "Aggregate Measures" is still unchecked.

3\. Drag "Measure Names" to the Columns shelf.

4\. Drag "Measure Values" to the Rows shelf.

5\. Filter for blood parameters only.



Phase 5: Formatting (Apply to both sheets)

1\. In the Marks card, change the dropdown from "Automatic" to "Circle".

2\. Click "Color" in the Marks card and reduce Opacity to 50% to show data density.

3\. Drag "Measure Names" onto the Color box to assign different colors to each parameter.



###### **2. Implement Inferential Statistic**

DATASET:-https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset

**Excel** 



Part A: Two-Sample T-Test (Unequal Variances)



1\. Go to Data → Data Analysis.

2\. Select t-Test: Two-Sample Assuming Unequal Variances.

3\. Select:

&#x20;  - Variable 1 Range → First dataset column

&#x20;  - Variable 2 Range → Second dataset column

4\. Set:

&#x20;  - Hypothesized Mean Difference = 0

&#x20;  - Alpha = 0.05

&#x20;  - Check Labels (if headers exist)

5\. Choose New Worksheet Ply and click OK.



Part B: Chi-Square Goodness-of-Fit Test



1\. Create columns: BMI Category, Observed, Expected, (O−E)²/E.



2\. Observed Values:

&#x20;  =COUNTIF(data\_range, category)



3\. Expected Values:

&#x20;  =SUM(observed\_range) / number\_of\_categories



4\. Chi-Square Calculation:

&#x20;  =(Observed - Expected)^2 / Expected



5\. Final Results:

&#x20;  - Chi-Square Statistic:

&#x20;    =SUM(calculated\_values)

&#x20;  - P-Value:

&#x20;    =CHISQ.TEST(observed\_range, expected\_range)

###### **3. Implement Data Cleaning Techniques Excel/R**

DATASET:-https://www.kaggle.com/datasets/ahmedmohamed2003/retail-store-sales-dirty-for-data-cleaning

**EXCEL (Measures of Central Tendency)**

1\. Open the .csv file.

2\. Numerical (Mean): Fill empty 'Price Per Unit', 'Quantity', and 'Total Spent' cells using: 

&#x20;  	Formula to find mean of Price Per Unit

&#x09;	=IF(ISBLANK(E2), AVERAGE(E:E), E2)

&#x09;Formula to find mean of Quantity

&#x09;	=IF(ISBLANK(F2), AVERAGE(F:F), F2)

&#x09;Formula to find mean of Total Spent

&#x09;	=AVERAGE(G2:G12576)

&#x09;Formula to find mode of Item

&#x09;	=IF(ISBLANK(D2),INDEX($D$2:$D$12576,MODE(IF($D$2:$D$12576<>"",MATCH($D$2:$D$12576, $D$2:$D$12576, 0)))), D2)

&#x09;Formula to find mode of Discount Applied

&#x09;=IF(ISBLANK(K2),INDEX($K$2:$K$12576,MODE(IF($K$2:$K$12576<>"",MATCH($K$2:$K$12576, $K$2:$K$12576, 0)))), K2)

3\. Categorical (Mode): Fill empty 'Item' and 'Discount Applied' cells using:

&#x20;  =INDEX(A:A, MATCH(MAX(COUNTIF(A:A, A:A)), COUNTIF(A:A, A:A), 0))

4\. Save as "retail\_sales\_excel\_cleaned.csv".



**RSTUDIO (Categorical Relationships \& Algebra)**

1\. Set Working Directory to your data folder.

2\. Run the R script provided below.

CODE:

\# 1. Load the package (It is already installed now!)

library(dplyr)



\# 2. Load the dataset

df <- read.csv("retail\_store\_sales.csv", check.names = FALSE, stringsAsFactors = FALSE)



\# 3. Destroy hidden spaces: convert all blanks and "NaN" text to true NA

df\[df == "" | df == " " | df == "NaN" | df == "null"] <- NA



\# 4. Custom Mode function for exact lookups

get\_mode <- function(v) {

&#x20; v\_clean <- na.omit(v)

&#x20; if(length(v\_clean) == 0) return(NA)

&#x20; uniqv <- unique(v\_clean)

&#x20; uniqv\[which.max(tabulate(match(v\_clean, uniqv)))]

}



\# 5. The Deterministic (Algebraic) Cleaning Pipeline

df\_cleaned <- df %>%

&#x20; 

&#x20; # --- FIXING TEXT ---

&#x20; group\_by(Category) %>%

&#x20; mutate(Item = ifelse(is.na(Item), get\_mode(Item), Item)) %>%

&#x20; ungroup() %>%

&#x20; 

&#x20; group\_by(Location) %>%

&#x20; mutate(`Discount Applied` = ifelse(is.na(`Discount Applied`), get\_mode(`Discount Applied`), `Discount Applied`)) %>%

&#x20; ungroup() %>%

&#x20; 

&#x20; # --- FIXING NUMBERS: EXACT LOOKUPS \& ALGEBRA ---

&#x20; group\_by(Item) %>%

&#x20; mutate(`Price Per Unit` = ifelse(is.na(`Price Per Unit`), get\_mode(`Price Per Unit`), `Price Per Unit`)) %>%

&#x20; ungroup() %>%

&#x20; 

&#x20; mutate(

&#x20;   Quantity = ifelse(

&#x20;     is.na(Quantity) \& !is.na(`Total Spent`) \& !is.na(`Price Per Unit`), 

&#x20;     round(`Total Spent` / `Price Per Unit`), 

&#x20;     Quantity

&#x20;   ),

&#x20;   `Total Spent` = ifelse(

&#x20;     is.na(`Total Spent`) \& !is.na(Quantity) \& !is.na(`Price Per Unit`), 

&#x20;     `Price Per Unit` \* Quantity, 

&#x20;     `Total Spent`

&#x20;   )

&#x20; )



\# 6. Save the perfectly calculated dataset

write.csv(df\_cleaned, "retail\_sales\_algebraic\_cleaned.csv", row.names = FALSE)



\# 7. Print Success Message

print("SUCCESS: Data cleaned using exact lookups and algebra! Saved as retail\_sales\_algebraic\_cleaned.csv")



3\. This script uses grouping to fill text based on Category/Location and algebra to solve for missing numbers.

4\. Verify the output file "retail\_sales\_rstudio\_cleaned.csv".



###### **4.** 

###### **5.** 

###### **6.** 

###### **7. Implement Time Forecasting Any One Method** 

DATASET:-https://www.kaggle.com/code/georgyzubkov/daily-climate-in-delhi-arima-with-timeseries

**Excel**

STEP 1:Ensure your data has two columns: one for Dates/Times (with consistent intervals) and one for the corresponding numerical values.

STEP 2:Select both columns.

STEP 3:Go to the Data tab on the ribbon.

STEP 4:Click on Forecast Sheet (in the Forecast group).

STEP 5:A preview will appear showing the historical data and the forecasted trend with confidence intervals. You can adjust the "Forecast End" date.

STEP 6:Click Create. Excel will generate a new worksheet with a chart and the calculated forecast values.



###### **8. Implement Karl Pearson Correlation**

DATASET:-https://www.kaggle.com/datasets/ehababoelnaga/anemia-types-classification

&#x09;=PEARSON(A2:A1282, F2:F1282)

###### **9. Implement Kurtosis**

DATASET:-https://www.kaggle.com/datasets/ehababoelnaga/anemia-types-classification

&#x09;=KURT(A2:A1282)

###### **10.Implement Outlier Detection Using Statistics**

DATASET:- https://www.kaggle.com/code/mohaiminul101/video-game-sales

**EXCEL**

Implementation steps for Outlier Detection (Z-Score Method):

1\. Calculate the Mean: 

&#x09;In an empty cell, calculate the average of your target data.

&#x09;Formula: =AVERAGE(\[Target Data Range])

&#x09;\* Let's refer to this cell as \[Mean Cell].



2\. Calculate the Standard Deviation: 

&#x09;In another empty cell, calculate the sample standard deviation.

&#x09;Formula: =STDEV.S(\[Target Data Range])

&#x09;\* Let's refer to this cell as \[StdDev Cell].



3\. Calculate Z-Scores: 

&#x09;Create a new column next to your data called "Z-Score". For the first data point, enter the formula below, ensuring you lock the Mean and StdDev cells with dollar signs ($) for 	absolute referencing.

&#x09;Formula: =(\[First Data Cell] - $\[Mean Cell]) / $\[StdDev Cell]

&#x09;\*Drag this formula down to fill all rows.



4\. Flag Outliers: 

&#x09;Create another new column called "Outlier Status". For the first data point, enter the formula below to automatically flag anomalies based on a threshold of 3 standard deviations.

&#x09;Formula: =IF(ABS(\[First Z-Score Cell])>3, "Outlier", "Normal")

&#x09;\* Drag this formula down to fill all rows. 

&#x09;\* Use the filter tool on this column to view only the "Outlier" rows.

###### **11.Implement Bowley Coefficient Of Skewness**

DATASET:-https://www.kaggle.com/datasets/ehababoelnaga/anemia-types-classification

=(QUARTILE.INC(A2:A1282, 3) + QUARTILE.INC(A2:A1282, 1) - 2\*QUARTILE.INC(A2:A1282, 2)) / (QUARTILE.INC(A2:A1282, 3) - QUARTILE.INC(A2:A1282, 1))

###### **12.Implement Karl Pearson’s Coefficient Of Skewness**

DATASET:-https://www.kaggle.com/datasets/ehababoelnaga/anemia-types-classification

=3\*(AVERAGE(A2:A1282) - MEDIAN(A2:A1282)) / STDEV.S(A2:A1282)

###### **13.Implement Spearman Correlation Coefficient**

DATASET:-https://www.kaggle.com/datasets/ehababoelnaga/anemia-types-classification

Step 1: Rank the first variable (WBC)

&#x09;=RANK.AVG(A2, $A$2:$A$1282, 1) \*A=any empty column

Step 2: Rank the second variable (RBC)

&#x09;=RANK.AVG(F2, $F$2:$F$1282, 1) \*F= another empty column

Step 3: Calculate the Correlation of the Ranks

&#x09;=CORREL(P2:P1282, Q2:Q1282)

###### **14.Implement Smote Method-Online MATLAB**

MATLAB CODE:

% ---------------- LOAD DATA ----------------

data = readtable('WA\_Fn-UseC\_-Telco-Customer-Churn.csv');



% ---------------- REMOVE ID COLUMN ----------------

if ismember('customerID', data.Properties.VariableNames)

&#x20;   data.customerID = \[];

end



% ---------------- FIX TotalCharges ----------------

if ismember('TotalCharges', data.Properties.VariableNames)

&#x20;   if iscell(data.TotalCharges) || isstring(data.TotalCharges)

&#x20;       data.TotalCharges = string(data.TotalCharges);

&#x20;       data.TotalCharges(data.TotalCharges == " " | data.TotalCharges == "") = "0";

&#x20;       data.TotalCharges = str2double(data.TotalCharges);

&#x20;   end

end



% Remove missing values

data = rmmissing(data);



% ---------------- ENCODE CATEGORICAL VARIABLES ----------------

for i = 1:width(data)

&#x20;   if \~isnumeric(data.(i))

&#x20;       data.(i) = grp2idx(categorical(data.(i)));

&#x20;   end

end



% ---------------- SEPARATE FEATURES \& TARGET ----------------

X = data{:,1:end-1};

y = data{:,end};



% ---------------- DISPLAY ORIGINAL DISTRIBUTION ----------------

disp("Before SMOTE:");

tabulate(y)



% ---------------- SMOTE FUNCTION ----------------

function \[X\_new, y\_new] = simple\_smote(X, y, k)



&#x20;   % Find minority class

&#x20;   classes = unique(y);

&#x20;   counts = histc(y, classes);

&#x20;   \[\~, idx] = min(counts);

&#x20;   minority\_class = classes(idx);



&#x20;   majority\_class = classes(classes \~= minority\_class);



&#x20;   X\_min = X(y == minority\_class, :);

&#x20;   X\_maj = X(y == majority\_class, :);



&#x20;   n\_min = size(X\_min,1);

&#x20;   n\_maj = size(X\_maj,1);



&#x20;   % Number of synthetic samples needed

&#x20;   n\_to\_generate = n\_maj - n\_min;



&#x20;   % Find neighbors

&#x20;   neighbors = knnsearch(X\_min, X\_min, 'K', k+1);



&#x20;   synthetic = \[];



&#x20;   % Generate only required samples

&#x20;   for i = 1:n\_to\_generate

&#x20;       idx\_i = randi(n\_min);

&#x20;       nn = neighbors(idx\_i, randi(\[2 k+1]));



&#x20;       diff = X\_min(nn,:) - X\_min(idx\_i,:);

&#x20;       gap = rand(1, size(X,2));



&#x20;       synthetic\_point = X\_min(idx\_i,:) + gap .\* diff;

&#x20;       synthetic = \[synthetic; synthetic\_point];

&#x20;   end



&#x20;   % Combine

&#x20;   X\_new = \[X; synthetic];

&#x20;   y\_new = \[y; repmat(minority\_class, size(synthetic,1),1)];

end

% ---------------- APPLY SMOTE ----------------

\[X\_sm, y\_sm] = simple\_smote(X, y, 5);



% ---------------- DISPLAY AFTER SMOTE ----------------

disp("After SMOTE:");

tabulate(y\_sm)



% ---------------- PLOTS ----------------



% Adjust indices if needed based on dataset

tenure = X(:,1);

monthly = X(:,end);



figure;

gscatter(tenure, monthly, y);

title('Before SMOTE');

xlabel('Tenure');

ylabel('Monthly Charges');



figure;

tenure\_sm = X\_sm(:,1);

monthly\_sm = X\_sm(:,end);



gscatter(tenure\_sm, monthly\_sm, y\_sm);

title('After SMOTE');

xlabel('Tenure');

ylabel('Monthly Charges');



###### **15.Linear Regression \& Performance Evaluation**

**WEKA**

1.Load Data: Open Weka Explorer and click Open file... to load your dataset (CSV or ARFF format).

2\. Select the Model: Navigate to the Classify tab.

3\. Click the Choose button under the Classifier section.

4\. Expand the functions folder and select LinearRegression.

5\. Set Target Variable: Just above the "Start" button, use the dropdown menu to select your target (dependent) variable.

6.Click Start.

7.Evaluate Performance: Look at the "Classifier output" panel on the right. Weka will display the linear regression equation and the crucial evaluation metrics, including:

8.Correlation coefficient (R): Measures how strong the relationship is (closer to 1 or -1 is better).

9.Mean absolute error (MAE): The average absolute difference between predicted and actual values.

10.Root mean squared error (RMSE): Similar to MAE but penalizes larger errors more heavily.



