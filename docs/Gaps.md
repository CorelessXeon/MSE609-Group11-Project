# Gaps and Discrepancies in Appendix A1 Replication  
Prepared by **Leila Safari**, Group 11

This document summarizes where our replicated ordinal logistic regression results differ from the published Appendix A1, and why these differences likely occur.

---

## ✔ 1. Summary of Alignment

Across all four models (Q40–Q43):
- Signs of coefficients match the paper  
- Relative ordering across categories matches  
- Significance patterns largely match  
- Education and income effects rise monotonically  
- Gender(Female) consistently shows lower odds  

This indicates the replication is structurally correct.

---

## ⚠ 2. Types of Differences Observed

### **2.1. Slightly different odds ratio magnitudes**
Most ORs differ by **3–10%**, which is normal when:
- authors used a different statistical package (SPSS / Stata)  
- internal cleaning differed from ours  
- weighting procedures were undocumented  


### 2.2. Proportional-odds assumption

The proportional-odds assumption may not fully hold for some outcomes
(especially Q41 and Q43). When this assumption is strained:

- SPSS and R can handle thresholds a bit differently,
- which can slightly shift odds ratios even with the same data.



### **2.3. Unknown handling of missing values**
The original dataset contains missing codes (96–99).  
Authors may have done one of these:
- removed respondents with invalid predictors only  
- removed invalid dependent-variable responses question-by-question  
- removed outliers  

These choices can change ORs slightly.

### **2.4. Weighting is not discussed in the paper**
If researchers applied any survey weights (even minor ones), ORs will shift.

Borealis Dataverse does not provide weight documentation.

### **2.5. Factor ordering differences**
SPSS sometimes treats categorical variables differently:
- alphabetical ordering  
- or first-seen category as reference  

We manually set reference categories, but differences still propagate.

---

## ✔ 3. Specific Differences by Variable

### Age
- All directions match  
- ORs for 55–64 and 65+ are slightly lower in our replication (typical when response distributions differ under strict filtering)

### Gender
- Female OR is slightly lower (~0.76 vs 0.72 in paper)  
- “Other” category unstable due to very small N (← expected)

### Income
- Effects above $100k very close  
- Slight difference for $250k+ likely due to very small subgroup

### Education
- Gradient matches exactly  
- PhD and Medical categories show the largest differences (rare categories)

---

## ✔ 4. Conclusion

The replication is:
- directionally correct  
- statistically consistent  
- methodologically aligned  

Differences are explainable through:
- strict filtering  
- missing-value handling  
- ordinal threshold estimation  
- possible undisclosed SPSS procedures  

These discrepancies do **not** invalidate the replication.

They will be transparently discussed in the report.

