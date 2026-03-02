# Statistical Machine Learning: Customer Segmentation and Channel Prediction  
### Gaussian Mixture Models & EDDA in R

---

## 📖 Project Overview

This project applies a **Statistical Machine Learning pipeline** to the *Wholesale Customers* dataset (UCI).

The objective is twofold:

1. **Unsupervised learning** to identify latent customer segments based on spending patterns.
2. **Supervised learning** to predict the sales channel (Horeca vs Retail).

The analysis combines probabilistic modeling, model selection, dimensionality reduction, and cross-validated classification within a rigorous statistical framework.

---

## 📊 Dataset

**Source:** UCI Machine Learning Repository  
📎 The direct link to the dataset is provided inside the project report.

**Observations:** 440 customers  

**Variables:**
- Fresh  
- Milk  
- Grocery  
- Frozen  
- Detergents_Paper  
- Delicassen  
- Region (excluded from analysis)  
- Channel (target variable)

---

## 🔎 Phase 1 — Data Exploration & Preprocessing

- Exploratory Data Analysis (EDA)
- Descriptive statistics
- Correlation analysis
- Distribution inspection
- Log-transformation to reduce skewness and outliers
- Channel-based density comparison

**R Packages Used:**
- tidyverse
- ggplot2
- GGally
- summarytools

---

## 🧠 Phase 2 — Model-Based Clustering (Unsupervised Learning)

Customer segmentation was performed using **Gaussian Mixture Models (GMM)** via the `mclust` package.

### Model Selection:
- BIC (Bayesian Information Criterion)
- ICL (Integrated Completed Likelihood)

Different covariance structures were compared:
- VVE
- VVV
- VEE

### Evaluation Metrics:
- CER (Classification Error Rate)
- Adjusted Rand Index (ARI)
- Cluster uncertainty analysis
- KL divergence between clusters

Initial results showed that forcing two clusters was suboptimal.  
The ICL-based 3-cluster solution provided better alignment with natural Channel labels.

---

## 🔄 Variable Selection

Feature reduction was performed using `clustvarsel` (greedy forward search).

Removed variables:
- Fresh  
- Frozen  
- Delicassen  

This significantly improved clustering performance:

| Model Version | CER | ARI |
|---------------|------|------|
| All variables | 0.284 | 0.359 |
| Reduced model | 0.154 | 0.547 |

Dimensionality reduction reduced unnecessary complexity and improved model coherence.

---

## 🎯 Phase 3 — Classification (Supervised Learning)

After identifying the most relevant features, a classifier was trained to predict **Channel**.

### Model Used:
**EDDA (Eigenvalue Decomposition Discriminant Analysis)**  
Implemented with `Rmixmod`.

Selected structure:
**Gaussian_pk_L_C**

Characteristics:
- Variable cluster volumes (pk)
- Common shape (L)
- Common orientation (C)

---

## 🧪 Model Validation

- 70/30 Train-Test split
- Nested cross-validation
- Model selection via:
  - Cross-validation (CV)
  - BIC
  - MER (Misclassification Error Rate)

---

## 📈 Test Set Performance

- **CER:** 0.1136  
- **Adjusted Rand Index:** 0.5914  
- Confusion Matrix analysis  
- Classification uncertainty visualization  

The classifier successfully captures the natural segmentation between Horeca and Retail customers.

---

## 🔬 Statistical Learning Perspective

This project demonstrates:

- Probabilistic generative modeling
- Gaussian Mixture Models
- Covariance structure selection
- Information-criterion-based model comparison
- Unsupervised-to-supervised workflow
- Cross-validated discriminant analysis
- Uncertainty quantification

It represents a complete **Statistical Machine Learning pipeline in R**.

---

## 🛠 Technologies Used

- R
- mclust
- Rmixmod
- clustvarsel
- tidyverse
- ggplot2
- GGally
