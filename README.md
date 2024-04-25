# 2023-01-ALMP-LTU
Replication package for Zezulka and Genin (2024). It contains the code to run the pre-processing, the estimation of the individualized potential outcomes, the (fairness constraint) risk scores, and the algorithmically informed policy simulations as described here. It further allows the reproduction of the reported results, tables, and figures. Unfortunately, we are not allowed to make the data publicly available.

## About the project
Algorithmic fairness focuses on the distribution of predictions at the time of training, rather than the distribution of social goods that arises after deploying the algorithm in a concrete social context. However, requiring a "fair" distribution of predictions may undermine efforts at establishing a fair distribution of social goods. Our first contribution is conceptual: we argue that addressing the fundamental question that motivates algorithmic fairness requires a notion of prospective fairness that anticipates the change in the distribution of social goods after deployment. Our second contribution is theoretical: we provide conditions under which this change is identified from pre-deployment data. That requires distinguishing between, and accounting for, different kinds of performative effects. In particular, we focus on the way predictions change policy decisions and, therefore, the distribution of social goods. Throughout, we are guided by an application from public administration: the use of algorithms to (1) predict who among the recently unemployed will remain unemployed in the long term and (2) target them with labor market programs. Our final contribution is empirical: using administrative data from the Swiss public employment service, we simulate how such policies would affect gender inequalities in long-term unemployment. When risk predictions are required to be "fair", targeting decisions are less effective, undermining efforts to lower overall levels of long-term unemployment and to close the gender gap in long-term unemployment. 

## Setup

## Data
The data is available as scientific use file on SWISSbase.

Michael Lechner, Michael Knaus, Martin Huber, Markus Frölich, Stefanie Behncke, Giovanni Mellace, Anthony Strittmatter (2020). Swiss Active Labor Market Policy Evaluation [Dataset]. Distributed by FORS, Lausanne. https://doi.org/10.23662/FORS-DS-1203-1.

## Contact
[Sebastian Zezulka](https://ethics.epistemology.ai/people#h.ybght6vasbh4)

[Konstantin Genin](https://ethics.epistemology.ai/people#h.frfuicc6nirv)

## Citation
Sebastian Zezulka and Konstantin Genin. 2024. From the Fair Distribution of Predictions to the Fair Distribution of Social Goods: Evaluating the Impact of Fair Machine Learning on Long-Term Unemployment. In: ACM Conference on Fairness, Accountability, and Transparency (ACM FAccT ’24). ACM, New York, NY, USA. https://doi.org/10.1145/3630106.3659020.

The preprint is available on [arxiv](https://arxiv.org/abs/2401.14438).

## Acknowledgements
The scripts "01-preprocessing.R" and "02-potential-outcomes" are based on the publications by:
1. Michael C. Knaus (2022). Double machine learning-based programme evaluation under unconfoundedness.The Econometrics Journal.
see especially: https://github.com/MCKnaus/mcknaus.github.io/blob/master/assets/code/Data_preparation_MCK2022.R

2. John Körtner and Ruben L. Bach (2023). Inequality-Averse Outcome-Based Matching. 

Many thanks to John Körtner and Ruben Bach for sharing their code and for many helpful discussions to Michael Knaus. All remaining errors are my own.
