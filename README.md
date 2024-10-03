# MDCEV_food

Here, a multiple discrete-continuous extreme value (MDCEV) model will be used (Bhat, 2008; Vasquez-Lavín & Hanemann, 2008). In this approach, people decide which food items to consume (Discrete decision) and how much consumption (positive amount) of the different chosen food items (the continuous component of the model) in a simultaneous decision process. That is, people consume some but not necessarily all food items (represented by food groups, such, meats, fruits or vegetables). The model includes a budget constraint for these multiple food items. This model allows to investigate choice behavior under multiple alternatives and provides an ideal platform for modeling food's purchase decisions (Bhat & Eluru, 2010).

Defining the budget constraint for a subset of goods (food items) is an issue that needs to be addressed theoretically and empirically. It starts by defining the food budget constraint based on data in the expenditure survey. For instance, the average expenditure on food in Chile was around 18,7% of the household income according to expenditure household survey (INE, 2018). Nevertheless, we can set a more flexible model by defining the budget constraint as the total household income and including an "outside good" into the model. This framework is more adaptable to capture substitution patterns inside and outside food items: substitution with other non-food items (e.g., clothing, education). In this case, the outside good is the difference between total income and food expenditure (LaMondia et al., 2008; Palma & Hess, 2020, p. 25)
The model will start with the definition of an additively separable utility function that has as an argument 𝑓𝑘 (the food budget allocation for each group of food), which could also be zero. This is:

$$
U(f) = \frac{1}{\alpha_1}\varphi_1f_1^{\alpha_1} + \sum_{k=2}^K \frac{\gamma_k}{\alpha_k}\varphi_k \left[\left(\frac{f_k}{\gamma_k}+1\right)^{\alpha_k}-1\right],
$$

in which 𝜑 corresponds with the baseline utility and is defined as positive, that is, 𝜑𝑘=exp(𝛽′𝑧𝑘+𝜀𝑘), where 𝛽′𝑧𝑘 indicates the alternative's baseline utility and 𝜀𝑘 is the i.i.d. random disturbance, following a Gumbel (0,𝜎) distribution. The first component of the equation points to the outside good. The marginal rate of substitution between the two alternatives can be expressed by the ratio of their respective baselines utilities. 𝛼𝑘 is a satiation parameter representing the diminishing marginal utility, 𝛾𝑘 is a translation parameter (also involved in the level of satiation) and captures possible corner solutions, and 𝑓𝑘 is the consumption quantity of alternative 𝑘, in this case, the 𝑘 food group of food budget use. Moreover, 𝜑>0,𝛾𝑘>0 and 𝛼𝑘≤1. The individual maximizes this utility subject to a budget constraint:

EQ2

where F is the total amount of budget available, which must be equal to the sum of the food budget allocated to each food group. Bhat (2008) proposed normalize α_k=0 for all alternatives to estimate a γ_k profile or normalize γ_k=1 for all alternatives to estimate an α_k-profile. Also, Bhat (2018) demonstrates that it is feasible to estimate σ scale parameter when γ_k-profile is used. Therefore, utility form in (7) is modified to:

EQ3

this maximization problem can be solved using the Lagrangian multiplier technique. The KT conditions (first-order conditions) can be used to build a likelihood function that can be estimated using conventional, or simulated, maximum likelihood approaches. The Lagrangian can be expressed as follows:

EQ4

where λ is the Lagrangian multiplier associated with the budget constraint. Then, the KT first-order condition for the optimal food allocation (f_k^* values) is given by:

EQ5

Where V_k^*=〖〖(β〗^*)〗^' z_k-1/σ  ln⁡((f_k^*)/γ_k +1) and V_1^*=〖(β^*)〗^' z_1-1/σ  ln⁡(f_1^* ) with β^*=β/σ. Note that β is the parameter's vector for the utility specification in equation 1, and β^* is for the utility form in equation 8 (γ_k-profile).

Finally, to forecast the implications of different policy implementations for diet changes, the algorithm proposed by Pinjari and Bhat (2010) will be used. They propose an efficient and computationally fast forecasting algorithm for the MDCEV model. The model predicts the average share of the budget that individuals will devote to each food group in the baseline situation. Once the model parameters are estimated, policy analysis solves the stochastic, constrained, and nonlinear utility maximization problem for each decisionmaker's optimal consumption quantities. The same authors suggest using these predicted values to analyze the model's sensitivity to changes in explanatory variables, representing various policy interventions. Two policy interventions will be assessed. First, a trade and productive intervention decreasing the availability of red and processed meat, and a tax-based instrument, increasing prices for sugar and red and processed meat. The proposed MDCEV model will be estimated using Apollo software (Hess & Palma, 2019).

