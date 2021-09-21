---
title: 
subtitle: Just notes. 
author: Mossa Merhi Reimert
---

### 2021-09-21

Maj \& Mossa;

The current model that we are going with for SCC curves (or $\log(SCC)$) curves
is called Wood's Model, i.e.

$$ y_t = a t ^ {b} \exp(-c t) $$

where $t$ is lactation day, and $a,b,c > 0$. Peak lactation day is $t_\texttt{peak} = b/c$.

Total yield is $y = \dfrac{a}{c^{b+1}} \Gamma(b+1)$; See [Algebraic Model of the Lactation Curve in Cattle](https://rdcu.be/cyaEH)

The areas that can be improved are many, and these are just some of them:

- [ ] Missing data overall: Since we only observe 10 measurements (at most) across
      across the entire lactation period (5, 305), there ought to be issues
      with low sample size here.
- [ ] Missing data early in the lactation: The peak period might not not be
    captured at all, and the missing information here matters; One can choose
    to reduce the leverage of these observations on overall estimation; Or install
    prior knowledge about the expected peak $b/c$; Etc.
- [ ] Influences from previous lactation period, like treatment, etc.
- [ ] Affects of treatment on the SCC curves.
