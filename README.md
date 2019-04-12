A Sufficient Statistics Approach to Ex Ante Health Policy Evaluation
================
John A. Graves

# Introduction

The objective of this document is to sketch out some initial thoughts
and analytics around a unified framework for ex ante policy evaluation
in health care.

## Background and Motivation

Models projecting the impact of reforms to health insurance programs and
markets play an important role in shaping U.S. health policy. In 2017,
for example, Congressional attempts to repeal and replace the 2010
Affordable Care Act (ACA) collapsed, in part, under public outcry after
the Congressional Budget Office (CBO)
[projected](https://www.nytimes.com/2017/05/24/us/politics/cbo-congressional-budget-office-health-care.html)
that upwards of 23 million people would become uninsured. The twists and
turns of earlier debates over the ACA–and before it, [the Clinton health
plan](figures/01_nyt-clinton-cbo.png)–also were shaped by modelers’
assessments of how reform would impact insurance coverage, premiums,
health care spending, and government costs.

Microsimulation models used by the CBO and by others to produce these
estimates draw on economic theory and on a large and growing literature
evaluating past state and federal reform efforts. Yet while modelers
derive many inputs from this shared evidence base, the evidence is
uncertain & not in uniform agreement. Moreover, models also differ in
their structure, underlying data sources and assumptions. Not
surprisingly, models often produce widely varying projections of the
same policy alternative.

This current state of affairs has subjected microsimulation models to
criticism over their “black box” like qualities, and over their tendency
to produce estimates with a limited accompanying sense of uncertainty or
sensitivity to alternative parameter values and assumptions. Moreover,
modelers have understandably but unfortunately shied away from producing
normative assessments of the overall welfare impacts of policy
alternatives. As a consequence, existing models produce an array of
intermediary point estimates on welfare-relevant outcomes (changes in
coverage, premiums, spending and government costs) and leave it to
policymakers to weigh those factors when considering policy
alternatvies.

This approach has a number of important shortcomings. First, despite
modelers’ attempts to caveat the high degree of uncertainty in their
estimates, projections are afforded a false sense of precision in
high-stakes policy debates. Second, the “black box” like quality of most
models makes it difficult for researchers to know whether and how their
work can inform modeling efforts. This leaves a muddled sense of how
future research could be directed and refined to optimize impact.

This project develops an approach to ex ante policy evaluation that
addresses many of these shortcomings. First, I outline a generalized
framework for modeling health reform alternatives. This framework is
simple yet powerful and has roots in health economic modeling techniques
often used for health technology assessments, and in the “sufficient
statistics” approach to welfare evaluation. I demonstrate that this
framework can not only encompass many existing approaches to
microsimulation, but also lends itself to powerful counterfactual policy
evaluations based simply on reduced form estimates (i.e., without the
need for detailed individual-level microsimulation). Second, within this
framework I tie together two imoportant but diverse approaches to
assessing the welfare impacts of policy.

![](./figures/01_model-diagrams_simple-model.png)

# Health Reform Modeling as a Discrete Time Markov Process

This section outlines a simple discrete time markov model for health
insurance coverage in the U.S. population. We begin by defining an ex
ante occupancy vector \(\mathbf{p_{exa}}\) that summarizes the fraction
of the population in each major health insurance type
(employer-sponsored insurance, other private insurance, public
insurance, and uninsured) in the pre-reform period.

\[
\mathbf{p_{exa}}=
\left(
\begin{array}{c}
p_{exa,esi}\\
p_{exa,pri}\\
p_{exa,pub}\\
p_{exa,unin}
\end{array}
\right) 
\] where \(p_{exa,k}\) is the fraction of the population in each
insurance category \(k\) in the ex ante period.

Now define the transition probability matrix:

\[
\mathbf{R} =  [r_{k,j}] =   
\begin{pmatrix}
      r_{esi,esi} & r_{esi,pri} & r_{esi,pub} & r_{esi,unin}  \\
       r_{pri,esi} & r_{pri,pri} & r_{pri,pub} & r_{pri,unin}  \\
        r_{pub,esi} & r_{pub,pri} & r_{pub,pub} & r_{pub,unin}  \\
         r_{unin,esi} & r_{unin,pri} & r_{unin,pub} & r_{unin,unin}  
    \end{pmatrix}
\] where \(r_{k,j}\) is the probability of transitioning from ex ante
category \(k\) to ex post category \(j\).

Finally, we can define an ex post occupancy vector:

\[
\mathbf{p_{exp}}=
\left(
\begin{array}{c}
p_{exp,esi}\\
p_{exp,pri}\\
p_{exp,pub}\\
p_{exp,unin}
\end{array}
\right) 
\]

Basic matrix algebra links the two occupancy vectors as follows:

\[
\begin{aligned}
    \begin{pmatrix}
p_{exa,esi}\\
p_{exa,pri}\\
p_{exa,pub}\\
p_{exa,unin} \\
    \end{pmatrix}'
        \cdot
    \begin{pmatrix}
      r_{esi,esi} & r_{esi,pri} & r_{esi,pub} & r_{esi,unin}  \\
       r_{pri,esi} & r_{pri,pri} & r_{pri,pub} & r_{pri,unin}  \\
        r_{pub,esi} & r_{pub,pri} & r_{pub,pub} & r_{pub,unin}  \\
         r_{unin,esi} & r_{unin,pri} & r_{unin,pub} & r_{unin,unin} 
    \end{pmatrix}
    &=&
    \begin{pmatrix}
p_{exp,esi}\\
p_{exp,pri}\\
p_{exp,pub}\\
p_{exp,unin} \\
    \end{pmatrix}'
  \end{aligned}
\]

In the equation above, the set of transition probabilities \(r_{k,j}\)
can be considered sufficient statistics for evaluating the impact of a
policy change on health insurance coverage in the population. That is,
once we know these probabilities and how they change under a given
reform option, we can simulate the impact on the overall coverage
distribution in the population. By attaching costs to population
movements among insurance types, we can simulate the cost impact to the
government. And finally, as we show below, social welfare weights can
also be attached to population movements. These weights can then be
aggregated and compared across reform alternatives to make normative
assessments of policy options.

### Estimating the Transition Probability Matrix

We first obtain a simple cross tabulation of insurance coverage in
January 2013 from the SIPP.

<table>

<caption>

Ex Ante Distribution of Insurance Coverage, January 2013

</caption>

<thead>

<tr>

<th style="text-align:left;">

Category

</th>

<th style="text-align:right;">

Number (millions)

</th>

<th style="text-align:right;">

Percent

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ESI

</td>

<td style="text-align:right;">

118.7

</td>

<td style="text-align:right;">

62.4

</td>

</tr>

<tr>

<td style="text-align:left;">

Private-Other

</td>

<td style="text-align:right;">

11.1

</td>

<td style="text-align:right;">

5.9

</td>

</tr>

<tr>

<td style="text-align:left;">

Public

</td>

<td style="text-align:right;">

20.9

</td>

<td style="text-align:right;">

11.0

</td>

</tr>

<tr>

<td style="text-align:left;">

Uninsured

</td>

<td style="text-align:right;">

39.5

</td>

<td style="text-align:right;">

20.8

</td>

</tr>

</tbody>

</table>

Next we fit nonpaarametric (Kaplan-Meier) and parametric multi-state
models to obtain the transition probabilities by December 2013.

<table>

<caption>

Transition Probabilities

</caption>

<thead>

<tr>

<th style="text-align:left;">

baseline

</th>

<th style="text-align:right;">

01\_esi

</th>

<th style="text-align:right;">

02\_priv\_oth

</th>

<th style="text-align:right;">

03\_public

</th>

<th style="text-align:right;">

04\_uninsured

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

01\_esi

</td>

<td style="text-align:right;">

94.4

</td>

<td style="text-align:right;">

1.6

</td>

<td style="text-align:right;">

0.8

</td>

<td style="text-align:right;">

3.2

</td>

</tr>

<tr>

<td style="text-align:left;">

02\_priv\_oth

</td>

<td style="text-align:right;">

15.1

</td>

<td style="text-align:right;">

76.1

</td>

<td style="text-align:right;">

2.6

</td>

<td style="text-align:right;">

6.2

</td>

</tr>

<tr>

<td style="text-align:left;">

03\_public

</td>

<td style="text-align:right;">

6.4

</td>

<td style="text-align:right;">

2.4

</td>

<td style="text-align:right;">

86.2

</td>

<td style="text-align:right;">

5.0

</td>

</tr>

<tr>

<td style="text-align:left;">

04\_uninsured

</td>

<td style="text-align:right;">

13.4

</td>

<td style="text-align:right;">

5.4

</td>

<td style="text-align:right;">

9.9

</td>

<td style="text-align:right;">

71.3

</td>

</tr>

</tbody>

</table>
