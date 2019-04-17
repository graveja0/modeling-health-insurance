A Sufficient Statistics Approach to Ex Ante Health Policy Evaluation
================
John A. Graves

Models projecting the impact of reforms to health insurance programs and
markets play an important role in shaping U.S. health policy. In 2017,
for example, Congressional attempts to repeal and replace the 2010
Affordable Care Act (ACA) were hampered by public outcry after the
Congressional Budget Office (CBO)
[projected](https://www.nytimes.com/2017/05/24/us/politics/cbo-congressional-budget-office-health-care.html)
that upwards of 23 million people would become uninsured. The
[twists](https://prescriptions.blogs.nytimes.com/2009/10/07/analysis-sees-baucus-bill-meeting-obamas-cost-and-deficit-targets/)
[and](https://www.nytimes.com/2009/10/19/us/19iht-letter.html)
[turns](https://www.nytimes.com/2009/10/06/health/policy/06health.html)
of earlier debates over the ACA–and before it, [the Clinton health
plan](figures/01_nyt-clinton-cbo.png)–also were shaped by modelers’
assessments of how reform would impact insurance coverage, premiums,
health care spending, and government costs.

Microsimulation models used by the CBO and by others to produce these
estimates draw on economic theory and on a large and growing literature
evaluating past state and federal reform efforts. Yet while models
derive inputs from this shared evidence base, the evidence is uncertain
and not in uniform agreement. Models also differ in their structure,
underlying data sources and assumptions. It should come as no surprise,
then, that models often produce [widely varying projections of the same
reform
proposal](https://www.nytimes.com/interactive/2019/04/10/upshot/medicare-for-all-bernie-sanders-cost-estimates.html).

This current state of affairs has subjected microsimulation models to
criticism over their “black box” like qualities and their tendency to
produce estimates with a limited accompanying sense of uncertainty or
sensitivity to alternative parameter values and assumptions. Moreover,
modelers have understandably but unfortunately shied away from producing
comparative assessments of overall welfare impact. Existing models
typically produce an array of intermediary point estimates on
welfare-relevant outcomes (e.g., changes in coverage, premiums, spending
and government costs) and leave it to policymakers to weigh those
factors when comparing policy choices.

This approach to health policy modeling has a number of important
shortcomings. First, despite modelers’ attempts to caveat the high
degree of uncertainty in their estimates, modeled projections are often
afforded a false sense of precision in high-stakes policy debates. This
results in decisions being made in spite of a high degree of uncertainty
surrounding the budgetary, health and coverage impact of proposed
reforms. Second, the “black box”-like opacity of microsimulation models
makes it difficult for researchers to know whether and how their work
can inform modeling efforts. Finally, the development, execution, and
maintenance costs of microsimulation models are considerable. Combined,
these factors contribute to high barriers to understanding and a muddled
sense of how the health economic and policy research enterprise could be
further refined to improve policy decision making.

This study outlines an approach to ex ante policy evaluation that
addresses many of the above shortcomings. First, I outline a generalized
discrete time modeling framework for assessing the cost, coverage and
welfare impact of health reform policies. This framework has roots in
health economic modeling methods used worldwide for health technology
assessment, and in the “sufficient statistics” approach to welfare
evaluation developed in public finance. I demonstrate that this modeling
framework can encompass many existing approaches to health policy
microsimulation, but also facilitates simple yet powerful counterfactual
policy aassessments based on reduced form estimates. That is, the
framework provides researchers with a simple tool to investigate the
coverage and cost impacts of reform alternatives without the need for a
detailed individual-level microsimulation model. As a proof of concept,
I demonstrate how differences-in-differences evidence on the impact of
Medicaid expansion on coverage, combined with regression-discontinuity
estimates on willingness to pay for subsidized health insurance
(Finkelstein, Hendren and Shepard 2019) can be harnessed to model the
impact of further expansion of coverage via public programs versus via
increased subsidies for private coverage.

Second, within this framework I tie together diverse approaches to
assessing uncertainty and the welfare impacts of policy. Specifically, I
draw linkages between standard wefare impact measures used in health
technology assessment (e.g., net health benefit and net monetary
benefit) and the marginal value of public funds (MVPFs), a summary
measure of the costs and benefits of public policies (Hendren 2017).
This linkage allows for a systematic approach to understanding parameter
and modeling uncertainty based on probalistic sensitivity anlayses
(PSAs) and value of information (VOI) methods.

Intuitively, VOI methods quantify the opportunity cost of decision
making under uncertainty. At a given policy efficiency or
willingness-to-pay threshold (e.g., a MVPF value of 0.8, above which a
policy might be implemented but below which it may not), uncertaninty
may or may not affect optimal decision making. If policy decisions based
on comparative assessments of MVPF are insensitive to varying
assumptions or to estimation uncertainty in model parameters, then the
value of information on these parameters is low – i.e., it is not worth
pursuing additional research to reduce uncertainty. If policy decisions
are sensitive to this uncertainty, however, then these methods provide a
guidepost for priortizing and refining future research. The value of
information on individual parameters in a model can be estimated, and
these

<!-- As an example, I  apply VOI methos to reduced form evidence on the MVPF  willingness to pay (WTP) for subsidized health insurance, which i -->

<!-- I show how VOI methods can fileter uncertainty in RD paramters from Finkelstein , as well as assumptions  -->

<!-- That is, VOI methdos can be used to quantify the opportunity cost of a "wrong" decision. In that way, we can identify and prioritize research and modeling efforts on the parameters and/or assumptions that drive uncertainty. To demonstrate the power of VOI -->

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
aggregated and compared across reform alternatives to make comparative
evaluations of policy options.

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

SIPP: Number (millions)

</th>

<th style="text-align:right;">

SIPP: Percent

</th>

<th style="text-align:right;">

MEPS: Number (millions)

</th>

<th style="text-align:right;">

MEPS: Percent

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

<td style="text-align:right;">

113.5

</td>

<td style="text-align:right;">

61.5

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

<td style="text-align:right;">

4.4

</td>

<td style="text-align:right;">

2.4

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

<td style="text-align:right;">

20.9

</td>

<td style="text-align:right;">

11.3

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

<td style="text-align:right;">

45.7

</td>

<td style="text-align:right;">

24.8

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

88.3

</td>

<td style="text-align:right;">

3.5

</td>

<td style="text-align:right;">

1.2

</td>

<td style="text-align:right;">

7.0

</td>

</tr>

<tr>

<td style="text-align:left;">

02\_priv\_oth

</td>

<td style="text-align:right;">

27.5

</td>

<td style="text-align:right;">

53.0

</td>

<td style="text-align:right;">

6.7

</td>

<td style="text-align:right;">

12.8

</td>

</tr>

<tr>

<td style="text-align:left;">

03\_public

</td>

<td style="text-align:right;">

5.5

</td>

<td style="text-align:right;">

3.7

</td>

<td style="text-align:right;">

79.1

</td>

<td style="text-align:right;">

11.7

</td>

</tr>

<tr>

<td style="text-align:left;">

04\_uninsured

</td>

<td style="text-align:right;">

18.4

</td>

<td style="text-align:right;">

9.1

</td>

<td style="text-align:right;">

20.0

</td>

<td style="text-align:right;">

52.5

</td>

</tr>

</tbody>

</table>

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

85.8

</td>

<td style="text-align:right;">

0.9

</td>

<td style="text-align:right;">

2.1

</td>

<td style="text-align:right;">

10.9

</td>

</tr>

<tr>

<td style="text-align:left;">

02\_priv\_oth

</td>

<td style="text-align:right;">

23.9

</td>

<td style="text-align:right;">

62.9

</td>

<td style="text-align:right;">

2.5

</td>

<td style="text-align:right;">

10.7

</td>

</tr>

<tr>

<td style="text-align:left;">

03\_public

</td>

<td style="text-align:right;">

6.5

</td>

<td style="text-align:right;">

1.1

</td>

<td style="text-align:right;">

68.2

</td>

<td style="text-align:right;">

22.8

</td>

</tr>

<tr>

<td style="text-align:left;">

04\_uninsured

</td>

<td style="text-align:right;">

24.1

</td>

<td style="text-align:right;">

5.8

</td>

<td style="text-align:right;">

19.3

</td>

<td style="text-align:right;">

50.3

</td>

</tr>

</tbody>

</table>
