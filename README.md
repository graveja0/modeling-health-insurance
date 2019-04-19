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
of earlier debates over the ACA—and before it, [the Clinton health
plan](figures/01_nyt-clinton-cbo.png)—also were shaped by modelers’
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
can inform modeling efforts. The development, execution, and maintenance
costs of microsimulation models are also considerable. Combined, these
factors contribute to high barriers to conducting rigorous ex ante
policy evaluation and a muddled sense of how the health economic and
policy research enterprise could be further refined to improve policy
decision making.

This study outlines an approach to ex ante policy evaluation that
addresses many of the above shortcomings. First, I outline a generalized
discrete time modeling framework for assessing the cost, coverage and
welfare impact of health reform policies. This framework has roots in
health economic modeling methods commonly used for health technology
assessment, and in the “sufficient statistics” approach to welfare
evaluation developed in public finance. I demonstrate that this modeling
framework can encompass many existing approaches to health policy
microsimulation, but also facilitates simple yet powerful counterfactual
policy assessments based primarily on reduced form estimates. That is,
the framework provides researchers with a simple tool to investigate the
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
draw linkages between the marginal value of public funds (MVPFs), a
summary measure of the costs and benefits of public policies (Hendren
2017), and value of information (VOI) methods developed to quantify the
importance of model parameter uncertainty in health technology
assessments. Intuitively, VOI quantifies the opportunity cost of
decision making under uncertainty. At a given policy efficiency or
willingness-to-pay threshold (e.g., a MVPF value of 0.8, above which a
policy might be desirable but below which it may not), uncertaninty may
or may not affect optimal decision making. If comparative assessments of
MVPF are insensitive to varying assumptions or to estimation uncertainty
in model parameters at a given threshold, then the value of information
on these parameters at that threshold is low—i.e., it is not worth
additional effort to reduce model uncertainty. If optimal policy
decisions are sensitive to this uncertainty, however, then VOI methods
provide a concrete sense of what drives decision making uncertainty—that
is, to what degree is variation in model outputs driven by sensitivty to
assumptions versus estimation uncertainty in the parameters? These
assessments, in turn, can provide a guidepost for refining and
prioritizing future research. I show the utility of VOI methods for
health policy modeling by assessing the relative contribution of
estimation precision and assumptions on the incidence of uncompensated
care in contributing to uncertainty in MVPF estimates for policies that
subsidize the purchase of private insurance coverage based on the
estimates in Finkelstein, Hendren and Shepard
(2019).

<!-- As an example, I  apply VOI methos to reduced form evidence on the MVPF  willingness to pay (WTP) for subsidized health insurance, which i -->

<!-- I show how VOI methods can fileter uncertainty in RD paramters from Finkelstein , as well as assumptions  -->

<!-- That is, VOI methdos can be used to quantify the opportunity cost of a "wrong" decision. In that way, we can identify and prioritize research and modeling efforts on the parameters and/or assumptions that drive uncertainty. To demonstrate the power of VOI -->

<!-- ### Estimating the Transition Probability Matrix -->

<!-- We first obtain a simple cross tabulation of insurance coverage in January 2013 from the SIPP.  -->

<!-- ```{r, echo = FALSE} -->

<!-- # source(here("R/estimate-overall-transition-probability-matrix.R")) -->

<!-- ex_ante <-  -->

<!--   read_rds(here("output/ex-ante-overall-population/ex-ante-distribution.rds")) -->

<!-- ex_ante_meps <-  -->

<!--   read_rds(here("output/ex-ante-overall-population/ex-ante-distribution-meps.rds")) -->

<!-- ex_ante %>%  -->

<!--   mutate(n = round(n/1e6,1)) %>%  -->

<!--   mutate(pct = round(100*pct,1)) %>%  -->

<!--   mutate(insurance_type = c("ESI","Private-Other","Public","Uninsured")) %>%  -->

<!--   cbind( -->

<!--     ex_ante_meps %>%  -->

<!--     mutate(n = round(n/1e6,1)) %>%  -->

<!--     mutate(pct = round(100*pct,1)) %>%  -->

<!--     select(-insurance_type)  -->

<!--   ) %>%  -->

<!--   knitr::kable(caption = "Ex Ante Distribution of Insurance Coverage, January 2013",col.names= c("Category","SIPP: Number (millions)","SIPP: Percent", "MEPS: Number (millions)","MEPS: Percent"), format = "html") -->

<!-- ``` -->

<!-- Next we fit nonpaarametric (Kaplan-Meier) and parametric multi-state models to obtain the transition probabilities by December 2013.  -->

<!-- ```{r} -->

<!-- trans_probs <-  -->

<!--     read_rds(here("output/ex-ante-overall-population/transition-probabilities-kaplan-meier.rds")) -->

<!-- trans_probs %>% filter(time==24) %>% pluck("data") %>% pluck(1) %>%  -->

<!--   mutate_at(vars(-1),function(x) round(100*x,2)) %>%  -->

<!--   knitr::kable(caption = "Transition Probabilities", format = "html") -->

<!-- ``` -->

<!-- ```{r} -->

<!--  trans_probs_meps <-  -->

<!--     read_rds(here("output/ex-ante-overall-population/transition-probabilities-kaplan-meier-meps.rds")) -->

<!-- trans_probs_meps %>% filter(time==24) %>% pluck("data") %>% pluck(1) %>%  -->

<!--   mutate_at(vars(-1),function(x) round(100*x,2)) %>%  -->

<!--   knitr::kable(caption = "Transition Probabilities", format = "html") -->

<!-- ``` -->
