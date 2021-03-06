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
assessments of how reform would affect insurance coverage, premiums,
health care spending, and government costs.\[1\]

Microsimulation models used by the CBO and by others to produce these
estimates draw on economic theory and on a large and growing literature
evaluating past state and federal reform efforts. Yet while models
derive inputs from this shared evidence base, the evidence is uncertain
and not in uniform agreement. Models also differ in their structure,
underlying data sources and assumptions. It should come as no surprise,
then, that models often produce [varying projections of the same reform
proposal](https://www.nytimes.com/interactive/2019/04/10/upshot/medicare-for-all-bernie-sanders-cost-estimates.html).

This current state of affairs has subjected microsimulation models to
criticism over their “black box” like qualities and their tendency to
produce estimates with a limited accompanying sense of sensitivity to
alternative parameters and assumptions. Moreover, modelers have shied
away from producing comparative assessments of overall welfare impact.
Existing models typically produce an array of intermediary point
estimates on welfare-relevant outcomes (e.g., changes in coverage,
premiums, spending and government costs) and leave it to policymakers to
weigh those factors when comparing policy
choices.

<!-- This variation arises from estimation (im)precision in parameters derived from underlying research, and from alternative assumptions on unknown or uncertain parameters. -->

This approach to health policy modeling has a number of important
shortcomings. First, despite modelers’ attempts to caveat the high
degree of uncertainty in their estimates, projections are often afforded
a false sense of precision in policy debates. This results in key
decisions being made without a full accounting of the uncertainty
surrounding the budgetary and coverage impacts on millions of people.
Second, despite [recent efforts at greater
transparency](https://www.cbo.gov/publication/55116), the opacity of
microsimulation models makes it difficult for researchers to know
whether and how their work can inform modeling efforts. Finally, the
development, execution, and maintenance costs of microsimulation models
are considerable. Combined, these factors contribute to high barriers to
conducting rigorous ex ante policy evaluation and a muddled sense of how
the health economic research enterprise could be further refined to
improve policy decision making.

This study outlines an approach to ex ante policy evaluation that
addresses many of the above shortcomings. The first major contribution
is a generalized discrete time and choice modeling framework for
assessing the cost, coverage and welfare impact of health reform
policies. This framework has roots in modeling methods commonly used for
health technology assessment, and in the “sufficient statistics”
[approach to welfare
evaluation](https://www.annualreviews.org/doi/full/10.1146/annurev.economics.050708.142910)
developed in the public finance literature. I demonstrate that this
modeling framework encompasses many existing approaches to health policy
microsimulation, including elasticity-based and utility
maximization-based models. Critically, however, the appraoch also
facilitates simple yet powerful counterfactual policy assessments based
on reduced form estimates. That is, the framework provides researchers
with a tool to investigate the coverage and cost impacts of reform
alternatives without the need for a detailed individual-level
microsimulation model. As a proof of concept, I demonstrate how
difference-in-differences evidence on the impact of Medicaid expansion
on coverage take-up, combined with estimates on take-up of subsidized
private health insurance derived from regression-discontinuity estimates
(Finkelstein, Hendren and Shepard 2019) can be harnessed to model the
coverage and cost impact of further expansion of coverage via public
programs versus via increased subsidies for private coverage.

Second, within this framework I tie together diverse approaches to
assessing uncertainty and the welfare impacts of policy. Specifically, I
draw linkages between the marginal value of public funds (MVPFs), [a
summary measure of the costs and benefits of public
policies](https://economics.mit.edu/files/16272) (Hendren 2017), and
value of information (VOI) methods. Intuitively, VOI quantifies the
opportunity cost of policy decision making under uncertainty. At a given
policy efficiency threshold (e.g., a MVPF value of 0.8, above which a
policy might be desirable but below which it may not), modeling
uncertaninty may or may not affect optional policy choices (i.e.,
choices that maximize relative comparisons of benefits to costs). If
decisions based on comparative assessments of MVPF are insensitive to
varying parameter values, then the value of uncertain information is
low—i.e., it is not worth additional effort to reduce paramter
uncertainty since the same decision would be made today as it would if
we had better information. If decisions are sensitive to this
uncertainty, however, then VOI methods quantify the opportunity cost of
making policy decisions based on *current* information versus if we had
perfect information on uncertain parameters. Variation in modeled
outputs can be further decomposed to identify the relative degree to
which specific parameters contribute to the overall value of perfect
information. These assessments, in turn, can provide guideposts for
refining and prioritizing future research to focus on domains where the
value of information is high. I provide a concrete example of how VOI
can enrich comparative welfare assessments by estimating the relative
contribution of estimation precision and assumptions on the incidence of
uncompensated care in contributing to uncertainty in MVPF estimates for
policies that subsidize the purchase of private insurance coverage.

The remainder of this paper proceeds as follows. In the next section, I
outline a discrete time modeling framework that provides a set of
sufficient statistics to estimate the coverage and cost impact of health
reform policies. I then demonstrate how existing approaches to
microsimulation, including utility maximization and
[elasticity-based](http://www.ct.gov/sustinet/lib/sustinet/board_of_directors_files/resources/grubermodellongerdescription.pdf)
approaches, tie to this generalized modeling framework. Thereafter, I
show the ability of the framework to accomodate modeling using
parameters derived rom reduced form estimates. To do so, I draw on novel
analyses of coverage changes estimtated in the Survey of Income and
Program Participation, and on estimates of subsidized coverage take-up
estimated in Finkelstein, Hendren and Shepard (2019). With this simple
sufficient statistics model in hand, I show how the MVPF can provide a
lens through which we can estimate the value of information on specific
parameters in the model. I do this by estimating the VOI to decompose
model output variation
…

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

1.  This history led one US Senator, Ron Wyden of Oregon, to
    [remark](https://prospect.org/article/number-cruncher-chief) that
    “The history of health reform is congressmen sending health
    legislation off to the Congressional Budget Office to die.”
