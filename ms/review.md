# Review: Dugar, Bhattacharya and Reiley, "Can't Buy Me Love?" (Economic Inquiry, 2012)

Audit of the replication package (`lpmdata.dta`, `Marriage Ads from newspaper issues.xls`) against the
December 2010 final draft ([sources/paper_2010_12_final_draft.pdf](../sources/paper_2010_12_final_draft.pdf)).
The published journal version was not retrieved (paywalled); see [Limits](#limits-of-this-audit).
Every number below is produced by `make run`; tables are in [`output/`](../output).

## Verdict

**The paper's numbers reproduce. Its central qualitative claims survive. Its headline magnitudes do not
carry the precision the text gives them, and one exclusion the text does not describe is load-bearing.**

- Tables 3, 4, 5 (all three columns, N and R²) and 20 of 21 footnote hypothesis tests reproduce exactly from
  the Stata file. Table 6's two Brahmin panels reproduce; its Kayastha panel has an arithmetic slip.
- Claim 1 (own-caste preference) and claim 2 (higher-caste families' letters shift toward a lower-caste groom
  as his advertised income rises) hold under every inference method tried and in both newspaper editions.
- Claim 3, the compensation magnitudes ("an LC groom at Rs 35,000 needs about Rs 36,000 more"), is a point
  estimate with a 95% bootstrap interval of roughly Rs 17,000 to 68,000, and moves between Rs 12,500 and
  Rs 45,000 depending on which income slope closes the gap. Every implied income lies above the 99th
  percentile of the 1,261 real advertisements the paper itself collected. The two secondary comparisons the
  paper draws from Table 6 (LC grooms need more than MC grooms; compensation falls with income) have
  intervals that include zero.
- The sample of 1,123 letters is reached by dropping 243 letters flagged `repeat` in the file. The text
  describes dropping 47 non-unique letters from 22 responders plus letters from other castes; the file
  contains no other-caste code. The dropped letters go disproportionately to lower-caste, high-income
  grooms, and including them roughly halves the Table 6 figures.

## A better reading of the same data

The paper frames the letters as higher-caste women discriminating against lower-caste men, and asks what
income buys the loss of status. The letters support a plainer description. Every caste writes mostly to its
own: per ad, an own-caste groom draws 1.4 to 5 times the letters of any other groom, and Brahmin and
Namasudra families sort equally hard. Nobody writes up: Kayastha families at Rs 35,000 prefer the Namasudra
groom to the Brahmin, and Namasudra families send the Brahmin groom 3.8% of letters per ad. Income moves
letters only downward, and pushes lower-caste letters away from rich higher-caste grooms. The preference is
horizontal, for one's own caste, not vertical, for rank, which is also what Banerjee et al. (2013) conclude
from the other side of this market.

A letter mixes what a family wants with what it expects to get, and no cell of this design separates the
two, in either direction. The rising Brahmin-to-Namasudra curve could be taste responding to money or a guess
about which Namasudra family would accept a Brahmin bride; the falling Namasudra-to-Brahmin curve could be
identity or discouragement. The price in Table 6 therefore prices a ranking of advertisements, not a loss of
status, and does so by extrapolating past every income the design tried. [The paper in its own
words](interpretation.md) sets thirteen of its sentences beside the numbers.

## Claim-to-estimand table

| # | Claim (paper location) | Unit / population | Estimand as implemented | Comparison | Uncertainty reported |
|---|---|---|---|---|---|
| 1 | Higher-caste families prefer own-caste grooms (§4.1, Table 4, Table 5, fn 15–16) | Letters from HC (478) and MC (374) families to nine ads in one Bengali newspaper, Sept 2007 | Share of a caste's letters sent to groom j, minus share sent to LCG-LI (LPM coefficient) | Own-caste vs lower-caste groom at the same income | HC1 robust SE on stacked rows |
| 2 | Higher-caste letters to a lower-caste groom rise with his income (§4.2, Table 5, fn 19–20) | Same | Same | Same groom caste across Rs 7k, 15k, 35k | Same |
| 3 | Compensation: extra income a lower-caste groom needs to draw the own-caste groom's letter share (§4.3, Table 6) | Same | (share own − share other) ÷ (share at 35k − share at 7k)/28, in thousand Rs | Linear extrapolation beyond the design | None |
| 4 | Higher-quality responders demand more compensation (§4.4, unreported) | Same, split into PCA quartiles | Same as 3 within quartile | Quartile 4 vs 1 | None; results "available on request" |

The estimand is a **share of letters**, not a family's probability of responding. Each letter chose exactly
one of nine ads, so the nine shares sum to one by construction. The regressions are a saturated multinomial
share model written as an LPM; "probability of response" in the paper means "share of this caste's letters".
Nothing here measures how many additional families a higher income draws into writing at all.

## Findings, ranked

### A. Number-changing

**A1. Table 6, Kayastha panel: arithmetic error.** Published MCR-LCG at MI is 17.16; the paper's own formula
(fn 21) gives (0.0508 − 0.0107) / (0.0829/28) = **13.55**. The published value is 0.0508 / 0.00296, i.e. the
LCG-MI coefficient was not subtracted. The LI cell is published as 0.00 where the formula gives **1.81**
(the paper appears to have zeroed it because MCG-LI = LCG-LI is not rejected, but that rule is not stated
and is not applied to other insignificant gaps). Published row mean 13.24 becomes **12.65**. Direction of the
paper's fn 22 remark (MC compensation rises with income) is unchanged.
Source: [`output/comparison_published.md`](../output/comparison_published.md).

**A2. Footnote 16 contradicts the body and the data.** Fn 16 says "H0: MCG-LI = LCG-LI is rejected at 1%".
The reproduced test gives F(1, 3357) = 0.08, p = 0.78. The body text (§4.1) correctly says MC responders
"treat a LCG at par with their own-caste groom" at low income. The footnote is wrong.

**A3. Sample exclusion is undocumented and load-bearing.** The file flags 243 of 1,366 letters as repeats
(HC 95, MC 91, LC 57; 176 of them from the second edition). The text says 47 non-unique letters from 22
responders were dropped, plus letters from castes outside the design. No other-caste code exists in the
file, so either the flag also carries the other-caste exclusion (then the text's 47 is wrong) or it marks
something else. With all 1,366 letters, Table 6 becomes:

| Cell | Paper sample | All letters |
|---|---|---|
| HCR-MCG, HI | 27.4 | 15.1 |
| HCR-LCG, HI | 35.9 | 19.8 |
| HCR-LCG, LI | 49.3 | 37.9 |
| MCR-LCG, HI | 22.6 | 4.3 |

Repeat letters go disproportionately to LCG-HI and MCG-HI. If a "repeat" family is one that wrote to several
ads, it is exactly the family whose behaviour the caste-income trade-off is about, and dropping it biases the
sample toward single-ad, own-caste writers. This cannot be resolved without the letter-level identifiers.
Source: [`output/audit_table6_with_repeat_letters.md`](../output/audit_table6_with_repeat_letters.md),
[`output/audit_sample_flow.md`](../output/audit_sample_flow.md).

### B. Inferential

**B1. Table 6 carries no uncertainty; the intervals are wide.** Letter-level bootstrap (2,000 draws within
responder caste) and a delta-method on the multinomial shares agree:

| Cell | Estimate | 95% bootstrap | Delta SE |
|---|---|---|---|
| HCR-MCG HI | 27.4 | 8.7 – 61.4 | 12.3 |
| HCR-LCG HI | 35.9 | 17.3 – 67.9 | 11.9 |
| HCR-LCG LI | 49.3 | 34.0 – 73.8 | 9.6 |
| MCR-LCG HI | 22.6 | 0.0 – 84.0 | 15.4 |
| MCR-LCG LI | 1.8 | −18.0 – 14.8 | 6.6 |

The "almost double his income" sentence (Rs 35,000 + 36,000) is consistent with anything from "plus 50%" to
"nearly triple". Source: [`output/audit_table6_uncertainty.md`](../output/audit_table6_uncertainty.md).

**B2. Secondary Table 6 comparisons are not distinguishable from zero.** The paper's "third observation"
(LC grooms need more than MC grooms at every income) and "first observation" (compensation falls as income
rises) are differences of two ratios. Bootstrap:

| Contrast (HC responders) | Estimate | 95% bootstrap | Share of draws > 0 |
|---|---|---|---|
| LC − MC compensation, HI | 8.5 | −26 – 43 | 0.72 |
| LC − MC compensation, MI | 1.7 | −24 – 24 | 0.57 |
| LC − MC compensation, LI | 8.9 | −17 – 33 | 0.79 |
| LC compensation, LI − HI | 13.4 | −6 – 30 | 0.92 |
| MC compensation, LI − HI | 13.1 | −9 – 30 | 0.90 |

Source: [`output/audit_table6_contrasts.md`](../output/audit_table6_contrasts.md).

**B3. Standard errors: unit-of-analysis concern is real but small.** The nine rows per letter are mechanically
dependent (they sum to one). Letter-clustered SEs exceed the paper's HC1 SEs by 1–12% (largest for LC
responders); an exact multinomial variance gives the same numbers. All 21 footnote tests keep their
verdicts. Source: [`output/audit_se_three_ways.md`](../output/audit_se_three_ways.md),
[`output/audit_footnote_tests_three_ways.md`](../output/audit_footnote_tests_three_ways.md).

### C. Identification and design

**C0. The nine ads were not the choice set.** The paper reports about 1,000 matrimonial ads per Sunday
edition, roughly 370 from grooms. A letter cost Rs 5. A family writes to every ad it finds acceptable, so
the count of letters an ad received is the number of families who found it acceptable that week, not the
number who ranked it above the other eight fictitious ads or above the real ones. Table 6 equalises two
ads' counts and calls the result the income a groom "would need in order to compensate" a family; no
family's trade-off is observed. An earlier version of this repository inverted the logic and reported income families "forgo" for caste;
that was retracted for the same reason: the counts being compared are different families, each facing a
menu of hundreds. What the
counts support is relative draw across ads and its income gradient (elasticities), and a sorting index
against the caste-blind third. Banerjee et al. (2013) observe the other side, an advertiser ranking the
letters it actually received; theirs is a choice within a family, and only that kind of observation prices
a trade-off.

**C1. Nine ads, each a single treatment cell.** Every caste-income combination is one advertisement with one
PO box, one wording, one page position, one randomly drawn age/height. Anything specific to an ad is absorbed
into its caste-income effect. The only replication is the second edition of the same nine ads. Letter shares
are statistically indistinguishable across editions (chi-square on 9 × 2 tables: HC p = 0.86, MC p = 0.13,
LC p = 0.21), which is reassuring for claims 1 and 2 but not a test of ad-specific effects, since the same
ads were reused. Table 6 by edition: HCR-LCG HI is 45.1 in edition 1 and 30.0 in edition 2; MCR-LCG HI is
51.7 and 1.6. Source: [`output/audit_edition_tests.md`](../output/audit_edition_tests.md),
[`output/audit_table6_by_edition.md`](../output/audit_table6_by_edition.md).

**C2. Income is confounded with education by design.** Low-income grooms carry a Bachelor's degree, medium
and high a Master's (§2.1). The "income" effect between Rs 7k and Rs 15k is income plus education. The paper
states this choice; it does not discuss it when interpreting the LI→MI step, which is the steepest part of
the response.

**C3. Functional form drives the magnitude.** The response is convex in income (HC letters to the LC groom:
5, 25, 51). Closing the same share gap with different slopes:

| HCR-LCG at HI | Paper (LI–HI slope) | LI–MI slope | MI–HI slope | Logit | Log-linear |
|---|---|---|---|---|---|
| Compensation (thousand Rs) | 35.9 | 23.6 | 45.4 | 12.5 | 245 |

A conditional logit with caste intercepts and caste-specific income slopes fits the nine HC shares well
(deviance 7.6 on 4 df) and puts the HI compensation at Rs 12,500; a log-linear share model puts it at
Rs 245,000. The paper's number is one point in a range that spans a factor of twenty.
Source: [`output/audit_table6_segment_slopes.md`](../output/audit_table6_segment_slopes.md),
[`output/audit_table6_logit.md`](../output/audit_table6_logit.md),
[`output/audit_table6_alternative_forms.md`](../output/audit_table6_alternative_forms.md).

**C4. Extrapolation beyond support.** Implied incomes: Rs 71,000 (HCR-LCG HI), 62,000 (HCR-MCG HI),
56,000 (HCR-LCG LI). Among 1,261 real ads reporting income, the 99th percentile is Rs 45,000 and the maximum
Rs 50,000. No real advertisement carries the income the compensation figures describe; the design cannot
say what letter share such an ad would draw. Source: [`output/audit_table6_support.md`](../output/audit_table6_support.md).

**C5. §4.4 responder-quality claim is not recoverable at the stated granularity.** Following the recipe
(PCA on ten attributes, first component, quartiles, 35 groom × quartile dummies): the first component
explains 16% of variance; within-quartile compensation figures range from −35 to +357 thousand rupees because
9-way splits of ~115 letters leave income slopes near zero. Splitting at the median instead, HC top-half
responders do show larger compensation than bottom-half (e.g. HCR-LCG HI: 130 vs −14), MC responders show
the reverse (17 vs 47). The direction the paper reports holds for HC in this coarser version, not for MC,
and neither is precise. Source: [`output/quality_compensation_wide.md`](../output/quality_compensation_wide.md),
[`output/quality_compensation_halves.md`](../output/quality_compensation_halves.md).

### D. Reproducibility and editorial

**D1. Table 2 does not reproduce from the supplied Excel file.** Row counts differ (HC 1,369 vs 1,368; MC
774 vs 768; income n 378 vs 375), aggregate income mean 15,867 vs 15,858, and no height convention gives the
published inch means. The Brahmin sheet contains 412 non-integer ages (e.g. 31.6214) and 1,267 non-integer
heights (e.g. 5.9050) that cannot be transcriptions of a printed ad; they look like regression-filled values.
The sheet used for the published Table 2 was evidently a different version. Source:
[`output/check_ads_sheets.md`](../output/check_ads_sheets.md), [`output/table2_ads.md`](../output/table2_ads.md).

**D2. Table 5, column (3) prints p-values in parentheses**, not standard errors as the note says (e.g.
"(0.477)" for MCG-MI is the p-value; the HC1 SE is 0.0311).

**D3. Height in Table 3.** The file stores 4 ft 11 in as 4.11; the paper's conversion reads it as 49 in.
Five letters affected; Table 3 means change in the second decimal only. This package converts correctly and
reports the 0.05–0.09 in discrepancy.

**D4. Draft history.** The May 2008 draft reported the trade-off as "1.6 additional responses per Rs 1,000"
and "an eightfold difference in income might be required". The 2010 draft replaced counts with shares and
reports Table 6 as "almost double". Both describe the same data; the change is presentational, with the 2008
estimate more strongly extrapolated.

## Rejected candidates

- *Stacked-row dependence inflates significance.* Checked (B3): clustered and multinomial SEs are within 12%
  of HC1, and no verdict changes. Not a finding.
- *Edition effects.* Checked (C1): shares are homogeneous across editions. Not a finding for claims 1–2.
- *Duplicate covariate profiles indicate duplicated rows.* Four pairs of letters share every reported
  covariate. Each pair has two responses to two different grooms, consistent with two genuinely similar
  letters or one family writing twice. Too few to matter; noted in the codebook.
- *The `maletype` string disagrees with `groomid`.* Checked in `prepare.R`: 0 mismatches on caste, income,
  or edition across 1,366 letters.

## Check matrix

| Check | Result |
|---|---|
| Row conservation (.dta → letters → stacked) | 12,294 → 1,366 → 12,294; per-cell counts identical |
| Sample flow 1,366 → 1,123 | Reproduced via `repeat` flag; rationale in text does not match (A3) |
| Tables 3, 4, 5 vs published | 149/149 cells match |
| Footnote tests | 20/21 match; fn 16 wrong (A2) |
| Table 6 | HC panels match; MC panel arithmetic (A1) |
| Table 2 | Not reproducible from supplied Excel (D1) |
| SE at letter level | Done; no change in verdicts (B3) |
| Design-level replication | Two editions homogeneous (C1) |
| Uncertainty on headline magnitudes | Added (B1, B2) |
| Functional form | Range spans 12.5k to 245k (C3) |
| Support / extrapolation | All implied incomes above real-ad 99th percentile (C4) |
| Unreported §4.4 regressions | Rebuilt; not stable (C5) |
| Estimand statement | Shares, not response probabilities (see table above) |
| Influence / leverage | Not applicable: saturated cell means; each letter is one count |
| Missing data | Covariates have 2–15% missing; not used in Tables 4–6 |
| Multiplicity | 21 tests in footnotes, none adjusted; verdicts robust enough that adjustment would not change the two headline claims |

## Untestable with this package

- Which letters were "non-unique" and which came from other castes (needs letter identifiers or texts).
- Differences between the 2010 draft and the published Economic Inquiry text.
- Ad-specific effects beyond the edition replication (would need multiple ads per cell).
- The version of the newspaper-ad file that produced Table 2.

## What the evidence supports

Families of every caste wrote mostly to their own caste's ads, and the number of higher-caste families
writing to a lower-caste groom rises steeply with his advertised income while the number of lower-caste
families writing to a higher-caste groom falls. Both facts are precisely estimated and robust. The paper's
translation into "how much income buys caste" equalises head counts across two of nine ads in a market of
hundreds, extrapolates from three income points without uncertainty, and observes no family's trade-off. A
defensible summary is: among 478 letters from Brahmin families, a Namasudra groom advertising Rs 35,000 drew
51 (10.7%) against the Brahmin groom's 110 (23.0%), and one advertising Rs 7,000 drew 5 (1.0%) against 86
(18.0%). No rupee figure follows from those counts.

## Research opportunities

1. **Re-run with several ads per cell and randomised page position** so ad-level shocks are separable from
   caste × income. Nine ads is the binding limitation; the cost is newspaper insertions, which the authors
   report as cheap.
2. **Recover the repeat letters.** If a family wrote to more than one ad, its choice set is observed. That
   is a within-family caste-income comparison, the design the paper says it wanted (fn 12).
3. **Vary income continuously** or at five or more levels so the compensation figure is an interpolation.
4. **Compare with Banerjee, Duflo, Ghatak and Lafortune (2013)**, who study the same newspaper's letters five
   years earlier with real advertisers; see the sibling repository
   [`caste-marriage-banerjee`](https://github.com/in-rolls/caste-marriage-banerjee). Their shortlisting model and this
   share model both produce an "income premium for crossing caste"; whether they agree is an open question.

## Limits of this audit

The target is the December 2010 final draft, not the journal pages. The two data files were used as
supplied; no letter texts or newspaper scans were available. Bootstrap intervals are percentile intervals
from 2,000 draws with seed 20260914. The conditional logit is fitted by direct maximum likelihood to the nine
cell counts (`src/audit_functional_form.R`); it is a share model, not a model of who writes.
