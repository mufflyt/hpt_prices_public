# Asking Trilliant Health what we may publish

Everything in this project that is derived from the Trilliant data stays in the private
repository, because their terms forbid building rate data for redistribution to third parties
(2.3(i), 2.3(iii)). That is why `hpt_prices` cannot be made public and why
`tools/export_public.sh` strips the figures, the methods documents, and the known answers out of
the public code copy.

The restriction is on redistribution, not on research. What it blocks is specific: publishing the
figures, the state and national medians, and a manuscript built on them. A permission covering
aggregates would remove that block without asking Trilliant to give up anything they sell, since
nobody can reconstruct a hospital's negotiated rates from a state median.

This file holds a draft of that request. **Nothing here has been sent.** Sending it is the owner's
decision, and the numbers below should be checked against the build of the day it goes out.

## What to ask for

1. **Publish aggregate results**: state and national medians by code and insurance type, the
   ratios to Medicare, the figures built from them, and the methods documents that quote them, in
   a public repository and in a peer-reviewed manuscript.
2. **Name Trilliant** as the data source in every one of them, which their attribution clause
   (2.2(b)) requires anyway.
3. **Confirm the boundary**: that per-hospital negotiated rates, the parsed lake, and any table
   from which a hospital's rates could be reconstructed stay unpublished.

## What not to ask for, and to say so plainly

- No redistribution of the DuckLake archive, in whole or in part.
- No per-hospital rate table, no hospital-level appendix, no supplementary file of rates.
- No API or bulk feed built on their data.
- No commercial use.

## Draft message

> Subject: Permission to publish aggregate results from the Hospital MRF Data Directory
>
> I am an obstetrician-gynecologist and health services researcher. Using the free Oria "Full Data
> Download" of the Hospital MRF Data Directory (snapshot 2026-07-21), I have built an analysis of
> hospital prices for a set of gynecologic and obstetric procedures: colonoscopy, endometrial
> biopsy, IUD insertion, vaginal hysterectomy, bariatric surgery, and childbirth.
>
> The work is research, not a product. Two questions it answers are whether a low-value add-on
> procedure pays for the operating-room time it takes, and how a hospital's cesarean price compares
> with its vaginal delivery price.
>
> Your terms forbid building rate data for redistribution (2.3(i), 2.3(iii)), so everything derived
> from your data currently stays in a private repository, and the public copy of the code carries
> no prices, figures, or results. I would like your permission to publish a narrow class of output:
>
> - state-level and national medians by procedure code and insurance type, and their ratio to the
>   Medicare rate;
> - figures built from those aggregates;
> - the methods documentation that quotes them;
> - a peer-reviewed manuscript reporting them.
>
> I am not asking to redistribute the archive, to publish per-hospital negotiated rates, or to
> publish any table from which a hospital's rates could be reconstructed. Those would stay private
> whatever you decide. Trilliant Health would be named as the data source in the repository, every
> figure, and the manuscript.
>
> If it helps, I can send the exact tables and figures I would publish before anything goes out.
>
> If aggregates are acceptable but a particular cut is not, I would rather hear the boundary than
> guess at it.

## Before sending

- Check the headline numbers against the current build; they have moved twice already
  (per-diem conversion, then the APR-DRG and CCN-matching changes).
- Attach or link the specific figures and tables, so the request is concrete rather than a
  category.
- Ask where a manuscript should cite the snapshot date and version.

## If the answer is no, or no answer comes

The project continues unchanged: the code stays public, the data-derived material stays private,
and a manuscript would report aggregates only if that becomes permissible. Nothing in the pipeline
depends on the answer.
