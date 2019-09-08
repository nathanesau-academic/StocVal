---
title:	Embedded options in Cash Balance plan
author:	Nathan Esau
date:	June 21, 2015
---

# Executive summary

Three embedded guarantees in cash balance plans:

1. "money-back guarantee": return of participant pay credits at benefit
   commencement
2. "enhanced money-back guarantee": return of participant pay credits at benefit
   commencement with a fixed return which might be offered to participant to
   protect against inflation.
3. "cash-balance guarantee": provides a minimnum interest rate credit to be
   earned each year. For example, an interest credit rating that provides a
   yield on 30 year treasury bond but not less than some maximum, say 4.0% per
   annum.

(1) and (2) are valued using closed form adaptation of Black-Scholes formula,
and (3) is valued using Monte-Carlo simulation of risk-neutral rate (Hull
White)

# Introduction

2011 SOA paper accomplished three objectives:

1. Defined embedded options using two categories:
	* Behavior driven (e.g. retirement, termination)
	* Based on underlyingh financial phenomena
2. Drew parallels between embedded pension plan options and those in life
   insurance and financial markets
3. Provided results of a survey on prevalence of embedded options in DB plans.

Note that embedded option and guarantees refer to the same thing.

# Background

* Survey results from 2011 paper showed that embedded options are usually
  ignored because they are assumed to have zero or very little value
* Volatile capital markets have either increased value of some gauarantees or
  highlighted that these guarantees probably should not be ignored
* Economic landscape, regulatory environment and de-risking of plans is forcing
  pension plans to better understand the risk profile of their plan.
* Trends in actuarail standards are increasingly leaning toward reporting fair
  values or market-consistent present values for liabilities.
* Two cash balance plans (one with embedded option, one without) will not have
  the same value.
	* Money-back and enhanced money-back guarantees protect against decline in
	  account balance below sum of pay credits, equivalent to a put option on
	  account value with strike equal to sum of pay credit owned by plan
	  participant. Annual interest credit rate gauarantee can be viewed as an
	  interest rate floor with a strike equal to the minimum interest crediting
	  rate owned by plan participant. 
* Paper focus is not on valuing overall cash balance plan liability but just the
  guarantee portion.
 
# Cash balance plans

* Cash balance plan is a DB plan that provides participant with a periodic pay
  credit (commonly linked to salary or wages and usually tied to age or
  experience) and prescribes an interest credit rate that defines how
  accumulation of pay credits will evolve over time. The benefit under the cash
  balance plan is expressed as a lump sum, known as the "current account
  balance". 
* The cash balance plan benefit is a contingent liability of the plan sponsor.
  The contribution of pay credits is not required of the plan sponsor - the plan
  sponsor has the option of freezing the granting of future pay credits. 
* A cash balance plan has the look and feel of a defined contribution plan (DC)
  or even a simple bank account, but individuals accounts are not actually
  established. 
* The retirement benefit is a "hypothetical account" in which all pension assets
  for participants of plan are pooled together and managed collectively - the
  participant does not have control over how the plan's assets are invested.
  Cash balance plans are sometimes referred to as "hybrid" plans.

# Interest crediting rates

* Old regulations (prior to 2010) allowed interest crediting rates based on bond
  yields of varying maturities or the CPI - margins were also sometimes allowed.
* 2010 regulations introduced new interest credit rates (either of these):
	* Rates based on actual investment return - assets must be diversified to
	  minimize volatility, and mix of bonds and equities would be accetable.
	  Annuitiy contract rates could also be credited as are mutual funds with
	  low volatility.
	* Rates based on bond yields up to fixed rate of return of 5.0%.

# Guarantees

Two categories:

1. Guarantee a minimum cumulative rate of return when crediting rate is based on
   actual portfolio return or equity index only.
2. Guarantee minimum annual rate of return when crediting rate is bond based.

Money-back guarantee:

* Participants must receieve at least sum of their pay credits. Long put owned
  by plan participants and underwritten by plan sponsor, with
  guarantee level at benefit commencement being strike price and account balance
  being the underlying asset. 

Enhanced money-back guarantee:

* Interest crediting rates can apply a cumulative floor of up to 3.0% per year
  through distribution. If this guarantee is written into the plan, plan
  participants must receive at least their sum of pay credits with up to 3%
  interest applied each year until benefit commencement. Long put owned by plan
  participants and underwritten by plan sponsor with guarantee level at benefit
  commencement being strike price (at x% cumulative return) and account balance
  being the underlying asset.

Minimum annual interest rate guarantee:

* Bond yield rates can apply minimum interest crediting rate of up to 4.0%. Each
  year, a participant's account balance would be increased by the greater of the
  actual yield on the bond or the minimum floor rate. A series of long puts
  owned by plan participant and underwritten by plan sponsor - downside
  protection should interest rate fall below strike price and protection is
  offered every year - however, here payoff is based upon the balance (a
  stochastic variable). 

# Historical analysis of cash balance plan guarantees

* Size of money-back guarantee has evaluated using historical data. 
* One reason this option has not been seriously explored in past is belief that
  diversified portfolio will earn positive nominal returns over the long run and
  therefore the guarantee will be of no value.
* The volatility of annualized returns declines over time, but volatility of
  total compound returns rises over time.

