# Amazon Package Pick-Time Analysis

**Matthew Scarpello · Bellevue University · Applied data visualization · January 2026**

An R-based simulation and operational visualization project inspired by my experience as an Amazon delivery driver. I wanted to understand the workflow of organizing packages by type and picking them as needed, rather than relying on intuition alone.

[Explore the portfolio case study](https://mjscarpello.github.io/Portfolio-Repository/school/amazon-pick-times.html) · [Read the original report (PDF)](https://mjscarpello.github.io/Portfolio-Repository/downloads/amazon-workflow-efficiency-report.pdf)

## Selected visualizations

![Box and envelope pick-time distributions](figures/amazon-pick-distribution.png)

The original report's **modeled** mean pick times were 4.2 seconds for boxes and 5.6 seconds for envelopes. These figures are outputs of the simulated workflow, not verified field measurements.

| Figure | Purpose |
| --- | --- |
| [Violin plot](figures/amazon-pick-distribution.png) | Compare package-type distributions and variability. |
| [Weekly histograms](figures/amazon-pick-weekly-comparison.png) | Examine modeled patterns across weeks. |
| [Rolling dashboard](figures/amazon-pick-rolling-dashboard.png) | Demonstrate rolling means and route-level visualization. |

## Repository structure

- [Simulation scripts](analysis/02-monthly-simulation/) — two alternative R generator versions.
- [Visualization scripts](analysis/03-visualizations/) — violin plots, bag-level scatter plots, daily mean charts and workload share.
- [Figures](figures/) — selected original project graphics.
- [Reproducibility notes](docs/reproducibility.md) — dependencies, missing inputs and limitations.

This is an initial published subset of the original coursework archive.

## Data and methodology

Firsthand observations and continuous voice memos motivated the research question. The supplied R scripts generate monthly pick times using a gamma base distribution described in the original comments as calibrated to real observations, with seeded random sampling and adjustments for package type, bag progress, difficulty and disruptions. The raw observations, calibration parameters and initial daily controls are not included. The exact generator variant behind each image has not been established. The available scripts depend on objects from the original R session and are not a tested end-to-end pipeline.

## Next steps

A future study would compare both organization methods under comparable conditions, include initial sorting time, recruit consenting drivers, and evaluate whether a predictive model generalizes to new drivers and routes. No comparative causal finding or validated predictive model is claimed here.

This independent coursework project is not an official Amazon study. Private voice recordings and customer/package identifiers are not included.
