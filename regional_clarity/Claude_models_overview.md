# Regional Clarity RS Model — overview of Claude's work

Six reports, built in sequence, all predicting Secchi disk depth (SDD, water clarity) from Landsat surface reflectance across a 6-state Mountain West region. Each one answers a question raised by the one before it: build the pipeline → close two deferred threads → find out why two partitions perform badly → find out if that's a fluke of one split → rebuild on a real ensemble of splits → test whether a different cross-sensor reference (and a smaller, simpler sensor set) helps. This is the top-line takeaway and supporting figures for each — click through to "Full report" for the complete write-up.

All reports live under `regional_clarity/` on branch `outlier-removal-rework`.

---

## 1. The foundational model

**[Full report](outlier_rework/python/report.html)**

Built the modeling pipeline from scratch: training-data construction (5-day Landsat/field-sample matching, 12,637 rows), cross-sensor harmonization, feature engineering, and a three-way model comparison (XGBoost, LightGBM, a feedforward neural net) under 5-fold HUC4 spatial CV. This is stored in the `outlier_rework` folder.

**Takeaway:** the final, properly-tuned production config (16-feature intersection set, gap-aware hyperparameters, SDD-weighted for the tree models) reached test RMSE **1.86m (XGBoost) / 1.83m (LightGBM) / 1.84m (NN)**, R²≈0.45–0.48. This careful, regularized final model actually did slightly worse on the held-out test partition than an earlier, richer 33-feature/unweighted checkpoint (RMSE 1.747m, R²=0.508). Of note, there is significant benchin at higher observed Secchi. We dig into why this might be happening in the subsequent reports.

![pred vs obs](outlier_rework/python/results/figures/pred_vs_obs.png)

![weighted vs unweighted](outlier_rework/python/results/figures/weighted_vs_unweighted_rmse.png)

Note scale changes per figure above.

![example time series](outlier_rework/python/results/figures/timeseries_examples.png)

---

## 2. Adding gap aware tuning and adding shoreline proximity flag

**[Full report](outlier_rework_v2/python/report.html)**

Closed two threads v1 identified as opportunities: gap-aware hyperparameter selection applied at *every* tuning stage (not just the final one), and a shoreline-proximity flag added as a candidate feature. The gap-aware hyperparameter selection is a method where models that perform within 2% of the best validation RMSE are re-ranked according to the difference between the traning and validation RMSE. This should help root out models where there is over-training and over-parameterization. This is stored in the `outlier_rework_v2` folder.

**Takeaway:** the shoreline flag survived backward elimination in all 3 models. These changes resulted in a negligible positive impact across the tree-based models (XGBoost 1.860→1.839m, LightGBM 1.830→1.815m) and a slightly worse result for the NN (1.839→1.888m). Optical signal still dominates attribution (61–68%). These two threads will be retained in future work.

| | |
|---|---|
| ![pred vs obs](outlier_rework_v2/python/results/figures/pred_vs_obs.png) | ![shap group contribution](outlier_rework_v2/python/results/figures/shap_group_contribution.png) |

![example time series](outlier_rework_v2/python/results/figures/timeseries_examples.png)

---

## 3. Why do two partitions of the TVT split perform wose than others?

**[Full report](outlier_investigation/report.html)**

v1 and v2 both showed test partition 5 underperforming CV by more than expected. This report asks why partition 5 (test) and partition 3 (the worst CV fold) peformed poorly: is there something functionally different about these poor-performing partitions? This is stored in the `outlier_investigation` folder.

**Takeaway:** every model compresses predictions toward the dataset mean above ~6–10m SDD, which we suspect (and has historically been shown) is a real Landsat optical-retrieval limit, not a modeling defect) — partitions 3 and 5 concentrate that pattern 2–3x their proportional share. Partition 3 is 54% one forested, naturally-clear lake HUC4 (HUC4 1701); partition 5 is dominated by Lake Powell (HUC4 1407), which also sits at the 97th percentile of its own training data for catchment area. The reason these do not perform well is that the features exist at the edge of and beyond the training data envelope which exacerbates the Landsat signal problem - this is most clear in the example of Lake Powell whose catchment is many times larger than anything in the training and validation sets.

![pred vs obs, all basins](outlier_investigation/figures/overview_pred_vs_obs.png)

![Lake Powell pred vs obs](outlier_investigation/figures/lake_powell_pred_vs_obs.png)

![compression bias](outlier_investigation/figures/compression_bias.png)

![HUC4 test RMSE](outlier_investigation/figures/huc4_test_rmse.png) 

![example time series, including Lake Powell and HUC4 1701 sites](outlier_investigation/figures/overview_timeseries_examples.png)

---

## 4. Is the current poor performance in partitions a fluke of splitting the data?

**[Full report](partition_sensitivity/report.html)**

We tested whether partitions 3/5's problem is an artifact of the one HUC4-grouped split every prior report used. Reuses v1's frozen model (same 16 features, same hyperparameters) unchanged, so this report's own contribution is entirely about how RMSE moves under different split designs. Since the NN does not show any improvement over the tree-based methods, we do not continue buildiing NN models from this point forward. This is stored in the `partition_sensitivity` folder.

**Takeaway:** This is structural when using HUC4 as a split point. Rotating which HUC4 partition reproduces partitions 3 and 5 as consistently worst regardless of training/validation or test set. To try to address this, we attempted to paritition via HUC8 resolution with the same algorithm. When done with a single random seed, this just relocates the provlem (a different partition becomes worst). Randomizing HUC8 assignment order helps on average (~10–18% less rotation-to-rotation spread) but not for every seed. Conclusion: the only viable fix is an ensemble across many split assignments.

![rotation by partition](partition_sensitivity/figures/rotation_by_partition.png)

![scheme spread](partition_sensitivity/figures/scheme_spread.png)

![cluster concentration](partition_sensitivity/figures/cluster_concentration.png)

## 5. An ensemble-based rebuild

**[Full report](outlier_rework_v3/report.html)**

A full top-to-bottom rebuild on a genuine ensemble: one fixed holdout no model ever trains on, plus 5 independent HUC8-random CV-fold arrangements, each running its own complete feature-selection + tuning pipeline. This pipeline includes a new binary `shore_flag` feature. This is stored in the `outlier_rework_v3` folder.

**Takeaway:** marginally performs better than the initial foundational model RMSE 1.432→1.391m (−2.9%), MAE −4.5%. Importantly, v1's systematic over-prediction bias very nearly eliminated (+0.142m → +0.009m). `shore_flag` survives unanimously (10/10 seeds×models). SHAP confirms the model leans on physically sensible features and independently re-derives the Lake Powell finding from report 3 via a completely different method (its catchment-area importance is concentrated almost entirely at that one basin).

![pred vs obs](outlier_rework_v3/figures/v3_pred_vs_obs.png)

![controlled comparison](outlier_rework_v3/figures/v3_controlled_comparison.png)

![SHAP by HUC4](outlier_rework_v3/figures/v3_shap_by_huc4.png)

![SHAP aggregate](outlier_rework_v3/figures/v3_shap_aggregate.png) |

![example time series](outlier_rework_v3/figures/v3_timeseries_examples.png)

---

## 6. Harmonizing to LS8 as the reference sensor (LS7-LS9 mocel), and a Landsat 8/9 model

**[Full report](ls8_harmonization/report.html)**

Two questions from one shared 5-seed-ensemble setup: does referencing cross-sensor harmonization to Landsat 8 (instead of the Landsat 7 every prior report used) help? And can Landsat 8/9 data alone with no cross sensor correction carry a model? This is stored in the `ls8_harmonization` folder.

**Takeaway:** switching to an LS8 reference is a marginal improvement (RMSE −1.4%), driven partly by the blue band and green/red ratio becoming reliably useful only under that reference. This appears to be genuine new spectral information, not feature-swapping. This is at the necessary cost of dropping Landsat 4/5 (44% of the corpus, no LS5→LS8 coefficient exists upstream). Further, a LS8/9-only corpus (1,925 rows, 15% of the full data) is viable: typical-case accuracy (MAE, R²) matches or marginally improves from the full corpus, including at Lake Powell specifically, despite 73% less training data. The cost for a LS8/9 model is noisier feature selection, not necessarily worse predictions and not better predictions either.

![pred vs obs, all arms](ls8_harmonization/figures/pred_vs_obs_arms.png)

Note the continued benching performance above 6m.

![controlled comparison](ls8_harmonization/figures/controlled_comparison.png)

![HUC4 comparison A vs B](ls8_harmonization/figures/huc4_comparison_ab.png)

![LS8/9-only basin-matched](ls8_harmonization/figures/l89_basin_matched.png)

![example time series, LS8-reference config](ls8_harmonization/figures/ls8ref_timeseries_examples.png)


