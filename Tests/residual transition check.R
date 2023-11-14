# Console from 9/12

identical(sim_fire_no_resid$TR_absolute, sim_fire_0.1_resid$TR_absolute)
figure_no_resid
figure_0.1_resid

no_resid_peak <- sim_fire_no_resid$TR_proportion[3, ]
resid0.1_peak <- sim_fire_0.1_resid$TR_proportion[3, ]
no_resid_peak
resid0.1_peak
sum(abs(no_resid_peak - resid0.1_peak))

no_fire_peak <- sim_no_fire$TR_proportion[3, ]

peaks_table <- rbind(no_fire_peak, resid0.1_peak, no_resid_peak)

diff1 <- sum(abs(no_resid_peak - no_fire_peak))
diff2 <- sum(abs(resid0.1_peak - no_fire_peak))

(diff1-diff2)/diff2
