# Figure 3
fig_RCDC_PT %>% ggsave(
  filename = "Results/Figures/EUPert_Fig3_RCDC_PT.svg",
  width = 7, height = 7)
fig_RCDC_PT %>% ggsave(
  filename = "Results/Figures/EUPert_Fig3_RCDC_PT.pdf",
  width = 7, height = 7)

# Figure S1
fig_GMC_PT %>% ggsave(
  filename = "Results/Figures/EUPert_FigS1_GMC_PT.svg",
  width = 7, height = 7*sqrt(2))
fig_GMC_PT %>% ggsave(
  filename = "Results/Figures/EUPert_FigS1_GMC_PT.pdf",
  width = 7, height = 7*sqrt(2))

# Figure 2A
fig_prev$`PT≥100` %>% ggsave(
  file = "Results/Figures/EUPert_Fig2A_prev_PT.svg",
  width = 7, height = 7*sqrt(2))
fig_prev$`PT≥100` %>% ggsave(
  file = "Results/Figures/EUPert_Fig2A_prev_PT.pdf",
  width = 7, height = 7*sqrt(2))

# Figure 2B
(fig_prev$`Dt<0.01` + fig_prev$`Dt<0.1` + plot_layout(guides = "collect") + plot_annotation(tag_levels = "a")) %>%
  ggsave(
    file = "Results/Figures/EUPert_Fig2B_prev_Dt.svg",
    width = 7*2, height = 7*sqrt(2))
(fig_prev$`Dt<0.01` + fig_prev$`Dt<0.1` + plot_layout(guides = "collect") + plot_annotation(tag_levels = "a")) %>%
  ggsave(
    file = "Results/Figures/EUPert_Fig2B_prev_Dt.pdf",
    width = 7*2, height = 7*sqrt(2))

# Figure 2C
(fig_prev$`TT<0.01` + fig_prev$`TT<0.1` + plot_layout(guides = "collect") + plot_annotation(tag_levels = "a")) %>%
  ggsave(
    file = "Results/Figures/EUPert_Fig2C_prev_TT.svg",
    width = 7*2, height = 7*sqrt(2))
(fig_prev$`TT<0.01` + fig_prev$`TT<0.1` + plot_layout(guides = "collect") + plot_annotation(tag_levels = "a")) %>%
  ggsave(
    file = "Results/Figures/EUPert_Fig2C_prev_TT.pdf",
    width = 7*2, height = 7*sqrt(2))
