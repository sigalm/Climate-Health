#### Tables and Figures ####

library(ggplot2)
library(reshape2)
library(scales)
library(RColorBrewer)
library(ggthemes)
library(knitr)
library(gridExtra)

# in one sim you start with everyone starting at 1. then start with everyone at 0 but with some small risk of developing asthma

####### Make figures ######
theme_set(theme_few())
mycolors <- brewer.pal(n=n_asthma_states, name="Set3")

trace_no_fire <- as.data.frame(sim_no_fire$TR_absolute)
trace_no_fire$cycle <- factor(0:n_t)
trace_no_fire_long <- melt(data=trace_no_fire,id.vars="cycle", variable.name="state", value.name="number_of_people")

trace_fire_noIntervention <- as.data.frame(sim_fire_noIntervention$TR.absolute)
trace_fire_noIntervention$cycle <- factor(0:n.t)
trace_fire_noIntervention_long <- melt(data=trace_fire_noIntervention,id.vars="cycle", variable.name="state", value.name="number_of_people")

trace_fire_Intervention_s1 <- as.data.frame(sim_fire_universalIntervention$TR.absolute)
trace_fire_Intervention_s1$cycle <- factor(0:n.t)
trace_fire_Intervention_s1_long <- melt(data=trace_fire_Intervention_s1,id.vars="cycle", variable.name="state", value.name="number_of_people")

trace_fire_Intervention_s2 <- as.data.frame(sim_fire_50percentIntervention$TR.absolute)
trace_fire_Intervention_s2$cycle <- factor(0:n.t)
trace_fire_Intervention_s2_long <- melt(data=trace_fire_Intervention_s2,id.vars="cycle", variable.name="state", value.name="number_of_people")

trace_fire_Intervention_s3 <- as.data.frame(sim_ruralfire_universalIntervention$TR.absolute)
trace_fire_Intervention_s3$cycle <- factor(0:n.t)
trace_fire_Intervention_s3_long <- melt(data=trace_fire_Intervention_s3,id.vars="cycle", variable.name="state", value.name="number_of_people")


trace_fire_Intervention_s4 <- as.data.frame(sim_ruralfire_50percentIntervention$TR.absolute)
trace_fire_Intervention_s4$cycle <- factor(0:n.t)
trace_fire_Intervention_s4_long <- melt(data=trace_fire_Intervention_s4,id.vars="cycle", variable.name="state", value.name="number_of_people")


fires <- colSums(m.fire)



fig_fire_Intervention_s1_zoom <- ggplot(trace_fire_Intervention_s1_long[trace_fire_Intervention_s1_long$state!="0", ], aes(x = cycle, y = number_of_people, group=state, fill = state, order = dplyr::desc(state))) +
  geom_area(alpha = .6) +
  geom_line(position = "stack", size = .2) +
  labs(title = "Health states over time, with fires, with intervention (S1)", x="cycle", y="Number in state") +
  scale_fill_manual(values=mycolors[-1], labels=states[-1]) +
  geom_vline(xintercept=which(fires>=1)+1)+
  theme(legend.text= element_text(size=20))


states <- c('No asthma',
            'Completely controlled',
            'Well-controlled',
            'Somewhat controlled',
            'Poorly controlled',
            'Not controlled at all',
            'Death - asthma',
            'Death - other cause')


fig_no_fire <- ggplot(trace_no_fire_long, aes(x = cycle, y = number_of_people, group=state, fill = state, order = dplyr::desc(state))) +
  geom_area(alpha = .6) +
  geom_line(position = "stack", size = .2) +
  labs(title = "Health states over time, no fires", x="cycle", y="Number in state") +
  scale_fill_manual(values=mycolors, labels=states) 


fig_no_fire_zoom <- ggplot(trace_no_fire_long[trace_no_fire_long$state!="0", ], aes(x = cycle, y = number_of_people, group=state, fill = state, order = dplyr::desc(state))) +
  geom_area(alpha = .6) +
  geom_line(position = "stack", size = .2) +
  labs(title = "Health states over time, no fires", x="cycle", y="Number in state") +
  scale_fill_manual(values=mycolors[-1], labels=states[-1]) +
  theme(legend.text= element_text(size=20))
fig_no_fire_zoom


fig_fire_noIntervention <- ggplot(trace_fire_noIntervention_long, aes(x = cycle, y = number_of_people, group=state, fill = state, order = dplyr::desc(state))) +
  geom_area(alpha = .6) +
  geom_line(position = "stack", size = .2) +
  labs(title = "Health states over time, with fires, no intervention", x="cycle", y="Number in state") +
  scale_fill_manual(values=mycolors, labels=states) +
  geom_vline(xintercept=which(fires>=1)+1)

fig_fire_noIntervention_zoom <- ggplot(trace_fire_noIntervention_long[trace_fire_noIntervention_long$state!="0", ], aes(x = cycle, y = number_of_people, group=state, fill = state, order = dplyr::desc(state))) +
  geom_area(alpha = .6) +
  geom_line(position = "stack", size = .2) +
  labs(title = "Health states over time, with fires, no intervention", x="cycle", y="Number in state") +
  scale_fill_manual(values=mycolors[-1], labels=states[-1]) +
  geom_vline(xintercept=which(fires>=1)+1)+
  theme(legend.text= element_text(size=20))




fig_fire_Intervention <- ggplot(trace_fire_Intervention_long, aes(x = cycle, y = number_of_people, group=state, fill = state, order = dplyr::desc(state))) +
  geom_area(alpha = .6) +
  geom_line(position = "stack", size = .2) +
  labs(title = "Health states over time, with fires, with intervention", x="cycle", y="Number in state") +
  scale_fill_manual(values=mycolors, labels=states) +
  geom_vline(xintercept=which(fires>=1)+1)

fig_fire_Intervention_s1_zoom <- ggplot(trace_fire_Intervention_s1_long[trace_fire_Intervention_s1_long$state!="0", ], aes(x = cycle, y = number_of_people, group=state, fill = state, order = dplyr::desc(state))) +
  geom_area(alpha = .6) +
  geom_line(position = "stack", size = .2) +
  labs(title = "Health states over time, with fires, with intervention", x="cycle", y="Number in state") +
  scale_fill_manual(values=mycolors[-1], labels=states[-1]) +
  geom_vline(xintercept=which(fires>=1)+1)+
  theme(legend.text= element_text(size=20))


p=list(fig_no_fire,
       fig_fire_noIntervention,
       fig_fire_Intervention,
       fig_no_fire_zoom,
       fig_fire_noIntervention_zoom,
       fig_fire_Intervention_zoom)

ggsave(
  filename = "plots.pdf", 
  plot = marrangeGrob(p, nrow=1, ncol=1), 
  width = 15, height = 9
)

############ Cost-effectiveness analysis #############

# store the mean costs (and the MCSE) of each strategy in a new variable v.C (vector costs)
v.C  <- c(sim_fire_noIntervention$tc_hat, sim_fire_Intervention$tc_hat) 
se.C <- c(sd(sim_fire_noIntervention$tc), sd(sim_fire_Intervention$tc)) / sqrt(n.i)
# store the mean QALYs (and the MCSE) of each strategy in a new variable v.E (vector health outcomes)
v.E  <- c(sim_fire_noIntervention$te_hat, sim_fire_Intervention$te_hat)
se.E <- c(sd(sim_fire_noIntervention$te), sd(sim_fire_Intervention$te)) / sqrt(n.i)

delta.C <- v.C[2] - v.C[1]                   # calculate incremental costs
delta.E <- v.E[2] - v.E[1]                   # calculate incremental QALYs
# se.delta.E <- sd(sim_no_fire$te - sim_fire$te) / sqrt(n.i) # Monte Carlo squared error (MCSE) of incremental costs
# se.delta.C <- sd(sim_no_fire$tc - sim_fire$tc) / sqrt(n.i) # Monte Carlo squared error (MCSE) of incremental QALYs
ICER    <- delta.C / delta.E                 # calculate the ICER
results <- c(delta.C, delta.E, ICER)         # store the values in a new variable


# Create full incremental cost-effectiveness analysis table
table_micro <- data.frame(
  c(round(v.C, 0)),           # costs per arm
  # c(round(se.C, 0), ""),           # MCSE for costs
  c(round(v.E, 3)),           # health outcomes per arm
  # c(round(se.E, 3), ""),           # MCSE for health outcomes
  c("", round(delta.C, 0)),  # incremental costs
  # c("", round(se.delta.C, 0),""),  # MCSE for incremental costs
  c("", round(delta.E, 3)),  # incremental QALYs 
  # c("", round(se.delta.E, 3),""),  # MCSE for health outcomes (QALYs) gained
  c("", round(ICER, 0))   # ICER
)
rownames(table_micro) <- v.intn  # name the rows
colnames(table_micro) <- c("Costs",   "QALYs", "Incremental Costs", "QALYs Gained", "ICER") # name the columns
kable(table_micro)  # print the table 
######

mean_costs <- melt(colMeans(sim$m.C, na.rm=TRUE), value.name = "mean_cost") 
mean_costs$cycle <- factor(0:n.t)

fig_costs <- ggplot(data=mean_costs, aes(x=cycle, y=mean_cost)) +
  geom_point() +
  labs(title="Mean cost per person per cycle", x="time",y="mean annual cost ($/year)")
# ggsave("wildfire_costs.tiff",fig_costs,units = "in",w=10,h=7)

tot_qalys <- melt(colSums(sim$m.E), value.name="tot_qalys")
tot_qalys$cycle <- factor(0:n.t)
tot_qalys$cum_qalys_lost <- 100-tot_qalys$tot_qalys


fig_qalyslost <- ggplot(data=tot_qalys, aes(x=cycle, y=cum_qalys_lost)) +
  geom_point() +
  labs(title="Total QALYs lost per cycle", x="time",y="total QALYs lost")
# ggsave("wildfire_qalys_lost.tiff",fig_qalyslost,units = "in",w=10,h=7)

