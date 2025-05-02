method_order <- c('scGen', 'inVAE', 'trVAE', 'scVIDR','scPRAM', 'biolord',
                  'scDisInFact', 'CellOT', 'SCREEN', 'scPreGAN','trainMean', "controlMean", "baseMLP", 'baseReg')
color_value <- c("scGen" = 'lightpink', "scVIDR" = '#F5B215', "biolord" = '#A4D29C',
                 "CellOT" = 'lightskyblue', "inVAE" = 'mediumpurple', "trVAE" = '#6CBB53',
                 "scPRAM" = '#E2B484', "scPreGAN" = '#C65388',
                 "SCREEN" = '#8C1C37', "scDisInFact" = 'royalblue',
                 'trainMean' = '#E73118', "controlMean" = '#BBBBFC', "baseMLP" = 'aquamarine', 'baseReg' = 'hotpink1')
scala <- read.csv('/home/wsg/perturbation/reproducibility/website/data/scalability_cellular.csv')
scala$method <- ifelse(scala$method == 'bioLord', 'biolord', scala$method)
scala$method <- ifelse(scala$method == 'cellot', 'CellOT', scala$method)


### ------------- 四、cellular 可用性 -------------------------------------
scala <- read.csv('/home/wsg/perturbation/reproducibility/website/data/scalability_cellular.csv')
time <- scala[, c('Time', 'method', 'corp', 'group', 'counts')]
time2 <- time[, c('method', 'Time')] %>%
  group_by(method) %>%
  summarise(across(everything(), mean)) %>%
  data.frame()
time2$min <- time2$Time/60
time2 <- time2 %>%
  mutate(Rank = rank(min, ties.method = "min")) %>%
  data.frame()
time2$method <- factor(time2$method, levels = rev(method_order))
time2$min <- round(time2$min, 1)
p=ggplot(data = time2) +
  geom_point(aes(x = 1, y = method,
                 size = 4, fill = Rank),
             color = 'grey20', shape = 21) +
  #geom_text(data = filter(time2, Rank_all <= 3),
  geom_text(data = time2,
            aes(x = 1, y = method,label = min)) +
  scale_fill_gradient(low = "white", high = "brown") +
  # scale_fill_gradientn(colors = c("red", "white", "cyan"),
  #                       values = scales::rescale(c(-1, 0, 2))) +
  # wyh_theme() +
  labs(x = 'Time', y = 'Methods', title = '') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
ggsave('./scalability/time_merge.pdf', p, width = 4, height = 3.5, limitsize = FALSE)





cpu <- scala[, c('CPU_memory', 'method', 'corp')]
cpu2 <- cpu[, c('method', 'CPU_memory')] %>%
  group_by(method) %>%
  summarise(across(everything(), mean)) %>%
  data.frame()
cpu2 <- cpu2 %>%
  mutate(Rank = rank(CPU_memory, ties.method = "min")) %>%
  data.frame()
cpu2$method <- factor(cpu2$method, levels = rev(method_order))
cpu2$Memory <- cpu2$CPU_memory/1024/1024
cpu2$Memory <- round(cpu2$Memory, 1)
p=ggplot(data = cpu2) +
  geom_point(aes(x = 1, y = method,
                 size = 4, fill = Rank),
             color = 'grey20', shape = 21) +
  #geom_text(data = filter(time2, Rank_all <= 3),
  geom_text(data = cpu2,
            aes(x = 1, y = method,label = Memory)) +
  scale_fill_gradient(low = "white", high = "brown") +
  # scale_fill_gradientn(colors = c("red", "white", "cyan"),
  #                       values = scales::rescale(c(-1, 0, 2))) +
  # wyh_theme() +
  labs(x = 'Memory', y = 'Methods', title = '') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
ggsave('./scalability/memory_merge.pdf', p, width = 4, height = 3.5, limitsize = FALSE)


time <- scala[, c('Time', 'method', 'corp', 'group', 'counts')]
time$min <- time$Time/60
time$method <- factor(time$method, levels = rev(method_order))

time_cell <- filter(time, time$corp == 'cell')
time_cell$group <- factor(time_cell$group, levels = c('cell1000', 'cell5000', 'cell10000', 'cell50000'))
p1 = ggplot(time_cell, aes(x = group, y =min, color=method, group = method)) +
  geom_line(size = 1)+
  geom_point(size = 3)+
  labs(x = 'cell counts')+
  scale_color_manual(values = rev(color_value))
  #scale_y_continuous(breaks = c(0, 1500), limits = c(0, 1500)) +
  # scale_y_break(c(200, 1100))  + # 在Y轴的30到80之间创建断裂
  # wyh_theme()
p1




time_cell <- filter(time, time$corp == 'perturb')
time_cell$group <- factor(time_cell$group, levels = c('perturb1', 'perturb3', 'perturb9'))
time_cell$method <- factor(time_cell$method, levels = method_order)
time_cell<- filter(time_cell,time_cell$method!='SCREEN')
p1 = ggplot(time_cell, aes(x = group, y =min, color=method, group = method)) +
  geom_line(size = 1)+
  geom_point(size = 3)+
  labs(x = 'perturb counts')+
  scale_color_manual(values = color_value) +
  #scale_y_continuous(breaks = c(0, 1500), limits = c(0, 1500)) +
  #scale_y_break(c(600, 900))  +# 在Y轴的30到80之间创建断裂
  wyh_theme()
p1

time_cell2 <- filter(time_cell, time_cell$method != 'SCREEN')
time_cell2 <- filter(time_cell2, time_cell2$method != 'scPreGAN')
time_cell2 <- filter(time_cell2, time_cell2$method != 'cellot')
p2 = ggplot(time_cell2, aes(x = group, y =min, color=method, group = method)) +
  geom_line(size = 1)+
  geom_point(size = 3)+
  labs(x = 'perturb counts')+
  scale_color_manual(values = color_value) +
  wyh_theme()
p2
p1+p2
ggsave('./scalability/time_line_cell.pdf', p1+p2, width = 12, height = 4, limitsize = FALSE)







cpu <- scala[, c('CPU_memory', 'method', 'corp', 'group', 'counts')]
cpu$Memory <- cpu$CPU_memory/1024/1024
cpu$method <- factor(cpu$method, levels = rev(method_order))

cpu_cell <- filter(cpu, cpu$corp == 'cell')
cpu_cell$group <- factor(cpu_cell$group, levels = c('cell1000', 'cell5000', 'cell10000', 'cell50000'))
p1 = ggplot(cpu_cell, aes(x = group, y =Memory, color=method, group = method)) +
  geom_line(size = 1)+
  geom_point(size = 3)+
  labs(x = 'cell counts')+
  scale_color_manual(values = rev(color_value)) +
  #scale_y_continuous(breaks = c(0, 1500), limits = c(0, 1500)) +
  #scale_y_break(c(200, 1100))  +# 在Y轴的30到80之间创建断裂
  wyh_theme()
p1




cpu_cell <- filter(cpu, cpu$corp == 'perturb')
cpu_cell$group <- factor(cpu_cell$group, levels = c('perturb1', 'perturb3', 'perturb9'))
cpu_cell$method <- factor(cpu_cell$method, levels = method_order)


p2 = ggplot(cpu_cell, aes(x = group, y =Memory, color=method, group = method)) +
  geom_line(size = 1)+
  geom_point(size = 3)+
  labs(x = 'perturb counts')+
  scale_color_manual(values = color_value) +
  wyh_theme()
p2
p1+p2
ggsave('./scalability/memory_line.pdf', p1+p2, width = 12, height = 4, limitsize = FALSE)


