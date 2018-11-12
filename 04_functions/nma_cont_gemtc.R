library(tidyverse)
library(forcats)
library(stargazer)
library(stringr)
library(grid)
library(reshape2)
library(netmeta)
library(gemtc)
library(gridExtra)
library(R2jags)
library(R2WinBUGS)
library(scales)
library(forestplot)

prep_gem = function(data, trt_group = "trt_group"){
  data$trt_group = fct_infreq(data[trt_group][[1]]) %>% droplevels(data)#reorders factor from most to least common
  
  data = droplevels(data) # Drops factor levels that don't exist (otherwise they are carried over)
  long_gemtc(data = data)
  
}


#============================================================================================
#
#
#             ---- Arrange long data into winbugs format -----
#
#
#============================================================================================

#data = pa_reac, a data frame limited to the outcome of interest



long_gemtc = function(data = pa_reac,studlab = "studlab", trt = "trt_group",mean = "mean",sd = "std_dev",sample_size = "samplesize",studylevel = rop_data_study){
  
  (input = data %>% select(studlab,trt_group,mean,std_dev,sample_size,p_value) %>% 
     rename(y = mean,
            n = sample_size,
            sd = std_dev) %>%  
     arrange(studlab,trt_group) %>% ##Arranges treatments by factor level
     group_by(studlab) %>%
     mutate(arm = row_number()) %>% rename(treatment = trt_group)
  )
  
 
data = input %>% group_by(studlab) %>% mutate(y = y - first(y)) %>% mutate(y = ifelse(arm == 1,NA,y)) %>%
    mutate(std.err = ifelse(arm == 1, round(sd/sqrt(n),2),
                            se_md(first(sd),sd,first(n),n))) %>% left_join(studylevel %>% select(studlab,design), by = studlab) %>%
  mutate(std.err = ifelse(design == "Crossover" & arm != 1 & !is.na(p_value),se_paired(y,p_value,first(n)),std.err)) %>% mutate(narm = max(arm)) %>%
  mutate(std.err = ifelse(narm == 2 & arm == 1, NA, std.err)) %>%
  rename(diff = y,
         study = studlab) %>% select(study,treatment,diff,std.err) %>% as.data.frame()
    
  ###Output a list

(wide= input %>%
    recast(studlab ~ variable + arm, id.var = c(studlab,"arm"))
)
  
list(input = input,data = data, wide = wide)
}

library(ggnetwork)
library(sna)
library(igraph)
library(intergraph)

pub_netgraph = function(chars,nodecolour = "mediumorchid2", layout = "circle"){
  
  chars_net = chars$int_char %>% select(Treatment, `Number of Patients`) %>% rename(sample = `Number of Patients`)
  
  df = chars$direct %>% rename(weight = `# Studies`) %>% select(Treatment, weight) %>% separate(Treatment,c("from","to"), sep = " vs ")
  df = graph.data.frame(df)
  V(df)$size = as.numeric(chars_net[match(V(df)$name,chars_net$Treatment),2][[1]])
  
  legend = data.frame(treat = V(df)$name, letter = LETTERS[as.factor(V(df)$name)]) %>% mutate(legend = paste(letter,": ",treat,sep = "")) %>% select(legend) %>%
    arrange(legend)
  
  network = ggnetwork(df, layout = layout)
  
  ggplot(network, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(aes(size = weight), color = "grey50", alpha = 0.8) +
    geom_nodes(aes(size = size,colour = vertex.names)) + 
    theme_blank() + 
    geom_edgelabel_repel(aes(label = weight), color = "grey25") +
    geom_nodetext(aes(label = LETTERS[vertex.names]))+
    scale_size_area("sample", max_size = 32,guide = FALSE) + scale_colour_manual(name = "Treatment", values = rep(nodecolour,length(network$x)),
                                                                                 labels = legend[[1]]) + theme(legend.position = "bottom",
                                                                                                               legend.text = element_text(size = 14))
  

}





#===================================================================================================


#Calculate SUCRA----
# Adapted from Dias WinBUGS codes

sucra = function(results,direction){
  rank = rank.probability(results, preferredDirection = direction)
  rank = rank[,1:length(rank[1,])]
  cumeffectiveness = rank
  SUCRA = rank[,1]
  for(k in seq_along(rank[,1])){
    for(h in seq_along(rank[1,])){
      cumeffectiveness[k,h] = sum(rank[k,1:h])
    }
  }
  
  for(i in seq_along(cumeffectiveness[,1])){
    SUCRA[i] = sum(cumeffectiveness[i, 1:(length(cumeffectiveness[,1]) - 1)])/(length(cumeffectiveness[,1]) - 1)
  }
  SUCRA
}


#=========== =
# Interrfacing with JAGS/WB #====


long_jags_md = function(data, studlab = "studlab", cont = TRUE){
  
  if(cont == TRUE){
  input = data %>% select(studlab,trt,mean,sd,n)  %>%
    rename(y = mean) %>%
    arrange(studlab,trt) %>%
    group_by(studlab) %>%
    mutate(arm = row_number(),
           t = as.numeric(trt),
           na = n())} else {
             
             input = data %>% select(studlab,trt,events,n)   %>%
               rename(r = events) %>%
               arrange(studlab,trt) %>%
               group_by(studlab) %>%
               mutate(arm = row_number(),
                      t = as.numeric(trt),
                      na = n())
             
           }
  
  treatments = input %>% ungroup() %>% select(trt,t) %>% distinct() %>% arrange(t) %>% rename(description = trt)
  
  wide = input %>% select(-trt) %>%
    recast(studlab ~ variable + arm, id.var = c("studlab","arm")) %>% select(studlab:na_1) %>% rename(na = na_1)
  
  
  if(cont == TRUE){
  
  for(i in 1:length(wide %>% select(starts_with("sd_")))){
    
    wide[paste("se_",i,sep= "")] = round(wide[paste("sd_",i,sep= "")]/sqrt(wide[paste("n_",i,sep= "")]),2)
  }
    list(input = input, treatments = treatments, wide = wide %>% arrange(na))
  } else {
    
    list(input = input, treatments = treatments, wide = wide %>% arrange(na))
  }
  
  
}

#================
# Create data list
#===============

wb_cont_list = function(data = wb$wide, cont = TRUE,contrast =FALSE){
  if(contrast == TRUE){
    output = list(
      t = as.matrix(data %>% select(starts_with("t_"))),
      y = cbind(rep(NA,nrow(data)),as.matrix(data %>% select(starts_with("y_")))),
      se = cbind(rep(NA,nrow(data)),as.matrix(data %>% select(starts_with("se_")))),
      na = (data %>% select(na))[[1]],
      ns2 = nrow(data %>% filter(na == 2)),
      ns3 = nrow(data %>% filter(na == 3)),
      ns4 = nrow(data %>% filter(na == 4)),
      nt = nrow(data %>% select(starts_with("t_")) %>% gather(val,var) %>% distinct(var) %>% arrange(var) %>% drop_na()),
      V = as.vector(data$V)
    )
  }else if(cont == TRUE){
    
    output = list(
      t = as.matrix(data %>% select(starts_with("t_"))),
      y = as.matrix(data %>% select(starts_with("y_"))),
      se = as.matrix(data %>% select(starts_with("se_"))),
      na = (data %>% select(na))[[1]],
      ns = nrow((data %>% select(starts_with("t_")))),
      nt = nrow(data %>% select(starts_with("t_")) %>% gather(val,var) %>% distinct(var) %>% arrange(var) %>% drop_na())
    )} else{
      
      output = list(
        t = as.matrix(data %>% select(starts_with("t_"))),
        r = as.matrix(data %>% select(starts_with("r_"))),
        n = as.matrix(data %>% select(starts_with("n_"))),
        na = (data %>% select(na))[[1]],
        ns = nrow(data),
        nt = nrow(data %>% select(starts_with("t_")) %>% gather(val,var) %>% distinct(var) %>% arrange(var) %>% drop_na())
      )
    }
  output
}
#============================================================
# Create basic outputs for tables - Consistency models
#============================================================
jags_out = function(model_summary = model$BUGSoutput$summary,treatments = lot_wb$treatments,
                    tables = c("meandiff","SUCRA"),est = c("Mean difference","SUCRA")){
  
  #Get summary statistics to make tables
  out_df = as.data.frame(model_summary) %>% rownames_to_column() %>% select(rowname,mean,`2.5%`,`50%`,`97.5%`) %>%
    separate(rowname,c("var","trt1","trt2"), extra = "drop") %>% 
    modify_if(is.numeric,~round(.,4)) %>% modify_at(c(2,3),as.numeric) %>%
    mutate(md = paste(`50%`," (",`2.5%`," to ",`97.5%`,")", sep = "")) %>% 
    arrange(as.numeric(trt1),as.numeric(trt2))
  
  #Make treatment list a data frame
  treatments = as.data.frame(treatments)
  
  #Use treatment dictionary to make labels
  out_df$trt1 = treatments[match(out_df$trt1,treatments$t),1]
  out_df$trt2 = treatments[match(out_df$trt2,treatments$t),1]
  
  #Table outputs
  res_tables = NULL
  
  for(i in 1:length(tables)){
    if(tables[[i]] == "SUCRA"){
      temp = out_df %>% filter(var == tables[[i]]) %>% select(trt1,mean)
      colnames(temp) = c("Treatment","SUCRA (mean)")} else{
        
        temp = out_df %>% filter(var == tables[[i]]) %>% select(trt1,trt2,md) %>% unite(comp,trt1,trt2, sep = " vs ")
        colnames(temp) = c("Treatment comparison",est[i]) 
      }
    
    res_tables[[i]] = temp
    names(res_tables)[[i]] = tables[[i]]
    
  }
  res_tables
}


#============================================================
# Create basic outputs for tables - Inconsistency models
#============================================================
inc_out = function(inc_summary,con_summary,treatments,outlier_thresh = 0.5,h_just = 1.2,v_just = 0,
                   log = TRUE, tables = "or", est = "Odds ratio"){
  
  
out_inc = as.data.frame(inc_summary) %>% rownames_to_column() %>% select(rowname,sd,`2.5%`,`50%`,`97.5%`) %>%
  separate(rowname,c("var","trt1","trt2"), extra = "drop") %>%
  modify_if(is.numeric,~round(.,2)) %>% modify_at(c(2,3),as.numeric)

if(log == TRUE){
  out_inc = out_inc %>% mutate(d = ifelse(var == "d",paste(round(exp(`50%`),2)," (",round(exp(`2.5%`),2)," to ",round(exp(`97.5%`),2),")", sep = ""),
         paste(`50%`," (",`2.5%`," to ",`97.5%`,")", sep = ""))) %>%
  arrange(as.numeric(trt1),as.numeric(trt2)) %>% filter(sd < 90, sd != 0)
} else{
  out_inc = out_inc %>% mutate(d = ifelse(var == "d",paste(round(`50%`,2)," (",round(`2.5%`,2)," to ",round(`97.5%`,2),")", sep = ""),
                                 paste(`50%`," (",`2.5%`," to ",`97.5%`,")", sep = ""))) %>%
    arrange(as.numeric(trt1),as.numeric(trt2)) %>% filter(sd < 90, sd != 0)
  
}


inc_treatments = as.data.frame(treatments)

  inc_md = out_inc %>% filter(var == "d") %>% select(trt1,trt2,d)
  inc_md["trt1"] = treatments[match(inc_md$trt1,treatments$t),1]
  inc_md["trt2"] = treatments[match(inc_md$trt2,treatments$t),1]

  inc_md = inc_md %>% unite(comp,trt1,trt2, sep = " vs ")
  colnames(inc_md) = c("Treatment comparison", est)
  
#=== Comparison table of consistency and inconsistency estimates
  cons = jags_out(model_summary = con_summary,treatments = treatments,
           tables = tables,est = est)

  comp_table = cons[tables][[1]] %>% right_join(inc_md, by = c("Treatment comparison"), suffix = c("_cons","_inc"))
  
#=== Deviance for plots
  cons_dev = as.data.frame(con_summary) %>% rownames_to_column() %>% select(rowname,mean) %>% separate(rowname,c("var","trt1","trt2"), extra = "drop") %>% filter(var == "dev") %>%
    modify_if(is.numeric,~round(.,2)) %>% modify_at(c(2,3),as.numeric) %>% select(trt1,trt2,mean) %>% arrange(trt1,trt2) %>% unite(comp,trt1,trt2, sep = ",")
  
  inc_dev = as.data.frame(inc_summary) %>% rownames_to_column() %>% select(rowname,mean) %>% separate(rowname,c("var","trt1","trt2"), extra = "drop") %>% filter(var == "dev") %>%
    modify_if(is.numeric,~round(.,2)) %>% modify_at(c(2,3),as.numeric) %>% select(trt1,trt2,mean) %>% arrange(trt1,trt2) %>% unite(comp,trt1,trt2, sep = ",")
  
  dev_data = cons_dev %>% right_join(inc_dev,by = "comp", suffix = c("_con","_inc")) %>% mutate(outlier = ifelse(abs(mean_inc-mean_con) > outlier_thresh,paste("[",comp,"]",sep = ""),NA))

  dev_plot = dev_data %>% ggplot(aes(x = mean_con,y = mean_inc)) + geom_point() + geom_abline(intercept = 0, slope = 1) + labs(title = "Deviance plot inconsistency vs consistency",
                                                                                                                    x = "Deviance (consistency model)",
                                                                                                                    y = "Deviance (inconsistency model)") +
    geom_text(aes(label = outlier),hjust = h_just) +
    ylim(0,max(dev_data$mean_inc)+1) +
    xlim(0,max(dev_data$mean_con)+1)
  
  out = list(comp_table = comp_table, plot = dev_plot)

  }
  

#===================================================== =
#===================================================== =
#Study characteristics tables ==========================
#===================================================== =
#===================================================== =
  
netmeta_xl_chars = function(data,treat = "treatment",location = getwd(),cont = TRUE,event = NULL){
  
  if(cont == TRUE){  
    contrast = pairwise(data = data,treat = data[[treat]], n= n, mean = mean,sd = sd,studlab = studlab)} else{
      
      contrast = pairwise(data = data,treat = data[[treat]], n= n, event = data[[event]],studlab = studlab)
    }
  
  
  
  studies = (data %>% select(studlab) %>% distinct() %>% count())[[1]] ## count the number of studies
  trts = (data %>% select(treat) %>% distinct() %>% count())[[1]] ## count the number of treatments
  totn = data %>% select(n) %>% sum() # count the number of patients
  poss_pw = choose(trts,2) # total possible pairwise comparisons
  
  
  #-- Create a numbered list of treatments with ref as #1--#
  data[[treat]] = fct_infreq(data[[treat]])
  data = droplevels(data)
  names= (data %>% select(treat) %>% distinct())[[treat]]
  
  names = tibble("Treatment Number" = as.numeric(names),
                 "Treatment Description" = names) %>% arrange(`Treatment Number`)
  
  
  #- Assign treatment numbers based on tibble above -#
  contrast = names %>% rename(treat2_asnum = `Treatment Number`,
                              treat2 = `Treatment Description`) %>% right_join(contrast, by = "treat2")
  contrast = names %>% rename(treat1_asnum = `Treatment Number`,
                              treat1 = `Treatment Description`) %>% right_join(contrast, by = "treat1")
  
  
  
  
  #- Counts number of comparisons with direct evidence -#
  direct = tibble(trt1 = combn(trts,2)[1,],
                  trt2 = combn(trts,2)[2,],
                  nstud = 0,
                  ntot = 0)
  
  for(i in 1:length(direct$trt1)){
    temp = contrast %>% filter(treat1_asnum == direct$trt1[i] & treat2_asnum == direct$trt2[i] | 
                                 treat1_asnum == direct$trt2[i] & treat2_asnum == direct$trt1[i]) %>% 
      summarise(n_stud = n(),
                sample = sum(n1,n2))
    
    direct$nstud[i] = temp[[1]]
    direct$ntot[i] = temp[[2]]
  }
  tot_direct = direct %>% filter(nstud > 0) %>% count()
  
  
  ###calculate number of arms
  arm_table = table(contrast$studlab)
  narms = tibble(test = names(arm_table),
                 vals = arm_table) %>% 
    mutate(twoarm = ifelse(vals <3,1,0),
           threearm = ifelse(vals == 3,1,0),
           fourarm = ifelse(vals == 6,1,0))
  
  twoarm = sum(narms$twoarm)
  multarm = studies - twoarm
  
  #-----------------------------------
  # Network characteristics table
  #-----------------------------------
  net_char = tibble("Characteristic" = c("Number of Interventions",
                                          "Number of Studies",
                                          "Total Number of Patients in Network",
                                          "Total Possible Pairwise Comparisons",
                                          "Total Number Pairwise Comparisons With Direct Data",
                                          "Number of Two-arm Studies",
                                          "Number of Multi-arms Studies"),
                     "Number" = c(trts,
                                  studies,
                                  totn,
                                  poss_pw,
                                  tot_direct[[1]],
                                  twoarm,
                                  multarm
                     ))
  
  
  #-----------------------------------
  # Intervention characteristics table
  #-----------------------------------,
  style = "asq"
  int_nums = data %>% group_by_(treat) %>% summarise("Number of Comparisons" = n(),
                                                     "Number of Patients" = sum(n)) %>% rename_(Treatment = treat)
  
  
  int_char = tibble("Treatment" = names[[2]]) %>% left_join(int_nums, by = "Treatment")
  
  
  
  #-----------------------------------
  # Direct comparison characteristics table
  #-----------------------------------
  direct_comp_char <- direct %>% filter(nstud > 0) %>% left_join(names, by = c("trt1" = "Treatment Number")) %>%
    left_join(names, by = c("trt2" = "Treatment Number")) %>% rename(treatment_1 = `Treatment Description.x`,
                                                                     treatment_2 = `Treatment Description.y`) %>%
    mutate(Treatment = paste(treatment_2,"vs",treatment_1)) %>%
    select(Treatment,nstud,ntot) %>% rename("# Studies" = nstud,
                                            "# Patients" = ntot)
  
  direct = direct %>% left_join(names, by = c("trt1" = "Treatment Number")) %>%
    left_join(names, by = c("trt2" = "Treatment Number"))
  
  out = list(net_char = net_char, int_char = int_char, direct = direct_comp_char,nma_data = contrast,direct_zeros = direct)
  
  out
}



#Diagnostic plots
gemtc_diag = function(results = pa_reac_data$pa$results){
  windows(record = TRUE)
  plot(results)
  gelman.plot(results)
  gelman.diag(results)
  
}


#Create a league table

league = function(results = pa_reac_basicp, order = pa_reac_sucra){
  
  t = results %>% select(order$treat)
  
  df = matrix(nrow = length(t), ncol = length(t))
  
  order = order %>% separate(treat, into = c("a","treat"), sep = "d.drops.") %>% select(-a)
  
  for(i in 1:length(t)){
    
  cons = t[,i] - t
  
  temp = purrr::map(cons, ~quantile(.,c(0.025,0.5,0.975))) %>% as.data.frame() %>% t() %>% as.data.frame() %>% rownames_to_column()
  
  df[,i] = paste(round(temp[,3],2)," (",round(temp[,2],2)," to ",round(temp[,4],2),")",sep = "")
  
  df[i,i] = order[i,1]
  }
  df[upper.tri(df)] = NA
  df
}


# League table plot =========================

league_plot = function(results = pa_reac_basicp, #Basic parameters (d's)
                       order = pa_reac_sucra,  #What is to be used for ordering (SUCRA in default)
                      colour = "#d2cee3", #Light purple for NAs
                       textsize = 3, #Size for text
                       exp = FALSE, #If d's are on log scale
                      RR = FALSE,
                       statsig = 0, #Threshold for statsig (change to 1 if ORs),
                      pop = TRUE
){
  
  
  t = results %>% select(order$treat)
  
  
  #Populate required data frame
  df = data.frame(lwr=NA, med=NA, upr=NA, trt1=NA, trt2=NA)
  
  
  #Fill will all comparisons
  for(i in 1:length(t)){
    
    if(RR == FALSE){
      cons =  t[,i] - t} else(cons = t/t[,i]) 

    temp = purrr::map(cons, ~quantile(.,c(0.025,0.5,0.975))) %>% as.data.frame() %>% t() %>% as.data.frame() %>% rownames_to_column()
    
    trt1 = colnames(t[i])
    temp = temp %>% filter(rowname != trt1)
    
    df = df %>% add_row(lwr = temp[,2],med = temp[,3],upr = temp[,4], trt1 = trt1, trt2 = temp$rowname)
  }
  
  
  
  

    df["trt1"] = order[match(df[["trt1"]],order[["treat"]]),1]
    df["trt2"] = order[match(df[["trt2"]],order[["treat"]]),1]
    
    df = df %>% mutate(trt1 = factor(df$trt1, levels = order[["Treatment"]]),
                       trt2 = factor(df$trt2, levels = order[["Treatment"]]))
    if(exp == TRUE){ 
      df$val = ifelse(is.na(df$lwr),NA,paste(formatC(exp(df$med), digits = 2, format = "f"), "\n (", 
                                             formatC(exp(df$lwr), digits = 2, format = "f"), " to ", 
                                             formatC(exp(df$upr), digits = 2, format = "f"),")", sep = ""))
      
    } else{
      
      df$val = ifelse(is.na(df$lwr),NA,paste(formatC(df$med, digits = 2, format = "f"), "\n (", 
                                             formatC(df$lwr, digits = 2, format = "f"), " to ", 
                                             formatC(df$upr, digits = 2, format = "f"),")", sep = ""))
    }
    
    # df = df %>% mutate(val = ifelse(as.numeric(trt1) > as.numeric(trt2), NA, val)) %>% mutate(trt1 = factor(df$trt1, levels = order[["Treatment"]]),
    #                                                                                           trt2 = factor(df$trt2, levels = rev(order[["Treatment"]])),
    #                                                                                           sig = ifelse(is.na(val),1,ifelse(lwr < statsig & upr < statsig | lwr > statsig & upr > statsig,2,0))) #Identify stat sig results
    # 
    
    df = df %>% mutate(val = ifelse(as.numeric(trt1) > as.numeric(trt2), NA, val)) %>% mutate(trt1 = factor(df$trt1, levels = order[["Treatment"]]),
                                                                                              trt2 = factor(df$trt2, levels = rev(order[["Treatment"]])),
                                                                                              sig = ifelse(is.na(val),0,ifelse(lwr < statsig & upr < statsig | lwr > statsig & upr > statsig,1,0))) #Identify stat sig results
    
    
    ext = data_frame(lwr=NA, med=NA, upr=NA, trt1=order[["Treatment"]], trt2=order[["Treatment"]], val=order[["Treatment"]], sig = NA)
    
    df = rbind(df,ext) %>% slice(-1)
    
    
  
  
    if(sum(df$sig,na.rm = TRUE) == 0){
      plot_base = ggplot(df, aes(x = trt1, y = trt2)) +
        geom_tile(aes(fill = med), colour = ifelse(is.na(df$val),"white","black"), show.legend = F) +
        geom_text(aes(label = ifelse(is.na(df$med),str_wrap(df$val, width = 10),df$val)), size = textsize) +
        ggtitle("League Table") +
        # add colour scale
        scale_fill_gradient2(name="",
                             midpoint = 0.5,
                             na.value = colour,
                             low = "white",
                             mid = NA,
                             high = NA
        )
    } else{
      plot_base = ggplot(df, aes(x = trt1, y = trt2)) +
        geom_tile(aes(fill = sig), colour = ifelse(is.na(df$val),"white","black"), show.legend = F) +
        geom_text(aes(label = ifelse(is.na(df$med),str_wrap(df$val, width = 10),df$val)), size = textsize) +
        ggtitle("League Table") +
        # add colour scale
        scale_fill_gradient2(name="",
                             midpoint = 0.5,
                             na.value = colour,
                             low = "white",
                             mid = NA,
                             high = ifelse(!is.na(df$val),"lightgrey","white"))
    }
    
    
    if(pop == TRUE) {windows()}
    plot_base + # # move x-axis label to top
      scale_x_discrete(position = "top") + xlab("NMA Model") +
      # use a white background, remove axis borders, labels, tickts
      theme_bw() + xlab(" ") + ylab(" ") +
      theme(panel.border=element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5)
      )
  
}



###=========================NMA heatplot====

heatplot = function(data, border = "white", border_size = 2, x_lab = "NMA Model"){
  
  #Arrange treatments according to SUCRA in primary analysis    
  data = data %>% arrange(-data[,2])  
  
  # Convert to long format, create labels for cells and factor variables in order of appearence (as_factor instead of as.factor). Aaron, I'm sure there is
  # a way to do this that doesn't require forcats? It's a Hadley package so I trust it to be maintained but base would be nicer
  
  long = data %>% gather(variable,value,-Treatment) %>% mutate(value2 = ifelse(!is.na(value),paste(round(value*100,0),"%",sep = ""),
                                                                               paste(round(value*100,0)))) %>% mutate(variable = as_factor(variable))
  
  #Orders treatments based on sucra. Rev needed because axis is flipped?
  long$Treatment = factor(data$Treatment,
                          levels = rev(data$Treatment))  
  
  
  
  long %>% ggplot(aes(y = Treatment, x = variable)) + 
    geom_tile(aes(fill = value), colour = "white", size = 2) + 
    geom_text(aes(label = value2)) + scale_fill_gradient2(name="Legend\n",
                                                          midpoint = 0.5,
                                                          limits = c(0, 1),
                                                          breaks = 0.5*0:2,
                                                          labels = percent(0.5*0:2),
                                                          na.value = I(rgb(255, 255, 255, maxColorValue=255)),
                                                          low = I(rgb(248, 105, 107, maxColorValue=255)),
                                                          mid = I(rgb(255, 235, 132, maxColorValue=255)),
                                                          high = I(rgb(0, 192, 82, maxColorValue=255))) +
    # move x-axis label to top
    scale_x_discrete(position = "top") + xlab(x_lab) +
    # use a white background, remove borders
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5),
          panel.border=element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    theme(legend.title =  element_text(face = "bold", size = 10),
          legend.text = element_text(size = 7.5),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14))
}




###=========================JAGS Models====
md_fe = function(){
  # *** PROGRAM STARTS
  for(i in 1:ns){
    #LOOP THROUGH STUDIES
    mu[i] ~ dnorm(0,.0001) # vague priors for all trial baselines
    for (k in 1:na[i]) {# LOOP THROUGH ARMS
      vari[i,k] <- pow(se[i,k],2) # calculate variances
      prec[i,k] <- 1/vari[i,k]  # set precisions
      y[i,k] ~ dnorm(theta[i,k],prec[i,k]) # normal likelihood
      theta[i,k] <- mu[i] + d[t[i,k]] - d[t[i,1]] # model for linear predictor 
      dev[i,k] <- (y[i,k]-theta[i,k])*(y[i,k]-theta[i,k])*prec[i,k] #Deviance contribution 
    } 
    resdev[i] <- sum(dev[i,1:na[i]]) # summed residual deviance contribution for this trial 
  }
  totresdev <- sum(resdev[])
  #Total Residual Deviance
  d[1]<-0  # treatment effect is zero for reference treatment
  for (k in 2:nt){ d[k] ~ dnorm(0,.0001) } # vague priors for treatment effects
  
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$								
  # Extra code for all mean differences, rankings								
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$								
  
  # pairwise mds and meandiffs for all possible pair-wise comparisons, if nt>2 
  for (c in 1:(nt-1)) { 
    for (k in (c+1):nt) {
       
      meandiff[c,k] <- (d[k]-d[c])
      better[c,k]  <- 1 - step(d[k] - d[c])
    } 
  }
  
  
  # ranking calculations								
  rk = rank(d[])# assumes differences < 0 favor the comparator				
  
  # rk <- nt+1-rank(d[]) # assumes differences > 0 favor the comparator
  # Prob Best
  for(k in 1:nt){
    best[k] <- equals(rk[k],1) #calculate probability that treat k is best  }			
    for(h in 1:nt) {								
      prob[k,h]<- equals(rk[k],h)								
    }								
  }
  
  for(k in 1:nt) {								
    for(h in 1:nt) {								
      cumeffectiveness[k,h]<- sum(prob[k,1:h])								
    }								
  }								
  #SUCRAS#								
  for(i in 1:nt) {								
    SUCRA[i]<- sum(cumeffectiveness[i,1:(nt-1)]) /(nt-1)								
  }
}

md_re = function(){
  # *** PROGRAM STARTS
  for(i in 1:ns){ #LOOP THROUGH STUDIES
    w[i,1] <- 0 # adjustment for multi-arm trials is zero for control arm
    delta[i,1] <- 0 # treatment effect is zero for control arm
    mu[i] ~ dnorm(0,.0001) # vague priors for all trial baselines
    for (k in 1:na[i]) { # LOOP THROUGH ARMS
      vari[i,k] <- pow(se[i,k],2) # calculate variances
      prec[i,k] <- 1/vari[i,k] # set precisions
      y[i,k] ~ dnorm(theta[i,k],prec[i,k]) # normal likelihood
      
      theta[i,k] <- mu[i] + delta[i,k]
      # model for linear predictor 
      dev[i,k] <- (y[i,k]-theta[i,k])*(y[i,k]-theta[i,k])*prec[i,k] #Deviance contribution 
      }
      
    resdev[i] <- sum(dev[i,1:na[i]])
      
    # summed residual deviance contribution for this trial
      
    for (k in 2:na[i]) {# LOOP THROUGH ARMS
        delta[i,k] ~ dnorm(md[i,k],taud[i,k]) # trial-specific LOR distributions
        md[i,k] <- d[t[i,k]] - d[t[i,1]] + sw[i,k] # mean of treat effects distributions (with multi-arm trial correction)
        taud[i,k] <- tau *2*(k-1)/k # precision of treat effects distributions (with multi-arm trial correction)
        w[i,k] <- (delta[i,k] - d[t[i,k]] + d[t[i,1]]) # adjustment for multi-arm RCTs
        sw[i,k] <- sum(w[i,1:(k-1)])/(k-1) # cumulative adjustment for multi-arm trials 
    } 
    }
        totresdev <- sum(resdev[]) #Total Residual Deviance
        d[1]<-0 # treatment effect is zero for reference treatment
        for (k in 2:nt){ d[k] ~ dnorm(0,.0001) } # vague priors for treatment effects
        sd ~ dunif(0,5) # vague prior for between-trial SD.
        tau <- pow(sd,-2) # between-trial precision = (1/between-trial variance)
        
        #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$								
        # Extra code for all mean differences, rankings								
        #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$								
        
        # pairwise mds and meandiffs for all possible pair-wise comparisons, if nt>2 
        for (c in 1:(nt-1)) { 
          for (k in (c+1):nt) {
             
            meandiff[c,k] <- (d[k]-d[c])
            better[c,k]  <- 1 - step(d[k] - d[c])
          } 
        }
        
        
        # ranking calculations								
        rk = rank(d[])# assumes differences < 0 favor the comparator				
        
        # rk <- nt+1-rank(d[]) # assumes differences > 0 favor the comparator
        # Prob Best
        for(k in 1:nt){
          best[k] <- equals(rk[k],1) #calculate probability that treat k is best  }			
          for(h in 1:nt) {								
            prob[k,h]<- equals(rk[k],h)								
          }								
        }
        
        for(k in 1:nt) {								
          for(h in 1:nt) {								
            cumeffectiveness[k,h]<- sum(prob[k,1:h])								
          }								
        }								
        #SUCRAS#								
        for(i in 1:nt) {								
          SUCRA[i]<- sum(cumeffectiveness[i,1:(nt-1)]) /(nt-1)								
        }
      }
     
#==========================Inconsistency models



inc_md_fe = function(){
  # *** PROGRAM STARTS
  for(i in 1:ns){
    #LOOP THROUGH STUDIES
    mu[i] ~ dnorm(0,.0001) # vague priors for all trial baselines
    for (k in 1:na[i]) {# LOOP THROUGH ARMS
      vari[i,k] <- pow(se[i,k],2) # calculate variances
      prec[i,k] <- 1/vari[i,k]  # set precisions
      y[i,k] ~ dnorm(theta[i,k],prec[i,k]) # normal likelihood
      
      theta[i,k] <- mu[i] + d[t[i,1],t[i,k]] # model for linear predictor 
      
      dev[i,k] <- (y[i,k]-theta[i,k])*(y[i,k]-theta[i,k])*prec[i,k] #Deviance contribution 
    } 
    resdev[i] <- sum(dev[i,1:na[i]]) # summed residual deviance contribution for this trial 
  }
  totresdev <- sum(resdev[])
  #Total Residual Deviance

  
  for(k in 1:nt){ d[k,k] <- 0}
  for(c in 1:(nt-1)) {
    for(k in (c+1):nt) {d[c,k] ~ dnorm(0,.0001)}
    
  }
}

inc_md_re = function(){
  # *** PROGRAM STARTS
  for(i in 1:ns){#LOOP THROUGH STUDIES
    w[i,1] <- 0 # adjustment for multi-arm trials is zero for control arm
    delta[i,1] <- 0 # treatment effect is zero for control arm
    mu[i] ~ dnorm(0,.0001) # vague priors for all trial baselines
    for (k in 1:na[i]) { # LOOP THROUGH ARMS
      vari[i,k] <- pow(se[i,k],2) # calculate variances
      prec[i,k] <- 1/vari[i,k] # set precisions
      y[i,k] ~ dnorm(theta[i,k],prec[i,k])# normal likelihood
      theta[i,k] <- mu[i] + delta[i,k] # model for linear predictor 
      
      dev[i,k] <- (y[i,k]-theta[i,k])*(y[i,k]-theta[i,k])*prec[i,k] #Deviance contribution 
    }
    
    resdev[i] <- sum(dev[i,1:na[i]]) # summed residual deviance contribution for this trials
    
    
    for (k in 2:na[i]) { # LOOP THROUGH ARMS
      delta[i,k] ~ dnorm(md[i,k],taud[i,k]) # trial-specific LOR distributions
      md[i,k] <- d[t[i,1],t[i,k]] + sw[i,k] # mean of treat effects distributions (with multi-arm trial correction)
      taud[i,k] <- tau *2*(k-1)/k # precision of treat effects distributions (with multi-arm trial correction)
      w[i,k] <- (delta[i,k] - d[t[i,1],t[i,k]]) # adjustment for multi-arm RCTs
      sw[i,k] <- sum(w[i,1:(k-1)])/(k-1) # cumulative adjustment for multi-arm trials 
    } 
  }
  
  totresdev <- sum(resdev[]) #Total Residual Deviance
  
 
  sd ~ dunif(0,5) # vague prior for between-trial SD.
  tau <- pow(sd,-2) # between-trial precision = (1/between-trial variance)
  
  
  for(k in 1:nt){ d[k,k] <- 0}
  for(c in 1:(nt-1)) {
    for(k in (c+1):nt) {d[c,k] ~ dnorm(0,.0001)}
    
  }							
  
}


#==========================Informative priors

md_re_inf = function(){
  # *** PROGRAM STARTS
  for(i in 1:ns){ #LOOP THROUGH STUDIES
    w[i,1] <- 0 # adjustment for multi-arm trials is zero for control arm
    delta[i,1] <- 0 # treatment effect is zero for control arm
    mu[i] ~ dnorm(0,.0001) # vague priors for all trial baselines
    for (k in 1:na[i]) { # LOOP THROUGH ARMS
      vari[i,k] <- pow(se[i,k],2) # calculate variances
      prec[i,k] <- 1/vari[i,k] # set precisions
      y[i,k] ~ dnorm(theta[i,k],prec[i,k]) # normal likelihood
      
      theta[i,k] <- mu[i] + delta[i,k]
      # model for linear predictor 
      dev[i,k] <- (y[i,k]-theta[i,k])*(y[i,k]-theta[i,k])*prec[i,k] #Deviance contribution 
    }
    
    resdev[i] <- sum(dev[i,1:na[i]])
    
    # summed residual deviance contribution for this trial
    
    for (k in 2:na[i]) {# LOOP THROUGH ARMS
      delta[i,k] ~ dnorm(md[i,k],taud[i,k]) # trial-specific LOR distributions
      md[i,k] <- d[t[i,k]] - d[t[i,1]] + sw[i,k] # mean of treat effects distributions (with multi-arm trial correction)
      taud[i,k] <- tau *2*(k-1)/k # precision of treat effects distributions (with multi-arm trial correction)
      w[i,k] <- (delta[i,k] - d[t[i,k]] + d[t[i,1]]) # adjustment for multi-arm RCTs
      sw[i,k] <- sum(w[i,1:(k-1)])/(k-1) # cumulative adjustment for multi-arm trials 
    } 
  }
  totresdev <- sum(resdev[]) #Total Residual Deviance
  d[1]<-0 # treatment effect is zero for reference treatment
  for (k in 2:nt){ d[k] ~ dnorm(0,.0001) } # vague priors for treatment effects
  tausq ~ dlnorm(-3.23,0.47) # Semi-objective outcome pharm vs pharm, log-normal (-3.23, 1/(1.88^2))
  tau <- 1/tausq
  sd <- sqrt(tausq)
  #sd ~ dunif(0,5) # vague prior for between-trial SD.
  #tau <- pow(sd,-2) # between-trial precision = (1/between-trial variance)
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$								
  # Extra code for all mean differences, rankings								
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$								
  
  # pairwise mds and meandiffs for all possible pair-wise comparisons, if nt>2 
  for (c in 1:(nt-1)) { 
    for (k in (c+1):nt) {
       
      meandiff[c,k] <- (d[k]-d[c])
      better[c,k]  <- 1 - step(d[k] - d[c])
    } 
  }
  
  
  # ranking calculations								
  rk = rank(d[])# assumes differences < 0 favor the comparator				
  
  # rk <- nt+1-rank(d[]) # assumes differences > 0 favor the comparator
  # Prob Best
  for(k in 1:nt){
    best[k] <- equals(rk[k],1) #calculate probability that treat k is best  }			
    for(h in 1:nt) {								
      prob[k,h]<- equals(rk[k],h)								
    }								
  }
  
  for(k in 1:nt) {								
    for(h in 1:nt) {								
      cumeffectiveness[k,h]<- sum(prob[k,1:h])								
    }								
  }								
  #SUCRAS#								
  for(i in 1:nt) {								
    SUCRA[i]<- sum(cumeffectiveness[i,1:(nt-1)]) /(nt-1)								
  }
  
}


#=====================================================================================


#                                     - Binomial Models -====


#=====================================================================================

binom_fe = function(){
  
  # *** PROGRAM STARTS
  for(i in 1:ns){                 # LOOP THROUGH STUDIES
    mu[i] ~ dnorm(0,.0001)      # vague priors for all trial baselines
    for (k in 1:na[i])  {       # LOOP THROUGH ARMS
      r[i,k] ~ dbin(p[i,k],n[i,k])    # binomial likelihood
      # model for linear predictor
      logit(p[i,k]) <- mu[i] + d[t[i,k]] - d[t[i,1]]
      # expected value of the numerators 
      rhat[i,k] <- p[i,k] * n[i,k]
      #Deviance contribution
      dev[i,k] <- 2 * (r[i,k] * (log(r[i,k])-log(rhat[i,k]))
                       +  (n[i,k]-r[i,k]) * (log(n[i,k]-r[i,k]) - log(n[i,k]-rhat[i,k])))
    }
    # summed residual deviance contribution for this trial
    resdev[i] <- sum(dev[i,1:na[i]])
  }   
  totresdev <- sum(resdev[])      # Total Residual Deviance
  d[1]<-0    # treatment effect is zero for reference treatment
  # vague priors for treatment effects
  for(k in 2:nt){  d[k] ~ dnorm(0,.0001) }
  
  ################################################################################ 
  # Extra code for all odds ratios and log odds ratios, ranking, and absolute effects, and relative effects 
  # on alternative scales: Numbers Needed to Treat, Risk Difference, Relative Risks 
  ################################################################################ 
  # pairwise ORs and LORs for all possible pair-wise comparisons, if nt>2 
  for(c in 1:(nt-1)) { for (k in (c+1):nt)
  {
    or[c,k] <- exp(d[k] - d[c]) 
    lor[c,k] <- (d[k]-d[c])
    better[c,k] <- 1 - step(d[k] - d[c])
  } }
  
  # ranking calculations								
  rk = rank(d[])# assumes differences < 0 favor the comparator				
  
  # rk <- nt+1-rank(d[]) # assumes differences > 0 favor the comparator
  # Prob Best
  for(k in 1:nt){
    best[k] <- equals(rk[k],1) #calculate probability that treat k is best  }			
    for(h in 1:nt) {								
      prob[k,h]<- equals(rk[k],h)								
    }								
  }
  
  for(k in 1:nt) {								
    for(h in 1:nt) {								
      cumeffectiveness[k,h]<- sum(prob[k,1:h])								
    }								
  }								
  #SUCRAS#								
  for(i in 1:nt) {								
    SUCRA[i]<- sum(cumeffectiveness[i,1:(nt-1)]) /(nt-1)								
  }
}

  


binom_re = function(){
  # *** PROGRAM STARTS
  for(i in 1:ns){                      # LOOP THROUGH STUDIES
    w[i,1] <- 0    # adjustment for multi-arm trials is zero for control arm
    delta[i,1] <- 0             # treatment effect is zero for control arm
    mu[i] ~ dnorm(0,.0001)           # vague priors for all trial baselines
    for (k in 1:na[i]) {             # LOOP THROUGH ARMS
      r[i,k] ~ dbin(p[i,k],n[i,k]) # binomial likelihood
      logit(p[i,k]) <- mu[i] + delta[i,k]  # model for linear predictor
      rhat[i,k] <- p[i,k] * n[i,k] # expected value of the numerators 
      #Deviance contribution
      dev[i,k] <- 2 * (r[i,k] * (log(r[i,k])-log(rhat[i,k]))  
                       +  (n[i,k]-r[i,k]) * (log(n[i,k]-r[i,k]) - log(n[i,k]-rhat[i,k])))         }
    #  summed residual deviance contribution for this trial
    resdev[i] <- sum(dev[i,1:na[i]])       
    for (k in 2:na[i]) {             # LOOP THROUGH ARMS
      # trial-specific LOR distributions
      delta[i,k] ~ dnorm(md[i,k],taud[i,k])
      # mean of LOR distributions (with multi-arm trial correction)
      md[i,k] <-  d[t[i,k]] - d[t[i,1]] + sw[i,k]
      # precision of LOR distributions (with multi-arm trial correction)
      taud[i,k] <- tau *2*(k-1)/k
      # adjustment for multi-arm RCTs
      w[i,k] <- (delta[i,k] - d[t[i,k]] + d[t[i,1]])
      # cumulative adjustment for multi-arm trials
      sw[i,k] <- sum(w[i,1:(k-1)])/(k-1)
    }
  }   
  totresdev <- sum(resdev[])           # Total Residual Deviance
  d[1]<-0       # treatment effect is zero for reference treatment
  # vague priors for treatment effects
  for (k in 2:nt){  d[k] ~ dnorm(0,.0001) }
  sd ~ dunif(0,5)     # vague prior for between-trial SD
  tau <- pow(sd,-2)   # between-trial precision = (1/between-trial variance)
  
  ############################################################################### # 
  # Extra code for all odds ratios and log odds ratios, ranking, and absolute effects, and relative effects 
  # on alternative scales: Numbers Needed to Treat, Risk Difference, Relative Risks 
  ############################################################################### # 
  # pairwise ORs and LORs for all possible pair-wise comparisons, if nt>2 
  for(c in 1:(nt-1)) { for (k in (c+1):nt)
  {
    or[c,k] <- exp(d[k] - d[c]) 
    lor[c,k] <- (d[k]-d[c])
    better[c,k] <- 1 - step(d[k] - d[c])
  } }
  
  # ranking calculations								
  rk = rank(d[])# assumes differences < 0 favor the comparator				
  
  # rk <- nt+1-rank(d[]) # assumes differences > 0 favor the comparator
  # Prob Best
  for(k in 1:nt){
    best[k] <- equals(rk[k],1) #calculate probability that treat k is best  }			
    for(h in 1:nt) {								
      prob[k,h]<- equals(rk[k],h)								
    }								
  }
  
  for(k in 1:nt) {								
    for(h in 1:nt) {								
      cumeffectiveness[k,h]<- sum(prob[k,1:h])								
    }								
  }								
  #SUCRAS#								
  for(i in 1:nt) {								
    SUCRA[i]<- sum(cumeffectiveness[i,1:(nt-1)]) /(nt-1)								
  }
  
}


#=========================Binomial inconsistency models====

inc_binom_fe = function(){
  
  # *** PROGRAM STARTS
  for(i in 1:ns){                 # LOOP THROUGH STUDIES
    mu[i] ~ dnorm(0,.0001)      # vague priors for all trial baselines
    for (k in 1:na[i])  {       # LOOP THROUGH ARMS
      r[i,k] ~ dbin(p[i,k],n[i,k])    # binomial likelihood
      # model for linear predictor
      logit(p[i,k]) <- mu[i] + d[t[i,1],t[i,k]]
      # expected value of the numerators 
      rhat[i,k] <- p[i,k] * n[i,k]
      #Deviance contribution
      dev[i,k] <- 2 * (r[i,k] * (log(r[i,k])-log(rhat[i,k]))
                       +  (n[i,k]-r[i,k]) * (log(n[i,k]-r[i,k]) - log(n[i,k]-rhat[i,k])))
    }
    # summed residual deviance contribution for this trial
    resdev[i] <- sum(dev[i,1:na[i]])
  }   
  totresdev <- sum(resdev[])      # Total Residual Deviance
  
  
  for(k in 1:nt){ d[k,k] <- 0}
  for(c in 1:(nt-1)) {
    for(k in (c+1):nt) {d[c,k] ~ dnorm(0,.0001)
      }
  }
}




inc_binom_re = function(){
  # *** PROGRAM STARTS
  for(i in 1:ns){                      # LOOP THROUGH STUDIES
    w[i,1] <- 0    # adjustment for multi-arm trials is zero for control arm
    delta[i,1] <- 0             # treatment effect is zero for control arm
    mu[i] ~ dnorm(0,.0001)           # vague priors for all trial baselines
    for (k in 1:na[i]) {             # LOOP THROUGH ARMS
      r[i,k] ~ dbin(p[i,k],n[i,k]) # binomial likelihood
      logit(p[i,k]) <- mu[i] + delta[i,k]  # model for linear predictor
      rhat[i,k] <- p[i,k] * n[i,k] # expected value of the numerators 
      #Deviance contribution
      dev[i,k] <- 2 * (r[i,k] * (log(r[i,k])-log(rhat[i,k]))  
                       +  (n[i,k]-r[i,k]) * (log(n[i,k]-r[i,k]) - log(n[i,k]-rhat[i,k])))         }
    #  summed residual deviance contribution for this trial
    resdev[i] <- sum(dev[i,1:na[i]])       
    for (k in 2:na[i]) {             # LOOP THROUGH ARMS
      # trial-specific LOR distributions
      delta[i,k] ~ dnorm(md[i,k],taud[i,k])
      # mean of LOR distributions (with multi-arm trial correction)
      md[i,k] <-  d[t[i,k]] - d[t[i,1]] + sw[i,k]
      # precision of LOR distributions (with multi-arm trial correction)
      taud[i,k] <- tau *2*(k-1)/k
      # adjustment for multi-arm RCTs
      w[i,k] <- (delta[i,k] - d[t[i,k]] + d[t[i,1]])
      # cumulative adjustment for multi-arm trials
      sw[i,k] <- sum(w[i,1:(k-1)])/(k-1)
    }
  }   
  totresdev <- sum(resdev[])           # Total Residual Deviance
  d[1]<-0       # treatment effect is zero for reference treatment
  # vague priors for treatment effects
  for (k in 2:nt){  d[k] ~ dnorm(0,.0001) }
  sd ~ dunif(0,5)     # vague prior for between-trial SD
  tau <- pow(sd,-2)   # between-trial precision = (1/between-trial variance)
  
  ################################################################################ 
  # Extra code for all odds ratios and log odds ratios, ranking, and absolute effects, and relative effects 
  # on alternative scales: Numbers Needed to Treat, Risk Difference, Relative Risks 
  ################################################################################ 
  # pairwise ORs and LORs for all possible pair-wise comparisons, if nt>2 
  for(c in 1:(nt-1)) { for (k in (c+1):nt)
  {
    or[c,k] <- exp(d[k] - d[c]) 
    lor[c,k] <- (d[k]-d[c])
    better[c,k] <- 1 - step(d[k] - d[c])
  } }
  
  # ranking calculations								
  rk = rank(d[])# assumes differences < 0 favor the comparator				
  
  # rk <- nt+1-rank(d[]) # assumes differences > 0 favor the comparator
  # Prob Best
  for(k in 1:nt){
    best[k] <- equals(rk[k],1) #calculate probability that treat k is best  }			
    for(h in 1:nt) {								
      prob[k,h]<- equals(rk[k],h)								
    }								
  }
  
  for(k in 1:nt) {								
    for(h in 1:nt) {								
      cumeffectiveness[k,h]<- sum(prob[k,1:h])								
    }								
  }								
  #SUCRAS#								
  for(i in 1:nt) {								
    SUCRA[i]<- sum(cumeffectiveness[i,1:(nt-1)]) /(nt-1)								
  }
  
}
#========================================================== =
# ===================== NMA THRESH ==========================


nas_thresh= function(data = lot_pa$wb_data,
                        bugs_output = lot_pa$cons$BUGSoutput,
                        param = "meandiff", #use bugsoutput target e.g. lrom if log odds
                        nmatype = "fixed",
                        y_title = "Mean difference (LoT)",
                        x_lab = "Mean difference",
                        opt_max = FALSE,
                        mcid = 0,
                        binom = FALSE){
  
  nr = nrow(data)
  data = data %>% mutate(studyid = seq(1:length(studlab)))
  
  ###Likelihood and variance/covariance for continuous data  
  if(binom == FALSE){
    for(i in 1:nr){
      
      for(j in 1:((data[i,"na"])-1)){
        #Code can be changed here to calculate LORs, RoM, etc....    
        data[i,paste(param,"_",j,sep= "")] = round(data[i,paste("y_",j+1,sep= "")] - data[i,paste("y_",1,sep= "")],2)
        
        data[i,paste("b_",j,sep = "")] = data[i,paste("t_",1, sep = "")]
        
        data[i,paste("k_",j,sep = "")] = data[i,paste("t_",j+1, sep = "")]
        
      }
    }
    
    
    
    V_diag = as.list(rep(NA,nr))
    
    #Needs to be altered if using outcome other than meandiff
    
    for (i in 1:nr){
      
      if(data$na[i] == 2){
        V_diag[[i]] = data$sd_1[i]^2/data$n_1[i] + data$sd_2[i]^2/data$n_2[i]
        
      } else if(data$na[i] == 3){
        
        v1 = data$sd_1[i]^2/data$n_1[i] + data$sd_2[i]^2/data$n_2[i]
        v2 = data$sd_1[i]^2/data$n_1[i] + data$sd_3[i]^2/data$n_3[i]
        # Covariance term
        c1 = data$sd_1[i]^2/data$n_1[i]
        
        V_diag[[i]] = matrix(c(v1,c1,c1,v2), nrow = 2)
      }
    }
    
  } else if(binom == TRUE){
    
    ###Quick fix for arms with zero events or all events
    
    for(i in 1:nr){
      for(j in 1:data[i,"na"]){
        
        r = paste("r",j, sep = "_")
        n = paste("n",j, sep = "_")
        
        data[i,r] = ifelse(data[i,r] == 0, data[i,r] + 0.5, ifelse(data[i,r] == data[i,n],data[i,r] - 0.5, data[i,r]))
      }
    }
    
    
    
    for(i in 1:nr){
      
      for(j in 1:((data[i,"na"])-1)){
        #Binomial likelihood    
        data[i,paste(param,"_",j,sep= "")] = round(log(data[i,paste("r_",j+1,sep= ""),]*(data[i,"n_1"] - data[i, "r_1"]) /
                                                         (data[i,"r_1"] * (data[i,paste("n_",j+1,sep= ""),] - data[i,paste("r_",j+1,sep= ""),]))),2)
        
        
        data[i,paste("b_",j,sep = "")] = data[i,paste("t_",1, sep = "")]
        
        data[i,paste("k_",j,sep = "")] = data[i,paste("t_",j+1, sep = "")]
        
      }
    }
    
    
    
    V_diag = as.list(rep(NA,nr))
    
    #Needs to be altered if using outcome other than meandiff
    
    attach(data)
    for (i in 1:nr){
      
      
      if(data$na[i] == 2){
        V_diag[[i]] = 1/r_1[i] + 1/r_2[i] + 1/(n_1[i]-r_1[i]) + 1/(n_2[i]-r_2[i])
        
      } else if(data$na[i] > 2){
        
        v1 <- 1/r_1[i] + 1/r_2[i] + 1/(n_1[i]-r_1[i]) + 1/(n_2[i]-r_2[i])
        v2 <- 1/r_1[i] + 1/r_3[i] + 1/(n_1[i]-r_1[i]) + 1/(n_3[i]-r_3[i])
        # Covariance term
        c1 <- 1/r_1[i] + 1/(n_1[i] - r_1[i])
        
        if(data$na[i] == 4){
          
          v3 = 1/r_1[i] + 1/r_4[i] + 1/(n_1[i]-r_1[i]) + 1/(n_4[i]-r_4[i])
          V_diag[[i]] = matrix(c(v1,c1,c1,c1,v2,c1,c1,c1,v3), nrow = 3)
          
        } else if(data$na[i] == 3){
          V_diag[[i]] <- matrix(c(v1, c1, c1, v2), nrow = 2)
        }
        
        
      } 
    }
    detach(data)
  }
  
  
  V = bdiag(V_diag)
  
  # Reshape the data to have one row per contrast per study
  col_ids = colnames(data %>% select(starts_with(param),starts_with("b"),starts_with("k")))
  
  datal <- reshape(data, varying = col_ids, 
                   timevar = "c", idvar = "studlab", direction = "long",sep = "_") %>%  
    arrange(studyid, c, b) %>% drop_na_(param)
  
  N = nrow(datal)
  K = length(unique(c(datal$b,datal$k)))
  
  # Construct the design matrix, with a row for each contrast and K-1 columns (parameters)
  X <- matrix(0, nrow = N, ncol = K-1)
  
  for (i in 1:N){
    X[i, datal$k[i]-1] = 1
    if (datal$b[i] != 1){
      X[i, datal$b[i]-1] <- -1
    }
  }
  
  #Covariance matrix of treatment differences
  res = as.data.frame(as.mcmc(bugs_output[["sims.matrix"]]))
  co = res %>% select(starts_with(paste(param,"[1", sep = "")))
  co = cov(co)
  
  
  #Mean estimates of treatment differences
  mean_t = as.data.frame(bugs_output[["summary"]]) %>% rownames_to_column() %>% 
    select(rowname,mean) %>% filter(grepl(paste(param,"\\[1",sep=""),rowname))
  
  
  #Calculate thresholds
  thresh <- nma_thresh(mean.dk = mean_t[1:(K-1), "mean"], 
                       lhood = as.matrix(V), 
                       post = co, 
                       nmatype = nmatype,
                       X = X,
                       opt.max = opt_max,
                       mcid = mcid)
  
  #Prepare data for plotting
  datal = datal %>% mutate(lab = paste(studlab, " (", k, " vs. ", b, ")", sep = ""))
  
  
  #Calculate CIs                     
  datal$CI2.5 = (datal[param] + qnorm(0.025)*sqrt(diag(V)))[[1]]
  datal$CI97.5 = (datal[param] + qnorm(0.975)*sqrt(diag(V)))[[1]]
  
  # Calculate the proportion of CI covered by invariant interval, for sorting.
  # Coverage <1 means that the CI extends beyond the bias invariant threshold, and 
  # the threshold is below the level of statistical uncertainty.
  
  datal$coverage = apply(cbind(thresh$thresholds$lo / (datal$CI2.5 - datal[param]), 
                               thresh$thresholds$hi / (datal$CI97.5 -datal[param])), 
                         1, min, na.rm = TRUE)
  
  #workaround to make code more generic
  datal = datal %>% rename_("y" = param)
  # Plot
  study_plot = thresh_forest(thresh, 
                             y = y, CI.lo = CI2.5, CI.hi = CI97.5, 
                             label = lab, orderby = coverage, data = datal,
                             CI.title = "95% Confidence Interval", y.title = y_title, 
                             label.title = "Study (Contrast)", xlab = x_lab, 
                             xlim = c(min(datal$CI2.5)-1, max(datal$CI97.5)+1), refline = 0, digits = 2,
                             calcdim = FALSE)
  
  
  #=======================Contrast plots
  # Contrast design matrix is
  
  trt_dat = data %>% select(na,starts_with("t_")) %>% arrange(t_1,t_2)
  K = (trt_dat %>% select(-na) %>% gather() %>% drop_na() %>% summarise(K = length(unique(value))))[[1]]
  
  contr_ab = data.frame(a = c(), b = c())
  
  for (i in 1:nrow(trt_dat)) {
    rowi = trt_dat %>% slice(i) %>% select_if(!is.na(.)) %>% select(-na) # non NA elements of ith row
    
    # get contrast from all combinations of treatments
    trtcomb <- combn(rowi, 2, function(x) sapply(x, as.numeric))
    
    a <- apply(trtcomb, 2, min)
    b <- apply(trtcomb, 2, max)
    
    # remove contrasts of treatments against themselves
    iseq <- a == b
    a <- a[!iseq]
    b <- b[!iseq]
    
    if (!all(iseq)) contr_ab <- rbind(contr_ab, cbind(a, b))
  }
  
  contr_ab = contr_ab %>% arrange(a,b) %>% distinct(a,b)
  
  
  X <- matrix(0, nrow = nrow(contr_ab), ncol = K-1)
  for (i in 1:nrow(X)) {
    if (contr_ab[i, "a"] > 1) X[i, contr_ab[i, "a"] - 1]  <- -1
    if (contr_ab[i, "b"] > 1)   X[i, contr_ab[i, "b"] - 1]    <- 1
  }
  
  
  
  
  # Reconstruct using NNLS
  lik.cov <- recon_vcov(co, prior.prec = .0001, X = X)
  
  
  thresh <- nma_thresh(mean.dk = mean_t$mean, 
                       lhood = lik.cov, 
                       post = co, 
                       nmatype = "fixed", 
                       X = X, 
                       opt.max = FALSE)
  
  # Get treatment codes for the contrasts with data
  d.a <- d.b <- vector(length = nrow(X))
  for (i in 1:nrow(X)){
    d.a[i] <- ifelse(any(X[i, ] == -1), which(X[i, ] == -1), 0) + 1
    d.b[i] <- ifelse(any(X[i, ] == 1), which(X[i, ] == 1), 0) + 1
  }
  
  # Transform from d_ab style contrast references into d[i] style from the full set of contrasts
  # for easy indexing in R
  d.i <- d_ab2i(d.a, d.b, K = K)
  
  t =  paste(d.a,",",d.b, sep = "")
  #JAGS Output results in [1,10] being indexed first
  post_data = as.data.frame(as.data.frame(bugs_output$summary) %>% rownames_to_column() %>% 
                              filter(grepl(param,rowname)) %>% separate(rowname, c("var","trt1","trt2"),extra = "drop") %>% 
                              select(-var) %>% map_df(as.numeric) %>% arrange(trt1,trt2)) %>% unite(trt,trt1,trt2,sep = ",") %>%
    filter(trt %in% t)
  
  
  # Create plot data
  plotdat <- data.frame(lab = paste0(d.b, " vs. ", d.a),
                        contr.mean = post_data["mean"][[1]],
                        CI2.5 = post_data["2.5%"][[1]],
                        CI97.5 = post_data["97.5%"][[1]])
  
  # Plot
  cont_plot = thresh_forest(thresh, 
                            contr.mean, 
                            CI2.5, 
                            CI97.5, 
                            label = lab, 
                            data = plotdat,
                            label.title = "Contrast", xlab = y_title, CI.title = "95 % Credible Interval",
                            xlim = c(min(datal$CI2.5)-1, max(datal$CI97.5)+1), refline = 0, digits = 2, calcdim = FALSE)
  
  
  out = list(study_plot,cont_plot)
}
