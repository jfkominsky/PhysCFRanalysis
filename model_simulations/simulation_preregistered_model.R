
# This document contains a short simulation for the preregistered model 
# described in https://osf.io/qn3b9/. 
# The first simulation checks relatively roughly if, with the envisioned sample
# size, one set of parameter can be recovered in some way. For this simulation
# only one model (i.e., one set of parameters) is estimated to the data
# aggregated across all age groups.
# The second simulation splits the data by age group and checks whether
# differences in parameters between age groups can be detected with the given
# sample size.
# The simulations use R package MPTinR: 
# https://cran.r-project.org/package=MPTinR



library("MPTinR")

#################################################################
##           Simulation 1 (aggregated across groups)           ##
#################################################################

model <- "
s*m_o*m_t + (1-s)*g1*g2
s*(1-m_o)*m_t + (1-s)*g1*(1-g2)
s*m_o*(1-m_t) + (1-s)*(1-g1)*g2
s*(1-m_o)*(1-m_t) + (1-s)*(1-g1)*(1-g2)
"

check.mpt(textConnection(model))

rnd <- gen.data(parameter.values = c("g1" = 0.5, "g2" = 0.5, 
                              "m_e" = 0.8, "m_s" = 0.6, "s" = 0.7), 
         samples = 10, model.filename = textConnection(model), 
         n.per.item.type = 24*6)

mm <- fit.mpt(data = rnd, model.filename = textConnection(model), 
              restrictions.filename = list("g1 = 0.5", "g2 = 0.5"))
t(mm$parameters$individual[,1,])


#################################################################
##            Simulation 2 (separated by age group)            ##
#################################################################

NSIM <- 100

## get three copis of model
cat(stringr::str_replace_all(model,
                         pattern = "([[:alpha:]][[:alnum:]_]*)",
                         "\\1.1"),
    stringr::str_replace_all(model,
                         pattern = "([[:alpha:]][[:alnum:]_]*)",
                         "\\1.2"),
    stringr::str_replace_all(model,
                         pattern = "([[:alpha:]][[:alnum:]_]*)",
                         "\\1.3"))

model_3 <- "
s.1*m_o.1*m_t.1 + (1-s.1)*g1.1*g2.1
s.1*(1-m_o.1)*m_t.1 + (1-s.1)*g1.1*(1-g2.1)
s.1*m_o.1*(1-m_t.1) + (1-s.1)*(1-g1.1)*g2.1
s.1*(1-m_o.1)*(1-m_t.1) + (1-s.1)*(1-g1.1)*(1-g2.1)
 
s.2*m_o.2*m_t.2 + (1-s.2)*g1.2*g2.2
s.2*(1-m_o.2)*m_t.2 + (1-s.2)*g1.2*(1-g2.2)
s.2*m_o.2*(1-m_t.2) + (1-s.2)*(1-g1.2)*g2.2
s.2*(1-m_o.2)*(1-m_t.2) + (1-s.2)*(1-g1.2)*(1-g2.2)
 
s.3*m_o.3*m_t.3 + (1-s.3)*g1.3*g2.3
s.3*(1-m_o.3)*m_t.3 + (1-s.3)*g1.3*(1-g2.3)
s.3*m_o.3*(1-m_t.3) + (1-s.3)*(1-g1.3)*g2.3
s.3*(1-m_o.3)*(1-m_t.3) + (1-s.3)*(1-g1.3)*(1-g2.3)
"

check.mpt(textConnection(model_3))
dput(check.mpt(textConnection(model_3))$parameters)

pgen_s <- c("g1.1" =.5, "g1.2" = .5, "g1.3" = .5, 
          "g2.1" = .5, "g2.2" = .5, "g2.3" = .5, 
          "m_o.1" = 0.5, "m_o.2"= 0.7, "m_o.3" = 0.7, 
          "m_t.1" = 0.6, "m_t.2" = 0.8, "m_t.3" = 0.8, 
          "s.1" = 0.8, "s.2" = 0.9, "s.3" = 0.9)

d3_s <- gen.data(parameter.values = pgen_s, 
         samples = NSIM, 
         model.filename = textConnection(model_3), 
         n.per.item.type = rep(24*6, 3))
d3_s[1:5,]

fit_s_u <- fit.mpt(data = d3_s, model.filename = textConnection(model_3), 
              restrictions.filename = list("g1.1 = 0.5", "g2.1 = 0.5",
                                           "g1.2 = 0.5", "g2.2 = 0.5",
                                           "g1.3 = 0.5", "g2.3 = 0.5"))

fit_s_r <- fit.mpt(data = d3_s, model.filename = textConnection(model_3), 
              restrictions.filename = list("g1.1 = 0.5", "g2.1 = 0.5",
                                           "g1.2 = 0.5", "g2.2 = 0.5",
                                           "g1.3 = 0.5", "g2.3 = 0.5",
                                           "s.1 = s.2 = s.3"))

fit_s_a <- fit.mpt(data = d3_s, model.filename = textConnection(model_3), 
              restrictions.filename = list("g1.1 = 0.5", "g2.1 = 0.5",
                                           "g1.2 = 0.5", "g2.2 = 0.5",
                                           "g1.3 = 0.5", "g2.3 = 0.5",
                                           "s.1 = s.2 = s.3",
                                           "m_o.1 = m_o.2 = m_o.3",
                                           "m_t.1 = m_t.2 = m_t.3"))


ps <- pchisq(fit_s_r$goodness.of.fit$individual$G.Squared - 
         fit_s_u$goodness.of.fit$individual$G.Squared, 2, lower.tail = FALSE)
mean(ps < .05)

ps <- pchisq(fit_s_a$goodness.of.fit$individual$G.Squared - 
         fit_s_u$goodness.of.fit$individual$G.Squared, 6, lower.tail = FALSE)
mean(ps < .05)


t(fit_s_u$parameters$individual[c("s.1", "s.2", "s.3"),1,])
boxplot(t(fit_s_u$parameters$individual[c("s.1", "s.2", "s.3"),1,]))
