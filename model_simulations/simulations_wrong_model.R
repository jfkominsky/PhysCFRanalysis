
# In addition to the model preregistered at https://osf.io/qn3b9/wiki/home/ we 
# initially also considered  a different model variant shown here. In this model
# we have two different substantive parameters, maintaining start point (m_s) 
# and maintaining end point (m_e). However, it became apparent that this model 
# made predictions that seemed implausible (either more errors than corrected
# responses or two many E4 errors). Consequently, we developed a different model
# variant we then preregistered.
#
# The structure of this file is similar to the other simulation file, see there
# for a high-level overview of the structure of this document.

library("MPTinR")

model <- "
s*m_s*(1-m_e) + (1-s)*g1*g2
s*m_s*m_e + (1-s)*g1*(1-g2)
s*(1-m_s)*m_e + (1-s)*(1-g1)*g2
s*(1-m_s)*(1-m_e) + (1-s)*(1-g1)*(1-g2)
"

check.mpt(textConnection(model))

rnd <- gen.data(parameter.values = c("g1" = 0.5, "g2" = 0.5, 
                              "m_e" = 0.8, "m_s" = 0.6, "s" = 0.7), 
         samples = 10, model.filename = textConnection(model), 
         n.per.item.type = 24*6)
rnd
### problem is visible in generated data: given reasonable parameter values,
### errors are more likely than correct responses. This also holds for other
### parameter settings.


mm <- fit.mpt(data = rnd, model.filename = textConnection(model), 
              restrictions.filename = list("g1 = 0.5", "g2 = 0.5"))
t(mm$parameters$individual[,1,])







#### 3 conditions

NSIM <- 100

# library(magrittr)
# stringr::str_replace_all(model, 
#                          pattern = "([[:alpha:]][[:alnum:]_]*)", 
#                          "\\1.3") %>% 
#   cat()

model_3 <- "
s.1*m_s.1*(1-m_e.1) + (1-s.1)*g1.1*g2.1
s.1*m_s.1*m_e.1 + (1-s.1)*g1.1*(1-g2.1)
s.1*(1-m_s.1)*m_e.1 + (1-s.1)*(1-g1.1)*g2.1
s.1*(1-m_s.1)*(1-m_e.1) + (1-s.1)*(1-g1.1)*(1-g2.1)

s.2*m_s.2*(1-m_e.2) + (1-s.2)*g1.2*g2.2
s.2*m_s.2*m_e.2 + (1-s.2)*g1.2*(1-g2.2)
s.2*(1-m_s.2)*m_e.2 + (1-s.2)*(1-g1.2)*g2.2
s.2*(1-m_s.2)*(1-m_e.2) + (1-s.2)*(1-g1.2)*(1-g2.2)

s.3*m_s.3*(1-m_e.3) + (1-s.3)*g1.3*g2.3
s.3*m_s.3*m_e.3 + (1-s.3)*g1.3*(1-g2.3)
s.3*(1-m_s.3)*m_e.3 + (1-s.3)*(1-g1.3)*g2.3
s.3*(1-m_s.3)*(1-m_e.3) + (1-s.3)*(1-g1.3)*(1-g2.3)
"

check.mpt(textConnection(model_3))
dput(check.mpt(textConnection(model_3))$parameters)

pgen_s <- c("g1.1" =.5, "g1.2" = .5, "g1.3" = .5, 
          "g2.1" = .5, "g2.2" = .5, "g2.3" = .5, 
          "m_e.1" = 0.25, "m_e.2"= 0.25, "m_e.3" = 0.25, 
          "m_s.1" = 0.75, "m_s.2" = 0.75, "m_s.3" = 0.75, 
          "s.1" = 0.6, "s.2" = 0.7, "s.3" = 0.8)

d3_s <- gen.data(parameter.values = pgen_s, 
         samples = NSIM, 
         model.filename = textConnection(model_3), 
         n.per.item.type = rep(24*6, 3))


fit_s_u <- fit.mpt(data = d3_s, model.filename = textConnection(model_3), 
              restrictions.filename = list("g1.1 = 0.5", "g2.1 = 0.5",
                                           "g1.2 = 0.5", "g2.2 = 0.5",
                                           "g1.3 = 0.5", "g2.3 = 0.5"))

fit_s_r <- fit.mpt(data = d3_s, model.filename = textConnection(model_3), 
              restrictions.filename = list("g1.1 = 0.5", "g2.1 = 0.5",
                                           "g1.2 = 0.5", "g2.2 = 0.5",
                                           "g1.3 = 0.5", "g2.3 = 0.5",
                                           "s.1 = s.2 = s.3"))

t(fit_s_u$parameters$individual[c("s.1", "s.2", "s.3"),1,])
