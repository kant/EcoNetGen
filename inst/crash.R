library(EcoNetGen)
set.seed(2222)

## Runs just fine
replicate(1000,
netgen(net_size = 50,
       ave_module_size = 10,
       min_module_size = 4,
       min_submod_size = 2,
       net_type = "bi-partite nested",
       ave_degree = 10,
       rewire_prob_global = 0.3,
       rewire_prob_local = 0.1,
       mixing_probs = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2 ,0.2),
       verbose = FALSE)
)


set.seed(1234)
## Catches error:
          netgen(net_size = 30,
                 ave_module_size = 10,
                 min_module_size = 1,
                 min_submod_size = 1,
                 net_type = "bn",
                 ave_degree = 10,
                 rewire_prob_global = 0.2,
                 rewire_prob_local = 0.0,
                 mixing_probs = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.0 ,0.0),
                 verbose = FALSE)




set.seed(2222)
replicate(1000,
          netgen(net_size = 50,
                 ave_module_size = 25,
                 min_module_size = 10,
                 min_submod_size = 1,
                 net_type = "mixed",
                 ave_degree = 9,
                 rewire_prob_global = 0.0,
                 rewire_prob_local = 0.0,
                 mixing_probs = c(1,1,1,0.1,0.1,0.1,0.1),
                 verbose = FALSE)
)



