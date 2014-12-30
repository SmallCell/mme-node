{application,mme,
             [{registered,[]},
              {description,"LTE MME Node"},
              {vsn,"1.0"},
              {applications,[kernel,stdlib,lager,mnesia]},
              {mod,{mme,[]}},
              {env,[{mme_config,enabled}]},
              {modules,[mme,mme_app,mme_sctp,mme_sctp_sup,mme_sup]}]}.
