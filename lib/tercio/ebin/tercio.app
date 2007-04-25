{application, tercio,
 [{description, "Tercio Javascript/Erlang Bridge"},
  {vsn, "0.1.0"},
  {modules, [tercio, 
             trc_app, 
             trc_sup,
             trc_worker,
             trc_worker_sup,
             trc_carre_handler]},
  
  {registered, [tercio]},
  {applications, [kernel, stdlib, sasl, carre]},
  {mod, {trc_app, []}}]}.
