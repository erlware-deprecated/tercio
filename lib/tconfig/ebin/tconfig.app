{application, tconfig,
 [{description, "Tercio TConfig handler"},
  {vsn, "0.1.0"},
  {modules, [    tcnf_sup,
                 tcnf_app,
                 tconfig
                ]},
  
  {registered, [tcnf_sup, tconfig]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {tcnf_app, []}}]}.
