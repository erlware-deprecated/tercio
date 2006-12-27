{application, carre,
 [{description, "Tercio Test Web Server"},
  {vsn, "0.1.0"},
  {modules, [carre_sup,
             carre_app,
             carre_server,
             carre_socket,
             carre_client,
             carre,
             carre_handler]},
  
  {registered, [carre_sup, carre]},
  {applications, [kernel, stdlib]},
  {mod, {carre_app, []}}]}.
