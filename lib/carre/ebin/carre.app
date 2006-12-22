{application, carre,
        [{description, "Tercio Test Web Server"},
         {vsn, "0.1.0"},
         {modules, [    carre_sup,
			carre_app,
			carre_server,
                        carre_socket,
			]},

         {registered, [	carre_sup]},
         {applications, [kernel, stdlib]},
	 {mod, {carre_app, []}}]}.
