{application, iserve,
        [{description, "Web Server"},
         {vsn, "0.2.0"},
         {modules, [    iserve_sup,
			iserve_app,
			iserve_server,
                        iserve_socket,
			]},

         {registered, [	iserve_sup]},
         {applications, [kernel, stdlib]},
	 {mod, {iserve_app, []}}]}.
