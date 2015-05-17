{application, emh,
	[{vsn, "0.0.1"},
	{description, "Start a TCP Message HUB"},
	{modules, [emh, emh_sup, emh_hubserv, emh_hubserv_sup, emh_clientserv, emh_clientserv_sup]},
	{registered, [hubserv]},
	{mod, {emh, []}},
	{env,
		[{port, 9090}]}
]}.