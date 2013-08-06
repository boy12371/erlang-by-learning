{application, factorial_system,
    [   {description,  "factorial calculation system"},
        {vsn,          "1"},
        {modules,      [    factorial_client,
                            factorial_logic,
                            factorial_server,
                            factorial_supervisor,
                            factorial_system
                       ]
        },
        {registered,   []},
        {applications, [kernel, stdlib]},
        {mod,          {factorial_system, []}},
        {env,          []}
    ]
}.