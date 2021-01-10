{application,lfu,[
    {description,"Least Frequently Used Algorithm"},
    {vsn,"3.0.0"},
    {modules,[
        lfu_app,lfu_sup,lfu,
        lfu_protocol,lfu_utils
    ]},
    {registered,[
        lfu_sup,lfu
    ]},
    {applications,[kernel,stdlib]},
    {included_applications,[ranch]},
    {mod,{lfu_app,[]}},
    {start_phases,[]},
    {env,[]},
    {maintainers,["Vladimir Solenkov"]},
    {links,[{"Github","https://github.com/Algorithms-Lab/LFU"}]}
]}.
