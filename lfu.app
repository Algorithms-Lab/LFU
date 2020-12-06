{application,lfu,[
    {description,"Least Frequently Used Algorithm"},
    {vsn,"2.1.3"},
    {modules,[
        lfu_app,lfu_sup,lfu,
        lfu_score_sups_sup,lfu_protocol,
        lfu_exact_score_sup,lfu_exact_score,
        lfu_quick_score_sup,lfu_quick_score,
        lfu_utils
    ]},
    {registered,[
        lfu_sup,lfu,
        lfu_score_sups_sup,
        lfu_exact_score_sup,
        lfu_quick_score_sup
    ]},
    {applications,[kernel,stdlib]},
    {included_applications,[ranch]},
    {mod,{lfu_app,[]}},
    {start_phases,[]},
    {env,[]},
    {maintainers,["Vladimir Solenkov"]},
    {links,[{"Github","https://github.com/Shpaky/LFU.git"}]}
]}.
