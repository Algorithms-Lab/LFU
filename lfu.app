{application,lfu,[
    {description,"Least Frequently Used Algorithm"},
    {vsn,"2.0.1"},
    {modules,[
        lfu_app,lfu_sup,lfu,lfu_score_sups_sup,
        lfu_exact_score_sup,lfu_exact_score,
        lfu_quick_score_sup,lfu_quick_score
    ]},
    {registered,[
        lfu_sup,lfu,
        lfu_score_sups_sup,
        lfu_exact_score_sup,
        lfu_quick_score_sup
    ]},
    {applications,[kernel,stdlib]},
    {included_applications,[]},
    {mod,{lfu_app,[]}},
    {start_phases,[]},
    {env,[]},
    {maintainers,["Vladimir Solenkov"]},
    {links,[{"Github","https://github.com/Shpaky/LFU.git"}]}
]}.
