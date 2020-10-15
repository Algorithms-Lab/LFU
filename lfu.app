{application,lfu,[
    {description,"Least Frequently Used Algorithm"},
    {vsn,"2.0.4"},
    {modules,[
        lfu_sup,lfu,lfu_score_sups_sup,
        lfu_exact_score_sup,lfu_exact_score,
        lfu_quick_score_sup,lfu_quick_score
    ]},
    {registered,[
        lfu_sup,lfu,
        lfu_score_sups_sup,
        lfu_exact_score_sup,
        lfu_quick_score_sup
    ]},
    {applications,[kernel,stdlib,sasl]},
    {included_applications,[]},
    {mod,{lfu_app,[]}},
    {start_phases,[]},
    {env,[]},
    {maintainers,["Vladimir Solenkov"]},
    {links,[{"Github","https://github.com/Shpaky/LFU.git"}]}
]}.
