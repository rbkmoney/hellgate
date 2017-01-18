-ifndef(__hg_log_scope__).
-define(__hg_log_scope__, 42).

-define(scope(Name, Fun),
    ?scope(Name, Fun,  #{})
).

-define(scope(Name, Fun, Meta),
    try
        hg_log_scope:new(Name),
        hg_log_scope:set_meta(Meta),
        Fun()
    after
        hg_log_scope:clean()
    end
).

-endif.
