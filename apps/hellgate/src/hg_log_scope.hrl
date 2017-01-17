-ifndef(__hg_log_scope__).
-define(__hg_log_scope__, 42).

-define(scope (Fun),
    ?scope(Fun, #{})
).

-define(scope(Fun, Meta),
    try
        hg_log_scope:new_scope(),
        hg_log_scope:set_meta(Meta),
        fun() -> Fun() end
    after
        hg_log_scope:cleanup()
    end
).

-endif.
