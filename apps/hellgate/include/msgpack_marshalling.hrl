-ifndef(__msgpack_marshalling__).
-define(__msgpack_marshalling__, 42).

-define(WRAP_VERSION_DATA(Version, Data),
    #{{str, "version"} => Version, {str, "data"} => Data}
).
-define(BIN(String),
    unicode:characters_to_binary(String, unicode)
).

-endif.