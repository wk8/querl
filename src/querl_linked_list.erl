%% @doc This is a private module for querl's internal use only
-module(querl_linked_list).

-export([
    new/0,
    append/3,
    to_list/2,
    to_reversed_list/2,
    reversed_out/3,
    reversed_empty/2,
    remove/3
]).

-on_load(init/0).

-define(NOT_LOADED, not_loaded(?LINE)).

new() -> ?NOT_LOADED.

append(_LinkedListRoot, _RootVersion, _Item) -> ?NOT_LOADED.

to_list(_LinkedListRoot, _RootVersion) -> ?NOT_LOADED.
to_reversed_list(_LinkedListRoot, _RootVersion) -> ?NOT_LOADED.

reversed_out(_LinkedListRoot, _RootVersion, _RequestedSize) -> ?NOT_LOADED.

reversed_empty(_LinkedListRoot, _RootVersion) -> ?NOT_LOADED.

remove(_LinkedListRoot, _RootVersion, _LinkedListNodeToRemove) -> ?NOT_LOADED.

%%% Private helpers

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "querl"), 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
