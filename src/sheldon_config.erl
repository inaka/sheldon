%%% @hidden
%%% @doc sheldon's module for config handling.
%%%
%%% Copyright 2016 Inaka &lt;hello@inaka.net&gt;
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Inaka <hello@inaka.net>
%%%
-module(sheldon_config).
-author("Felipe Ripoll <ferigis@gmail.com>").

%% API
-export([
          default/0
        ]).

-export_type([
               config/0
             ]).

-type config() ::  #{
                      lang => sheldon_dictionary:language()
                    }.

%%%===================================================================
%%% API
%%%===================================================================

-spec default() -> config().
default() -> #{
                lang => default_lang()
              }.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec default_lang() -> sheldon_dictionary:language().
default_lang() -> eng.
