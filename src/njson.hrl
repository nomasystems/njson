%%% Copyright 2023 Nomasystems, S.L. http://www.nomasystems.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-ifndef(njson).
-define(njson, true).

%%% TYPES
-type t() :: null | boolean() | number() | binary() | [t()] | #{binary() => t()}.

-type decode_error_reason() ::
    invalid_value | unexpected_trailing_char | invalid_key | invalid_array | invalid_object.
-type decode_error() :: {error, {decode_error_reason(), [byte()], non_neg_integer()}}.

-type encode_error_reason() :: invalid_key | invalid_value | invalid_map | invalid_list.
-type encode_error() :: {error, {encode_error_reason(), any()}}.

%%% EXPORT TYPES
-export_type([t/0]).
-export_type([decode_error_reason/0]).
-export_type([decode_error/0]).
-export_type([encode_error_reason/0]).
-export_type([encode_error/0]).

% -ifndef(njson)
-endif.