%% @author Alfonso De Gregorio <adg@secYOUre.com>
%% @copyright 2011 Alfonso De Gregorio
%% @doc Simple comment spam filter module. Filters comment spams using Akismet.

%% Copyright 2011 Alfonso De Gregorio
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

-module(mod_akismet).
-author("Alfonso De Gregorio <adg@secYOUre.com>").

-mod_title("Akismet Comments Spam Filtering").
-mod_description("Akismet comments anti-spam. Implements a simple comment spam defense with Akismet.").

%% gen_server exports
-export([init/1]).

%% interface functions
-export([
    event/2
]).

-include_lib("zotonic.hrl").


%% @doc Handle the submit event of a new comment
event({submit, {akismet_newcomment, Args}, TriggerId, _TargetId}, Context) ->
    Key = case m_config:get_value(?MODULE, akismet_key, false, Context) of
               KB when is_binary(KB) ->
                   binary_to_list(KB);
               K -> K
          end,
    case Key of
        false ->
                error_logger:info_msg("No akismet_key configuration for mod_akismet. ~n"),
                z_session_manager:broadcast(#broadcast{type="error", message="No configuration (mod_akismet.akismet_key) found, filtering disabled.", title="Akismet", stay=true}, z_acl:sudo(Context)),
                Context;
        _ ->
                Peer   = m_req:get(peer, Context),
                UAgent = m_req:get(user_agent, Context),
                Blog   = m_site:get(hostname, Context),
                Message= z_context:get_q_validated("message", Context),

                Author = z_context:get_q_validated("name", Context),
                Email  = z_context:get_q_validated("mail", Context),
                Extra  =[
                         {"referrer", ""},
                         {"comment_type", ""},
                         {"comment_author", Author},
                         {"comment_author_email", Email},
                         {"comment_author_url", ""}
                        ],
                case erlakismet:check(Key, Blog, Peer, UAgent, Message, Extra) of
                        "false"   -> 
                                mod_comment:event({submit, {newcomment, Args}, TriggerId, _TargetId}, Context);
                        "true"    -> 
                                error_logger:info_msg("Comment spam detected. ~n"),
                                z_session_manager:broadcast(#broadcast{type="error", message="Comment spam detected.", title="Akismet", stay=true}, z_acl:sudo(Context)),
                                Context;
                        "invalid" ->
                                error_logger:info_msg("Invalid akismet_key configuration for mod_akismet. ~n"),
                                z_session_manager:broadcast(#broadcast{type="error", message="Invalid configuration (mod_akismet.akismet_key), filtering failed.", title="Akismet", stay=true}, z_acl:sudo(Context)),
                                Context
                end
    end.

init(_Context) -> ok.
