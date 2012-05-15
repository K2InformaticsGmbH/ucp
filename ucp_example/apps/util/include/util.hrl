%% -------------------------------------------------------------------
%%
%%
%% Copyright (c) 2012 Mobile Interactive Group a Velti company. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-define(EVENT_LOG, event_log).
-define(ERROR_LOG, error_log).
-define(ALARM_LOG, alarm_log).
-define(INFO_LOG, info_log).
-define(DUPLICATE_DELIVER_SM_KEY, duplicate_deliver_sm_key).

-type proplist() :: [{atom() | string(), any()}].
-type connection() :: string().
-type operator() :: {string(), string()}.
-type country() :: string().
-type pdu() :: proplist().
-type msisdn() :: string().
-type originator() :: string().
-type now() :: {integer(), integer(), integer()}.
-type date() :: {integer(), integer(), integer()}.
-type time() :: {integer(), integer(), integer()}.
-type datetime() :: {date(), time()}.

-type content_part() :: {text_part, binary(), string()} |
                        {header_text_part, string(), binary(), string()}.

-type xser_message_reference() :: integer().
-type originator_format() :: alphanumeric | international_msisdn.
-record(mt, {
        
          key                                   :: string(), %% guid-batch_item_number
          msisdn                                :: string(),
          originator                            :: string(),
          originator_format = alphanumeric      :: originator_format(),
          connection = []                       :: string(), %% TODO: remove connection from MT completely?
          operator                              :: atom(),
          created_at                            :: integer(),
          expires_at                            :: integer(),
          part_data                             :: content_part(),
          billing_code = []                     :: string()
         }).
