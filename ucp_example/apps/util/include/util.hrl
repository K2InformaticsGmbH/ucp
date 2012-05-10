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
