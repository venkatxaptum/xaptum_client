-ifndef('__definitions_hrl__').
-define('__definitions_hrl__',true).


-define(GUID_SIZE, 16).
-define(USER_SIZE, 8).
-define(TOKEN_SIZE, 8).
-define(SESSION_TOKEN_SIZE, 36).

-define(DDS_MARKER, 120).

-define(AUTH_EMP_REQ, 64).
-define(AUTH_REG_REQ, 80).
-define(AUTH_OBB_REQ, 112).
-define(AUTH_SUB_REQ, 88).
-define(AUTH_RES, 208).

-define(OBB_MSG, 48).
-define(REG_MSG, 16).

-define(CONTROL_MSG, 148).

-define(ENV_GUID, "XAPTUM_CLIENT_GUID").
-define(ENV_USER, "XAPTUM_CLIENT_USER").
-define(ENV_TOKEN, "XAPTUM_CLIENT_TOKEN").

-define(AUTH_INFO_SIZE, (?GUID_SIZE + ?USER_SIZE + ?TOKEN_SIZE)).

-record(creds, {guid, user, token, session_token, queue}).

-endif.