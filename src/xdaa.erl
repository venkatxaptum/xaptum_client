%%%-------------------------------------------------------------------
%%% @author zanebeckwith
%%% @copyright (C) 2017, Xaptum, Inc.
%%% @doc
%%%
%%% @end
%%% xdaa implements the Xaptum TLS-DAA handshake protocol
%%%-------------------------------------------------------------------
-module(xdaa).
-export([connect/2]).

-define(TIMEOUT, 5000).
-define(XDAA_VERSION, 0).

%% Sizes are in octets
-define(XDAA_GID_LENGTH, 16).
-define(XDAA_NONCE_LENGTH, 32).
-define(XDAA_ECDHE_PUB_KEY_LENGTH, 32).
-define(XDAA_SERVER_KEY_EXCHANGE_HEADER_LENGTH, 9).

connect(Host, Port) ->
        tcp_connect(Host, Port).

tcp_connect(Host, Port) ->
        case gen_tcp:connect(Host,
                             Port, 
    		             [binary, {active, false}, {packet, 0}, {keepalive, true}, {nodelay, true}]) of
                {ok, TCPSocket} ->
                        lager:debug("TCP connection established with host:~p on port:~p", [Host, Port]),
                        xdaa_send_client_hello(TCPSocket);
                {error, Reason} ->
                        lager:warning("Error making TCP connection: ~p", [Reason]),
                        {error, Reason}
        end.

xdaa_send_client_hello(TCPSocket) ->
        {MyDSAPropList, ServerDSAPropList} = create_gid_list_from_file("foo.csv"),
        {MyGID, MyDSAPubKey} = hd(MyDSAPropList),
        ClientNonce = enacl:randombytes(?XDAA_NONCE_LENGTH),
        Packet = <<?XDAA_VERSION:8,
                   ?XDAA_GID_LENGTH:16/big,
                   ?XDAA_NONCE_LENGTH:16/big,
                   MyGID/binary,
                   ClientNonce/binary>>,
        lager:debug("Sending XDAA ClientHello (~p)...", [Packet]),
        case gen_tcp:send(TCPSocket, Packet) of
                ok ->
                        lager:debug("Sent XDAA ClientHello (~p) of length ~p octets", [Packet, bit_size(Packet) div 8]),
                        xdaa_wait_on_server_key_exchange_header(TCPSocket, ClientNonce, MyDSAPubKey, ServerDSAPropList);
                {error, Reason} ->
                        lager:warning("XDAA: Error sending ClientHello: ~p", [Reason]),
                        {error, Reason}
        end.

xdaa_wait_on_server_key_exchange_header(TCPSocket, ClientNonce, MyDSAPubKey, ServerDSAPropList) ->
        case gen_tcp:recv(TCPSocket, ?XDAA_SERVER_KEY_EXCHANGE_HEADER_LENGTH, ?TIMEOUT) of
                {ok, <<?XDAA_VERSION:8,
                       ?XDAA_GID_LENGTH:16/big,
                       ?XDAA_NONCE_LENGTH:16/big,
                       ?XDAA_ECDHE_PUB_KEY_LENGTH:16/big,
                       ServerSigLength:16/big>>
                } ->
                        lager:debug("Received XDAA ServerKeyExchangeHeader with SigLength:~p", [ServerSigLength]),
                        xdaa_wait_on_server_key_exchange(TCPSocket, ServerSigLength, ClientNonce, MyDSAPubKey, ServerDSAPropList);
                {ok, <<?XDAA_VERSION:8,
                       ServerGIDLength:16/big,
                       ServerNonceLength:16/big,
                       ServerECDHEPubKeyLength:16/big,
                       ServerSigLength:16/big>>
                } ->
                        lager:warning("XDAA: Received ServerKeyExchange header with incorrect lengths - " ++
                                        "GIDLength:~p(not ~p), NonceLength:~p(not ~p), PubKeyLength:~p(not ~p), and SigLength:~p",
                                      [ServerGIDLength, ?XDAA_GID_LENGTH, ServerNonceLength, ?XDAA_NONCE_LENGTH, ServerECDHEPubKeyLength, ?XDAA_ECDHE_PUB_KEY_LENGTH, ServerSigLength]),
                        {error, "XDAA: Incorrect lengths in ServerKeyExchange header"};
                {ok, <<Version:8,
                       _/binary>>
                } ->
                        lager:warning("XDAA: Received ServerKeyExchange header with unsupported version: ~p(not ~p)", [Version, ?XDAA_VERSION]),
                        {error, "XDAA: Unsupported version in ServerKeyExchange header"};
                {ok, _} ->
                        lager:warning("XDAA: Received mal-formed ServerKeyExchange header", []),
                        {error, "XDAA: Mal-formed ServerKeyExchange header"};
                {error, Reason} ->
                        lager:warning("XDAA: Error receiving ServerKeyExchange header: ~p", [Reason]),
                        {error, Reason}
        end.

xdaa_wait_on_server_key_exchange(TCPSocket, ServerSigLength, ClientNonce, MyDSAPubKey, ServerDSAPropList) ->
        case gen_tcp:recv(TCPSocket, ?XDAA_GID_LENGTH+?XDAA_NONCE_LENGTH+?XDAA_ECDHE_PUB_KEY_LENGTH+ServerSigLength, ?TIMEOUT) of
                {ok, <<ServerGID:?XDAA_GID_LENGTH/binary-unit:8,
                       ServerNonce:?XDAA_NONCE_LENGTH/binary-unit:8,
                       ServerECDHEPubKeySwapped:?XDAA_ECDHE_PUB_KEY_LENGTH/binary-unit:8,
                       ServerSig:ServerSigLength/binary-unit:8>>
                } ->
                        lager:debug("Received XDAA ServerKeyExchange with GID:~p, Nonce:~p, SwappedPubKey:~p, and Sig:~p",
                                    [ServerGID, ServerNonce, ServerECDHEPubKeySwapped, ServerSig]),
                        xdaa_validate_server_gid(TCPSocket, ServerGID, ServerNonce, ServerECDHEPubKeySwapped, ServerSig, ClientNonce, MyDSAPubKey, ServerDSAPropList);
                {ok, _} ->
                        lager:warning("XDAA: Received mal-formed ServerKeyExchange", []),
                        {error, "XDAA: Mal-formed ServerKeyExchange message"};
                {error, Reason} ->
                        {error, Reason}
        end.

xdaa_validate_server_gid(TCPSocket, ServerGID, ServerNonce, ServerECDHEPubKeySwapped, ServerSig, ClientNonce, MyDSAPubKey, ServerDSAPropList) ->
        case proplists:lookup(ServerGID, ServerDSAPropList) of
                {ServerGID, ServerDSAPubKey} ->
                        lager:debug("Server GID (~p) accepted", [ServerGID]),
                        xdaa_validate_server_signature(TCPSocket, ServerDSAPubKey, ServerNonce, ServerECDHEPubKeySwapped, ServerSig, ClientNonce, MyDSAPubKey);
                none ->
                        lager:info("XDAA: Server GID (~p) not recognized", [ServerGID]),
                        {error, "XDAA: Server GID not recognized"}
        end.

xdaa_validate_server_signature(TCPSocket, ServerDSAPubKey, ServerNonce, ServerECDHEPubKeySwapped, ServerSig, ClientNonce, MyDSAPubKey) ->
        SigStruct = <<ServerECDHEPubKeySwapped/binary, ClientNonce/binary>>,
        case crypto:verify(ecdsa,
                           sha256,
                           SigStruct,
                           ServerSig,
                           [ServerDSAPubKey, secp256r1] ) of
                true ->
                        lager:debug("XDAA: Validation of server signature successful", []),
                        ServerECDHEPubKey = reverse_bytes(ServerECDHEPubKeySwapped),
                        xdaa_generate_ecdhe_keys(TCPSocket, ServerNonce, ServerECDHEPubKey, MyDSAPubKey);
                false ->
                        lager:warning("XDAA: Validation of server signature failed", []),
                        {error, "XDAA: Validation of server signature failed"}
        end.

xdaa_generate_ecdhe_keys(TCPSocket, ServerNonce, ServerECDHEPubKey, MyDSAPubKey) ->
        #{public := ClientECDHEPubKey, secret := ClientECDHEPrivKey} = enacl:kx_keypair(),
        case bit_size(ClientECDHEPubKey) div 8 of
                ?XDAA_ECDHE_PUB_KEY_LENGTH ->
                        xdaa_generate_signature(TCPSocket, ServerNonce, ServerECDHEPubKey, ClientECDHEPubKey, ClientECDHEPrivKey, MyDSAPubKey);
                PubKeyLength ->
                        lager:warning("XDAA: Generated ECDHE key with unexpected length: ~p (not ~p)", [PubKeyLength, ?XDAA_ECDHE_PUB_KEY_LENGTH]),
                        {error, "XDAA: Generated ECDHE key with unexpected length"}
        end.

xdaa_generate_signature(TCPSocket, ServerNonce, ServerECDHEPubKey, ClientECDHEPubKey, ClientECDHEPrivKey, MyDSAPubKey) ->
        ClientECDHEPubKeySwapped = reverse_bytes(ClientECDHEPubKey),
        SigStruct = <<ClientECDHEPubKeySwapped/binary, ServerNonce/binary>>,
        Signature = crypto:sign(ecdsa,
                                sha256,
                                SigStruct,
                                [MyDSAPubKey, secp256r1]),
        SignatureLength = bit_size(Signature) div 8,
        xdaa_send_client_key_exchange(TCPSocket, ServerECDHEPubKey, Signature, SignatureLength, ClientECDHEPubKeySwapped, ClientECDHEPrivKey).

xdaa_send_client_key_exchange(TCPSocket, ServerECDHEPubKey, Signature, SignatureLength, ClientECDHEPubKeySwapped, ClientECDHEPrivKey) ->
        Packet = <<?XDAA_VERSION:8,
                   ?XDAA_ECDHE_PUB_KEY_LENGTH:16/big,
                   SignatureLength:16/big,
                   ClientECDHEPubKeySwapped/binary,
                   Signature/binary>>,
        lager:debug("Sending XDAA ClientKeyExchange (~p)...", [Packet]),
        case gen_tcp:send(TCPSocket, Packet) of
                ok ->
                        lager:debug("Sent XDAA ClientKeyExchange (~p) of length ~p octets", [Packet, bit_size(Packet) div 8]),
                        xdaa_run_diffie_hellman(TCPSocket, ServerECDHEPubKey, ClientECDHEPrivKey);
                {error, Reason} ->
                        {error, Reason}
        end.

xdaa_run_diffie_hellman(TCPSocket, ServerECDHEPubKey, ClientECDHEPrivKey) ->
        DHSharedSecret = enacl:curve25519_scalarmult(ClientECDHEPrivKey, ServerECDHEPubKey),
        lager:debug("Got shared-secret:~p, starting TLS handshake...", [DHSharedSecret]),
        DHSharedSecretSwapped = reverse_bytes(DHSharedSecret),
        xdaa_tls_connect(TCPSocket, DHSharedSecretSwapped).

xdaa_tls_connect(TCPSocket, DHSharedSecretSwapped) ->
        SSLOptions = [{ciphers, [{psk, aes_256_gcm, null, sha384}]},
                      {psk_identity, "id"},
                      {user_lookup_fun, {fun(psk, _, UserState) -> {ok, UserState} end, <<DHSharedSecretSwapped/binary>>}}],
        case ssl:connect(TCPSocket, SSLOptions, ?TIMEOUT) of
                {ok, SSLSocket} ->
                        lager:debug("Completed TLS handshake"),
                        {ok, SSLSocket};
                {error, Reason} ->
                        {error, Reason}
        end.

reverse_bytes(Input) ->
        %% Interpret input as little-endian, then re-interpret as big-endian.
        Size = size(Input),
        <<AsLittle:Size/little-unit:8>> = Input,
        <<AsLittle:Size/big-unit:8>>.

create_gid_list_from_file(_) ->
        %% TODO
        %% A tuple of proplists:
        %%      - The first proplist is my GID and ECDSA private key (just one entry),
        %%      - The second proplist is all accepted server GIDs and ECDSA public keys.
        {[{<<"1234567898765432">>,
          16#43F8F422782842DE799239644A49E359FDBFF81AB604AFD8BE0089B4F3686A0C}],
         [{<<"1234567898765432">>,
          16#04D676C0F20309A1060A15ADBF20A28C27494908F13F34E3DA1F4973DD19B62DA281BBDEEB0D00354CD0A0E437958DE2BBD32BA70FE8FF805D9C395A1E069CDCEB}]
        }.
