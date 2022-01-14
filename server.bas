'Made available under the MIT license, see LICENSE for details
OPTION _EXPLICIT
DEFLNG A-Z

CONST PORT = 8080
CONST MAX_CONNECTIONS = 8

CONST FALSE = 0
CONST TRUE = -1
DIM SHARED CRLF AS STRING
CRLF = CHR$(13) + CHR$(10)
CONST HTTP_10 = 1
CONST HTTP_11 = 11
CONST HTTP_GET = 1
CONST HTTP_HEAD = 2
TYPE connection_t
    handle AS LONG
    read_buf AS STRING
    http_version AS INTEGER
    method AS INTEGER
    request_uri AS STRING
END TYPE

TYPE http_error_t
    code AS INTEGER
    message AS STRING
    connection AS INTEGER
END TYPE

TYPE file_error_t
    failed AS INTEGER
    code AS INTEGER
END TYPE

DIM i
DIM num_active_connections
DIM server_handle
DIM SHARED Connections(1 TO MAX_CONNECTIONS) AS connection_t
DIM SHARED Http_error_info AS http_error_t
DIM SHARED File_error_info AS file_error_t

ON ERROR GOTO error_handler

server_handle = _OPENHOST("TCP/IP:" + LTRIM$(STR$(PORT)))
DO
    IF num_active_connections < MAX_CONNECTIONS THEN
        DIM new_connection
        new_connection = _OPENCONNECTION(server_handle)
        IF new_connection THEN
            num_active_connections = num_active_connections + 1
            FOR i = 1 TO MAX_CONNECTIONS
                IF Connections(i).handle = 0 THEN
                    DIM empty_connection AS connection_t
                    Connections(i) = empty_connection
                    Connections(i).handle = new_connection
                    EXIT FOR
                END IF
            NEXT i
        END IF
    END IF

    FOR i = 1 TO MAX_CONNECTIONS
        IF Connections(i).handle THEN
            DIM buf$
            GET #Connections(i).handle, , buf$
            IF buf$ <> "" THEN
                Connections(i).read_buf = Connections(i).read_buf + buf$
                process_request i
                http_error_complete:
            END IF
        END IF
    NEXT i
    _LIMIT 240
LOOP



error_handler:
IF ERR = 100 THEN 'HTTP error
    PRINT "HTTP error"; Http_error_info.code; Http_error_info.message; " for connection"; Http_error_info.connection
    RESUME http_error_complete
END IF
PRINT "error"; ERR; "on line"; _ERRORLINE
END

file_error_handler:
File_error_info.failed = TRUE
File_error_info.code = ERR
RESUME NEXT

SUB http_send_status (c, code, message AS STRING)
    DIM s$
    s$ = "HTTP/1.1" + STR$(code) + " " + message + CRLF
    PUT #Connections(c).handle, , s$
END SUB

SUB http_send_header (c, header AS STRING, value AS STRING)
    DIM s$
    s$ = header + ": " + value + CRLF
    PUT #Connections(c).handle, , s$
END SUB

SUB http_end_headers (c)
    PUT #Connections(c).handle, , CRLF
END SUB

SUB http_send_body (c, body AS STRING)
    PUT #Connections(c).handle, , body
END SUB

SUB http_do_get (c)
    DIM filepath AS STRING, filedata AS STRING
    DIM fh
    filepath = get_requested_filesystem_path(c)
    IF NOT _FILEEXISTS(filepath) THEN http_error 404, "Not Found", c

    ON ERROR GOTO file_error_handler
    fh = FREEFILE
    File_error_info.failed = FALSE
    OPEN filepath FOR BINARY AS #fh
    ON ERROR GOTO error_handler
    IF File_error_info.failed THEN http_error 403, "Permission Denied", c

    'Doing this all in one go isn't healthy for a number of reasons (memory usage, starving other clients)
    'It should be done in chunks in the main loop
    filedata = SPACE$(LOF(fh))
    GET #fh, , filedata
    CLOSE #fh
    http_send_status c, 200, "OK"
    http_send_header c, "Content-Length", LTRIM$(STR$(LEN(filedata)))
    http_send_header c, "Connection", "close"
    http_end_headers c
    http_send_body c, filedata
    close_connection c
END SUB

SUB http_do_head (c)
    DIM s$
    s$ = "HTTP/1.1 200 OK" + CRLF + CRLF
    PUT #Connections(c).handle, , s$
END SUB

SUB close_connection (c)
    CLOSE #Connections(c).handle
    Connections(c).handle = 0
END SUB

FUNCTION get_requested_filesystem_path$ (c)
    '7230 5.3 also 3986 for URI
    'Origin form only for now
    DIM raw_path AS STRING
    raw_path = Connections(c).request_uri
    IF LEFT$(raw_path, 1) <> "/" THEN http_error 400, "Malformed URI", c

    DIM hash, questionmark, path_len
    hash = INSTR(raw_path, "#") 'Clients shouldn't be sending fragments, but we will gracefully ignore them
    questionmark = INSTR(raw_path, "?")
    path_len = LEN(raw_path)
    IF hash > 0 THEN path_len = hash - 1
    IF questionmark > 0 AND questionmark < hash THEN path_len = questionmark - 1
    ' Query strings are ignored for now

    get_requested_filesystem_path = _CWD$ + cannonicalise_path(percent_decode(LEFT$(raw_path, path_len)))
END FUNCTION

FUNCTION percent_decode$ (raw_string AS STRING)
    DIM final_string AS STRING, hexchars AS STRING
    DIM i, c
    FOR i = 1 TO LEN(raw_string)
        c = ASC(raw_string, i)
        IF c = 37 THEN '%
            hexchars = MID$(raw_string, i + 1, 2)
            IF LEN(hexchars) = 2 AND INSTR("0123456789abcdefABCDEF", LEFT$(hexchars, 1)) > 0 AND INSTR("0123456789abcdefABCDEF", RIGHT$(hexchars, 1)) > 0 THEN
                final_string = final_string + CHR$(VAL("&H" + hexchars))
            ELSE
                'String ends in something like "%1", or is invalid hex characters
                final_string = final_string + "%" + hexchars
            END IF
            i = i + LEN(hexchars)
        ELSE
            final_string = final_string + CHR$(c)
        END IF
    NEXT i
    percent_decode = final_string
END FUNCTION


FUNCTION cannonicalise_path$ (raw_path AS STRING)
    DIM path AS STRING
    REDIM segments(1 TO 1) AS STRING
    DIM i, uplevels
    split raw_path, "/", segments()
    FOR i = UBOUND(segments) TO 1 STEP -1
        IF segments(i) = "." OR segments(i) = "" THEN
            _CONTINUE
        ELSEIF segments(i) = ".." THEN
            uplevels = uplevels + 1
        ELSE
            IF uplevels = 0 THEN
                path = "/" + segments(i) + path
            ELSE
                uplevels = uplevels - 1
            END IF
        END IF
    NEXT i
    IF path = "" THEN path = "/"
    'Note: if uplevels > 0 at this point, the path attempted to go above the root
    'This is usually a client trying to be naughty
    cannonicalise_path = path
END FUNCTION

'https://www.qb64.org/forum/index.php?topic=1607.0
SUB split (SplitMeString AS STRING, delim AS STRING, loadMeArray() AS STRING)
    DIM curpos AS LONG, arrpos AS LONG, LD AS LONG, dpos AS LONG 'fix use the Lbound the array already has
    curpos = 1: arrpos = LBOUND(loadMeArray): LD = LEN(delim)
    dpos = INSTR(curpos, SplitMeString, delim)
    DO UNTIL dpos = 0
        loadMeArray(arrpos) = MID$(SplitMeString, curpos, dpos - curpos)
        arrpos = arrpos + 1
        IF arrpos > UBOUND(loadMeArray) THEN REDIM _PRESERVE loadMeArray(LBOUND(loadMeArray) TO UBOUND(loadMeArray) + 1000) AS STRING
        curpos = dpos + LD
        dpos = INSTR(curpos, SplitMeString, delim)
    LOOP
    loadMeArray(arrpos) = MID$(SplitMeString, curpos)
    REDIM _PRESERVE loadMeArray(LBOUND(loadMeArray) TO arrpos) AS STRING 'get the ubound correct
END SUB


SUB process_request (c)
    DIM eol
    DIM l AS STRING
    DO
        eol = INSTR(Connections(c).read_buf, CRLF)
        IF eol = 0 THEN EXIT SUB
        l = LEFT$(Connections(c).read_buf, eol - 1)
        Connections(c).read_buf = MID$(Connections(c).read_buf, eol + 2)
        IF Connections(c).http_version = 0 THEN 'First line not yet read
            process_start_line c, l
        ELSE
            IF l = "" THEN
                'headers complete; act upon request now
                SELECT CASE Connections(c).method
                    CASE HTTP_GET
                        http_do_get c
                    CASE HTTP_HEAD
                        http_do_head c
                END SELECT
                EXIT SUB
            ELSE
                process_header c, l
            END IF
        END IF
    LOOP
END SUB

SUB process_start_line (c, l AS STRING)
    '7230 3.1.1
    'METHOD uri HTTP/x.y
    DIM sp1, sp2
    sp1 = INSTR(l, " ")
    IF sp1 = 0 THEN http_error 400, "Bad Request", c

    '7231 4.3
    SELECT CASE LEFT$(l, sp1 - 1)
        CASE "GET"
            Connections(c).method = HTTP_GET
        CASE "HEAD"
            Connections(c).method = HTTP_HEAD
        CASE ELSE
            http_error 501, "Not Implemented", c
    END SELECT

    sp2 = INSTR(sp1 + 1, l, " ")
    IF sp2 = 0 OR sp2 - sp1 = 1 THEN http_error 400, "Bad Request", c
    Connections(c).request_uri = MID$(l, sp1 + 1, sp2 - sp1 - 1)

    '7230 2.6
    IF MID$(l, sp2 + 1, 5) <> "HTTP/" THEN
        http_error 400, "Bad Request", c
    END IF
    SELECT CASE MID$(l, sp2 + 6)
        CASE "1.0"
            Connections(c).http_version = HTTP_10
        CASE "1.1"
            Connections(c).http_version = HTTP_11
        CASE ELSE
            http_error 505, "HTTP Version Not Supported", c
    END SELECT
END SUB

SUB process_header (c, l AS STRING)
    'All headers ignored for now
END SUB

SUB http_error (code, message AS STRING, connection)
    http_send_status connection, code, message
    http_send_header connection, "Content-Length", "0"
    http_send_header connection, "Connection", "close"
    http_end_headers connection
    close_connection connection
    Http_error_info.code = code
    Http_error_info.message = message
    Http_error_info.connection = connection
    ERROR 100
END SUB
