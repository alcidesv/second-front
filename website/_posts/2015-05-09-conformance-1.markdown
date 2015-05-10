---
layout: post
title:  H2Spec test results
date:   2015-05-10 19:00:36
categories: 
    - second-transfer 
    - conformance
author: Alcides Viamontes
permalink: /second-transfer/conformance-1/
---

These are the results of running [h2spec](https://github.com/summerwind/h2spec) 
version 0.0.8 in second-transfer. In short:

66 tests, 39 passed, 1 skipped, 26 failed

Not too shy... in theory. 
Truth is I know of quite a few bugs that went probably unchecked.
The full log is below.

      myself@myself-pc:~/projects/second-front$ tools/h2spec -p 443 -h www.httptwo.com -t -k
      3.5. HTTP/2 Connection Preface
        ✓ Sends invalid connection preface

      4.2. Frame Size
        ✓ Sends large size frame that exceeds the SETTINGS_MAX_FRAME_SIZE

      4.3. Header Compression and Decompression
        ✓ Sends invalid header block fragment

      5.1. Stream States
        ✓ idle: Sends a DATA frame
        ✓ idle: Sends a RST_STREAM frame
        × idle: Sends a WINDOW_UPDATE frame
          - The endpoint MUST treat this as a connection error (Section 5.4.1) of type PROTOCOL_ERROR.
            Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                      Connection close
              Actual: Test timeout
        × idle: Sends a CONTINUATION frame
          - The endpoint MUST treat this as a connection error (Section 5.4.1) of type PROTOCOL_ERROR.
            Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                      Connection close
              Actual: DATA frame
        ✓ half closed (remote): Sends a DATA frame
        × half closed (remote): Sends a HEADERS frame
          - The endpoint MUST respond with a stream error (Section 5.4.2) of type STREAM_CLOSED.
            Expected: GOAWAY frame (ErrorCode: STREAM_CLOSED)
                      RST_STREAM frame (ErrorCode: STREAM_CLOSED)
                      Connection close
              Actual: DATA frame
        ✓ half closed (remote): Sends a CONTINUATION frame
        ✓ closed: Sends a DATA frame
        × closed: Sends a HEADERS frame
          - The endpoint MUST treat this as a stream error (Section 5.4.2) of type STREAM_CLOSED.
            Expected: GOAWAY frame (ErrorCode: STREAM_CLOSED)
                      RST_STREAM frame (ErrorCode: STREAM_CLOSED)
                      Connection close
              Actual: DATA frame
        ✓ closed: Sends a CONTINUATION frame

        5.1.1. Stream Identifiers
          × Sends even-numbered stream identifier
            - The endpoint MUST respond with a connection error of type PROTOCOL_ERROR.
              Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                        Connection close
                Actual: DATA frame
          × Sends stream identifier that is numerically smaller than previous
            - The endpoint MUST respond with a connection error of type PROTOCOL_ERROR.
              Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                        Connection close
                Actual: DATA frame

        5.1.2. Stream Concurrency
            Sends HEADERS frames that causes their advertised concurrent stream limit to be exceeded
            - Skipped: SETTINGS_MAX_CONCURRENT_STREAMS is unlimited.

      5.4. Error Handling
        5.4.1. Connection Error Handling
          ✓ Receives a GOAWAY frame

      5.5. Extending HTTP/2
        ✓ Sends an unknown extension frame
        × Sends an unknown extension frame in the middle of a header block
          - The endpoint MUST treat as a connection error of type PROTOCOL_ERROR.
            Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                      Connection close
              Actual: Test timeout

      6.1. DATA
        ✓ Sends a DATA frame with 0x0 stream identifier
        ✓ Sends a DATA frame on the stream that is not opend
        ✓ Sends a DATA frame with invalid pad length

      6.2. HEADERS
        ✓ Sends a HEADERS frame followed by any frame other than CONTINUATION
        × Sends a HEADERS frame followed by a frame on a different stream
          - The endpoint MUST treat the receipt of a frame on a different stream as a connection error of type PROTOCOL_ERROR.
            Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                      Connection close
              Actual: DATA frame
        ✓ Sends a HEADERS frame with 0x0 stream identifier
        ✓ Sends a HEADERS frame with invalid pad length

      6.3. PRIORITY
        ✓ Sends a PRIORITY frame with 0x0 stream identifier
        ✓ Sends a PRIORITY frame with a length other than 5 octets

      6.4. RST_STREAM
        ✓ Sends a RST_STREAM frame with 0x0 stream identifier
        ✓ Sends a RST_STREAM frame on a idle stream
        ✓ Sends a RST_STREAM frame with a length other than 4 octets

      6.5. SETTINGS
        ✓ Sends a SETTINGS frame
        ✓ Sends a SETTINGS frame that is not a zero-length with ACK flag
        ✓ Sends a SETTINGS frame with the stream identifier that is not 0x0
        ✓ Sends a SETTINGS frame with a length other than a multiple of 6 octets

        6.5.2. Defined SETTINGS Parameters
          × SETTINGS_ENABLE_PUSH (0x2): Sends the value other than 0 or 1
            - The endpoint MUST respond with a connection error of type PROTOCOL_ERROR.
              Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                        Connection close
                Actual: SETTINGS frame
          × SETTINGS_INITIAL_WINDOW_SIZE (0x4): Sends the value above the maximum flow control window size
            - The endpoint MUST respond with a connection error of type FLOW_CONTROL_ERROR.
              Expected: GOAWAY frame (ErrorCode: FLOW_CONTROL_ERROR)
                        Connection close
                Actual: SETTINGS frame
          × SETTINGS_MAX_FRAME_SIZE (0x5): Sends the value below the initial value
            - The endpoint MUST respond with a connection error of type PROTOCOL_ERROR.
              Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                        Connection close
                Actual: SETTINGS frame
          × SETTINGS_MAX_FRAME_SIZE (0x5): Sends the value above the maximum allowed frame size
            - The endpoint MUST respond with a connection error of type PROTOCOL_ERROR.
              Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                        Connection close
                Actual: SETTINGS frame

      6.7. PING
        ✓ Sends a PING frame
        ✓ Sends a PING frame with the stream identifier that is not 0x0
        ✓ Sends a PING frame with a length field value other than 8

      6.8. GOAWAY
        ✓ Sends a GOAWAY frame with the stream identifier that is not 0x0

      6.9. WINDOW_UPDATE
        ✓ Sends a WINDOW_UPDATE frame with an flow control window increment of 0
        ✓ Sends a WINDOW_UPDATE frame with an flow control window increment of 0 on a stream
        ✓ Sends a WINDOW_UPDATE frame with a length other than a multiple of 4 octets

        6.9.1. The Flow Control Window
          × Sends multiple WINDOW_UPDATE frames on a connection increasing the flow control window to above 2^31-1
            - The endpoint MUST sends a GOAWAY frame with a FLOW_CONTROL_ERROR code.
              Expected: GOAWAY frame (ErrorCode: FLOW_CONTROL_ERROR)
                Actual: Test timeout
          × Sends multiple WINDOW_UPDATE frames on a stream increasing the flow control window to above 2^31-1
            - The endpoint MUST sends a RST_STREAM with the error code of FLOW_CONTROL_ERROR code.
              Expected: RST_STREAM frame (ErrorCode: FLOW_CONTROL_ERROR)
                Actual: DATA frame

        6.9.2. Initial Flow Control Window Size
          × Sends a SETTINGS_INITIAL_WINDOW_SIZE settings with an exceeded maximum window size value
            - The endpoint MUST respond with a connection error of type FLOW_CONTROL_ERROR.
              Expected: GOAWAY frame (ErrorCode: FLOW_CONTROL_ERROR)
                        Connection close
                Actual: SETTINGS frame

      6.10. CONTINUATION
        ✓ Sends a CONTINUATION frame
        ✓ Sends multiple CONTINUATION frames
        ✓ Sends a CONTINUATION frame followed by any frame other than CONTINUATION
        ✓ Sends a CONTINUATION frame followed by a frame on a different stream
        ✓ Sends a CONTINUATION frame with the stream identifier that is 0x0
        × Sends a CONTINUATION frame after the frame other than HEADERS, PUSH_PROMISE or CONTINUATION
          - The endpoint MUST treat as a connection error of type PROTOCOL_ERROR.
            Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                      Connection close
              Actual: DATA frame

      8.1. HTTP Request/Response Exchange
        8.1.2. HTTP Header Fields
          × Sends a HEADERS frame that contains the header field name in uppercase letters
            - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
              Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                        RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                        Connection close
                Actual: DATA frame

          8.1.2.1. Pseudo-Header Fields
            × Sends a HEADERS frame that contains the pseudo-header field defined for response
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame
            × Sends a HEADERS frame that contains the invalid pseudo-header field
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame
            × Sends a HEADERS frame that contains a pseudo-header field that appears in a header block after a regular header field
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame

          8.1.2.2. Connection-Specific Header Fields
            × Sends a HEADERS frame that contains the connection-specific header field
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame
            × Sends a HEADERS frame that contains the TE header field that contain any value other than "trailers"
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame

          8.1.2.3. Request Pseudo-Header Fields
            × Sends a HEADERS frame that is omitted mandatory pseudo-header fields
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame
            × Sends a HEADERS frame containing more than one pseudo-header fields with the same name
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame

          8.1.2.6. Malformed Requests and Responses
            × Sends a HEADERS frame that contains the "content-length" header field which does not equal the sum of the DATA frame payload lengths
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame
            × Sends a HEADERS frame that contains the "content-length" header field which does not equal the sum of the multiple DATA frame payload lengths
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame

      8.2. Server Push
        ✓ Sends a PUSH_PROMISE frame

    66 tests, 39 passed, 1 skipped, 26 failed

    ===============================================================================
    Failed tests
    ===============================================================================

      5.1. Stream States
        × idle: Sends a WINDOW_UPDATE frame
          - The endpoint MUST treat this as a connection error (Section 5.4.1) of type PROTOCOL_ERROR.
            Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                      Connection close
              Actual: Test timeout
        × idle: Sends a CONTINUATION frame
          - The endpoint MUST treat this as a connection error (Section 5.4.1) of type PROTOCOL_ERROR.
            Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                      Connection close
              Actual: DATA frame
        × half closed (remote): Sends a HEADERS frame
          - The endpoint MUST respond with a stream error (Section 5.4.2) of type STREAM_CLOSED.
            Expected: GOAWAY frame (ErrorCode: STREAM_CLOSED)
                      RST_STREAM frame (ErrorCode: STREAM_CLOSED)
                      Connection close
              Actual: DATA frame
        × closed: Sends a HEADERS frame
          - The endpoint MUST treat this as a stream error (Section 5.4.2) of type STREAM_CLOSED.
            Expected: GOAWAY frame (ErrorCode: STREAM_CLOSED)
                      RST_STREAM frame (ErrorCode: STREAM_CLOSED)
                      Connection close
              Actual: DATA frame

        5.1.1. Stream Identifiers
          × Sends even-numbered stream identifier
            - The endpoint MUST respond with a connection error of type PROTOCOL_ERROR.
              Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                        Connection close
                Actual: DATA frame
          × Sends stream identifier that is numerically smaller than previous
            - The endpoint MUST respond with a connection error of type PROTOCOL_ERROR.
              Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                        Connection close
                Actual: DATA frame

      5.5. Extending HTTP/2
        × Sends an unknown extension frame in the middle of a header block
          - The endpoint MUST treat as a connection error of type PROTOCOL_ERROR.
            Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                      Connection close
              Actual: Test timeout

      6.2. HEADERS
        × Sends a HEADERS frame followed by a frame on a different stream
          - The endpoint MUST treat the receipt of a frame on a different stream as a connection error of type PROTOCOL_ERROR.
            Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                      Connection close
              Actual: DATA frame

      6.5. SETTINGS
        6.5.2. Defined SETTINGS Parameters
          × SETTINGS_ENABLE_PUSH (0x2): Sends the value other than 0 or 1
            - The endpoint MUST respond with a connection error of type PROTOCOL_ERROR.
              Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                        Connection close
                Actual: SETTINGS frame
          × SETTINGS_INITIAL_WINDOW_SIZE (0x4): Sends the value above the maximum flow control window size
            - The endpoint MUST respond with a connection error of type FLOW_CONTROL_ERROR.
              Expected: GOAWAY frame (ErrorCode: FLOW_CONTROL_ERROR)
                        Connection close
                Actual: SETTINGS frame
          × SETTINGS_MAX_FRAME_SIZE (0x5): Sends the value below the initial value
            - The endpoint MUST respond with a connection error of type PROTOCOL_ERROR.
              Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                        Connection close
                Actual: SETTINGS frame
          × SETTINGS_MAX_FRAME_SIZE (0x5): Sends the value above the maximum allowed frame size
            - The endpoint MUST respond with a connection error of type PROTOCOL_ERROR.
              Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                        Connection close
                Actual: SETTINGS frame

      6.9. WINDOW_UPDATE
        6.9.1. The Flow Control Window
          × Sends multiple WINDOW_UPDATE frames on a connection increasing the flow control window to above 2^31-1
            - The endpoint MUST sends a GOAWAY frame with a FLOW_CONTROL_ERROR code.
              Expected: GOAWAY frame (ErrorCode: FLOW_CONTROL_ERROR)
                Actual: Test timeout
          × Sends multiple WINDOW_UPDATE frames on a stream increasing the flow control window to above 2^31-1
            - The endpoint MUST sends a RST_STREAM with the error code of FLOW_CONTROL_ERROR code.
              Expected: RST_STREAM frame (ErrorCode: FLOW_CONTROL_ERROR)
                Actual: DATA frame

        6.9.2. Initial Flow Control Window Size
          × Sends a SETTINGS_INITIAL_WINDOW_SIZE settings with an exceeded maximum window size value
            - The endpoint MUST respond with a connection error of type FLOW_CONTROL_ERROR.
              Expected: GOAWAY frame (ErrorCode: FLOW_CONTROL_ERROR)
                        Connection close
                Actual: SETTINGS frame

      6.10. CONTINUATION
        × Sends a CONTINUATION frame after the frame other than HEADERS, PUSH_PROMISE or CONTINUATION
          - The endpoint MUST treat as a connection error of type PROTOCOL_ERROR.
            Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                      Connection close
              Actual: DATA frame

      8.1. HTTP Request/Response Exchange
        8.1.2. HTTP Header Fields
          × Sends a HEADERS frame that contains the header field name in uppercase letters
            - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
              Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                        RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                        Connection close
                Actual: DATA frame

          8.1.2.1. Pseudo-Header Fields
            × Sends a HEADERS frame that contains the pseudo-header field defined for response
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame
            × Sends a HEADERS frame that contains the invalid pseudo-header field
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame
            × Sends a HEADERS frame that contains a pseudo-header field that appears in a header block after a regular header field
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame

          8.1.2.2. Connection-Specific Header Fields
            × Sends a HEADERS frame that contains the connection-specific header field
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame
            × Sends a HEADERS frame that contains the TE header field that contain any value other than "trailers"
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame

          8.1.2.3. Request Pseudo-Header Fields
            × Sends a HEADERS frame that is omitted mandatory pseudo-header fields
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame
            × Sends a HEADERS frame containing more than one pseudo-header fields with the same name
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame

          8.1.2.6. Malformed Requests and Responses
            × Sends a HEADERS frame that contains the "content-length" header field which does not equal the sum of the DATA frame payload lengths
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame
            × Sends a HEADERS frame that contains the "content-length" header field which does not equal the sum of the multiple DATA frame payload lengths
              - The endpoint MUST respond with a stream error of type PROTOCOL_ERROR.
                Expected: GOAWAY frame (ErrorCode: PROTOCOL_ERROR)
                          RST_STREAM frame (ErrorCode: PROTOCOL_ERROR)
                          Connection close
                  Actual: DATA frame


Check out the [Jekyll docs][jekyll] for more info on how to get the most out of Jekyll. File all bugs/feature requests at [Jekyll’s GitHub repo][jekyll-gh]. If you have questions, you can ask them on [Jekyll’s dedicated Help repository][jekyll-help].

[jekyll]:      http://jekyllrb.com
[jekyll-gh]:   https://github.com/jekyll/jekyll
[jekyll-help]: https://github.com/jekyll/jekyll-help
