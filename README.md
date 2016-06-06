# Munchausen

Munchausen is a simple modern HTTP and Websocket proxy that is written
in [Erlang/OTP](http://www.erlang.org) using
[Gun](https://github.com/ninenines/gun) and
[Cowboy](https://github.com/ninenines/cowboy) designed to run in a
[Docker](https://www.docker.com) container.

Munchausen uses the following environment variables:

|Variable                   |Default |Description                                             |
|---------------------------|--------|--------------------------------------------------------|
|MUNCHAUSEN\_HTTP\_PORT     |80      |Munchausen will listen for HTTP connections on this port|
|MUNCHAUSEN\_HTTP\_ENABLED  |true    |When enabled Munchausen starts a HTTP listener          |
|MUNCHAUSEN\_HTTP\_ACCEPTORS|100     |Number of HTTP acceptors                                |


