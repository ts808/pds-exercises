repbet
=====

A replicated gambling game using Raft. 


Build
-----

    $ rebar3 compile

Run
----

To start 3 instances run the following commands, each line in its own terminal:

On Unix systems:

    R_NODES="n1@127.0.0.1,n2@127.0.0.1,n3@127.0.0.1" rebar3 shell --name 'n1@127.0.0.1'
    R_NODES="n1@127.0.0.1,n2@127.0.0.1,n3@127.0.0.1" rebar3 shell --name 'n2@127.0.0.1'
    R_NODES="n1@127.0.0.1,n2@127.0.0.1,n3@127.0.0.1" rebar3 shell --name 'n3@127.0.0.1'
    
On Windows systems in a CMD shell:

    set R_NODES=n1@127.0.0.1,n2@127.0.0.1,n3@127.0.0.1&& rebar3 shell --name "n1@127.0.0.1"
    set R_NODES=n1@127.0.0.1,n2@127.0.0.1,n3@127.0.0.1&& rebar3 shell --name "n2@127.0.0.1"
    set R_NODES=n1@127.0.0.1,n2@127.0.0.1,n3@127.0.0.1&& rebar3 shell --name "n3@127.0.0.1"

On Windows systems in a Powershell:

    $Env:R_NODES="n1@127.0.0.1,n2@127.0.0.1,n3@127.0.0.1"; rebar3 shell --name "n1@127.0.0.1"
    $Env:R_NODES="n1@127.0.0.1,n2@127.0.0.1,n3@127.0.0.1"; rebar3 shell --name "n2@127.0.0.1"
    $Env:R_NODES="n1@127.0.0.1,n2@127.0.0.1,n3@127.0.0.1"; rebar3 shell --name "n3@127.0.0.1"


Then the application functions can be called on any shell:

    Node = {node, node()}.
    repbet_machine:create_user(Node, "Hans", "test@example.com", "hunter2").
    repbet_machine:check_password(Node, "Hans", "hunter2").


#### Configuration:

The environment variable `R_NODES` used in the example above configures which nodes belong to the cluster. 
It is a comma-separated list of node names.
The argument `--name` sets the name of the node.
Since all nodes run on the same machine, the same secret cookie will be used. 
For real distributed deployment the cookie has to be set as well.