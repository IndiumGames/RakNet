# RakNet RPC3 with C++14, without boost.

This is Boost-free version of RakNet's RPC3 plugin. Boost is replaced with features of standard C++ thanks to C++14.

Because C++11 introduced variadic templates, there is no limitation to the count of arguments. 

This should work as drop-in for you current RakNet project with old RPC3.

Tested with Linux and GCC 5.3.1


## Run the demo

You will need a copy of RakNet in this directory and you can run demo with run-client.sh.
You need to run a server with run-server.sh
