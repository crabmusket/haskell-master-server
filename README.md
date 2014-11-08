# haskell-master-server

An HTTP master server for Torque 3D.
Read [Main.hs](./Main.hs) for code, and [this page](http://wiki.torque3d.org/coder:haskell-master-server) for a detailed explanation and tutorial.

## What?

The master server provides a central location where game servers can register themselves, and game clients can get a list of available servers.
This is a basic example of a flexible master server that responds to HTTP requests.

## How do I use it?

Run it somewhere, then send a GET request to `/games` to see the list of currently active servers.
The plain-text response will contain one server IP address per line.

POST to `/games` to register a server at your current IP address.
You must post once every 30 seconds, or the master server will drop your game from the list.
