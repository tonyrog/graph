GRAPH
=====

Start graph_edit by calling

    > graph_edit:start()
  
from the Erlang shell.

or 

    # erl -s graph_edit

from os shell

minor hints

* press ctrl left click to add vertices
* press alt left click and drag to add edges
* press shift to add nodes to selection
* select and drag to move graph parts around
* select and backspace to delete vertices
* press arrow keys to move selected subgraph

# generate AppImage

    erl -config graph.config -s graph_edit -s servator make_appimage graph
	[close window]
	
