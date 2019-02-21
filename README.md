GRAPH
=====

Tool

Editor

Representation

#{ type => vertex, id => 1, x => 10, y => 20, shape => circle,
	radius => 10, color => red, label => "V1" ... }.
#{ type => vertex, id => 2, x => 40, y => 45, shape => square,
	side => 15, color => blue, label => "V2" ... }.

#{ type => edge, directed => true, id => 1, from => 1, to => 2,
	points => [{15,30},{19,40}], label => "E1" ... }.
#{ type => edge, directed => true, id => 2, from => 1, to => 2,
	label => "E2" ... }.
#{ type => edge, directed => true, id => 3, from => 2, to => 1,
	label => "E3" ... }.
