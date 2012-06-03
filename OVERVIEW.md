Investigation of the existing code modules.


GUI
---

NetworkUI - main GUI scaffolding stuff, menus, etc
 GUIEvent - handles mouse and kbd events
  ContextMenu - context menus for canvas, edge, node, via (control point)
  NetworkView - handles the main pane
  State - current GUI state
PDDefaults - manages save/load/modification of the document, as the GUI stuff
Colors - what it says
Constants - what it says
DisplayOptions - which of global info, node labels, node info, edge info to show.
Palette - shapes for rendering the network items
Shape - manages/renders items from the palette
StateUtil - ?

Util
----

Math - utility stuff
Common - utilities
CommonIO - IO utils
SafetyNet - error handlers
XTC - not used.

Model
-----
Document - is a network and a selection, and some ops on it
 Network - the main data structure. Has Nodes, Edges, a Palette, Canvas size and extra Info
  InfoKind - need to clarify
NetworkControl - modifies network in reaction to GUI events
NetworkFile - reads and writes network file in XML format.
Operations - user defined operations on the network.
PersistentDocument - Handles the document over time, including undo/redo

-------------------------

Everything works around the Network, which has three type placeholders
g - The global state type
n - The type applied to a node
e - The type applied to an edge.

The InfoKind class is used to trigger type checking when needed.

class (Eq a, Show a, Parse a, XmlContent a) => InfoKind a g | a -> g where
    blank :: a
    check :: String -> g -> a -> [String]		-- returns warnings
	-- ^ first arg is container label for error reporting.
	--   second arg is global value

blank provides an empty item which will pass the check function.

Require an instance of InfoKind n g for the node constructor
Require an instance of InfoKind e g for the edge constructor




