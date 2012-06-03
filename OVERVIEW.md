Investigation of the existing code


GUI
---

ContextMenu - context menus for canvas, edge, node, via (control point)
Colors - what it says
Constants - what it says
DisplayOptions - which of global info, node labels, node info, edge info to show.
GUIEvent - handles mouse and kbd events
NetworkUI - main GUI scaffolding stuff, menus, etc
NetworkView - handles the main pane
Palette - shapes for rendering the network items
Shape - manages/renders items from the palette
State - current GUI state
StateUtil - ?
PDDefaults - manages save/load/modification of the document, as the GUI stuff

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
InfoKind - need to clarify
NetworkControl - modifies network in reaction to GUI events
NetworkFile - reads and writes network file in XML format.
Network - the main data structure. Has Nodes, Edges, a Palette, Canvas size and extra Info
Operations - user defined operations on the network.
PersistentDocument - Handles the document over time, including undo/redo





