# Tools and options
STRIP   = strip
HC      = ghc
MAIN    = Blobs
EXE     = 
HC_OPTS = -package HaXml -package wx -package lang -fglasgow-exts -i$(IFACES) -Wall -static $(EXTRA_OPTS)
# -optl-mwindows
# voor GHC 6.4:  -ignore-package network-1.0
MAC     = yes

.SUFFIXES : .o .hs .hi .lhs .hc	.s .ag

# Sources
BLOBS = src/Main.hs src/State.hs src/StateUtil.hs src/Math.hs src/GUIEvents.hs \
		src/Common.hs src/CommonIO.hs \
		src/Network.hs src/NetworkFile.hs \
		src/Colors.hs src/Constants.hs src/SafetyNet.hs  \
		src/Document.hs \
		src/NetworkUI.hs src/NetworkView.hs src/NetworkControl.hs \
		src/ContextMenu.hs \
		src/PersistentDocument.hs src/PDDefaults.hs \
		src/XTC.hs \
		src/Shape.hs src/Palette.hs src/InfoKind.hs \
		src/DisplayOptions.hs src/Operations.hs \

DDATA = lib/DData/IntBag.hs lib/DData/IntMap.hs lib/DData/IntSet.hs \
	lib/DData/Map.hs lib/DData/MultiSet.hs \
	lib/DData/Queue.hs lib/DData/Scc.hs \
	lib/DData/Seq.hs lib/DData/Set.hs

SRCS = $(BLOBS) $(DDATA)
OBJS = $(SRCS:.hs=.o) 
IFACES = src:lib/DData

# Main target
blobs: $(OBJS)
	$(HC)	-o $(MAIN)$(EXE) $(HC_OPTS) $(OBJS)
ifdef STRIP
	$(STRIP) $(MAIN)$(EXE)
endif
ifdef MAC
	/usr/local/wxhaskell/bin/macosx-app $(MAIN)$(EXE)
	rm $(MAIN)$(EXE)
	cp $(MAIN).icns $(MAIN).app/Contents/Resources
	patch -p0 <patch.icons
endif
	

# Documentation target (use Haddock 0.5 in combination with GHC 6.0.1)	
doc:
	haddock +RTS -K4M -RTS \
		-h -o docs/haddock \
		--lib C:\cygwin\home\administrator\haddock-0.5\haddock\html \
		$(BLOBS)

# Clean target
clean:
	$(RM) src/*.o
	$(RM) src/*.hi
	$(RM) lib/DData/*.o
	$(RM) lib/DData/*.hi
	$(RM) $(MAIN)$(EXE)

# Inter-module dependencies
depend :
	# Checking dependencies
	$(HC) -M $(HC_OPTS) $(SRCS)

# Standard suffix rules
.o.hi:
	@:

.lhs.o:
	$(HC) -c $< $(HC_OPTS)

.hs.o:
	$(HC) -c $< $(HC_OPTS)

.hs.hi:
	$(HC) -c $< $(HC_OPTS) 

.lhs.hi:
	$(HC) -c $< $(HC_OPTS)

.ag.hs:
	$(AG) -a $<

# DO NOT DELETE: Beginning of Haskell dependencies
src/Main.o : src/Main.hs
src/Main.o : lib/DData/IntMap.hi
src/Main.o : src/Operations.hi
src/Main.o : src/Network.hi
src/Main.o : src/InfoKind.hi
src/Main.o : src/State.hi
src/Main.o : src/NetworkUI.hi
src/State.o : src/State.hs
src/State.o : src/DisplayOptions.hi
src/State.o : src/PersistentDocument.hi
src/State.o : src/Math.hi
src/State.o : src/Document.hi
src/StateUtil.o : src/StateUtil.hs
src/StateUtil.o : src/PersistentDocument.hi
src/StateUtil.o : src/Common.hi
src/StateUtil.o : src/State.hi
src/Math.o : src/Math.hs
src/GUIEvents.o : src/GUIEvents.hs
src/GUIEvents.o : src/InfoKind.hi
src/GUIEvents.o : src/PersistentDocument.hi
src/GUIEvents.o : src/ContextMenu.hi
src/GUIEvents.o : src/Document.hi
src/GUIEvents.o : src/CommonIO.hi
src/GUIEvents.o : src/Common.hi
src/GUIEvents.o : src/State.hi
src/GUIEvents.o : src/NetworkControl.hi
src/GUIEvents.o : src/NetworkView.hi
src/Common.o : src/Common.hs
src/Common.o : lib/DData/IntMap.hi
src/Common.o : src/Colors.hi
src/CommonIO.o : src/CommonIO.hs
src/CommonIO.o : src/SafetyNet.hi
src/CommonIO.o : src/Common.hi
src/CommonIO.o : src/Math.hi
src/Network.o : src/Network.hs
src/Network.o : lib/DData/IntMap.hi
src/Network.o : src/Palette.hi
src/Network.o : src/Shape.hi
src/Network.o : src/InfoKind.hi
src/Network.o : src/Math.hi
src/Network.o : src/Common.hi
src/NetworkFile.o : src/NetworkFile.hs
src/NetworkFile.o : src/Palette.hi
src/NetworkFile.o : src/InfoKind.hi
src/NetworkFile.o : src/Shape.hi
src/NetworkFile.o : src/Colors.hi
src/NetworkFile.o : src/Common.hi
src/NetworkFile.o : src/Math.hi
src/NetworkFile.o : src/Network.hi
src/Colors.o : src/Colors.hs
src/Constants.o : src/Constants.hs
src/Constants.o : src/Colors.hi
src/SafetyNet.o : src/SafetyNet.hs
src/Document.o : src/Document.hs
src/Document.o : src/Math.hi
src/Document.o : src/InfoKind.hi
src/Document.o : src/Network.hi
src/NetworkUI.o : src/NetworkUI.hs
src/NetworkUI.o : src/NetworkControl.hi
src/NetworkUI.o : src/Operations.hi
src/NetworkUI.o : src/DisplayOptions.hi
src/NetworkUI.o : src/InfoKind.hi
src/NetworkUI.o : src/Palette.hi
src/NetworkUI.o : src/PDDefaults.hi
src/NetworkUI.o : src/PersistentDocument.hi
src/NetworkUI.o : src/CommonIO.hi
src/NetworkUI.o : src/Common.hi
src/NetworkUI.o : src/Document.hi
src/NetworkUI.o : src/NetworkFile.hi
src/NetworkUI.o : src/NetworkView.hi
src/NetworkUI.o : src/Network.hi
src/NetworkUI.o : src/StateUtil.hi
src/NetworkUI.o : src/State.hi
src/NetworkUI.o : src/SafetyNet.hi
src/NetworkUI.o : src/GUIEvents.hi
src/NetworkView.o : src/NetworkView.hs
src/NetworkView.o : lib/DData/IntMap.hi
src/NetworkView.o : src/InfoKind.hi
src/NetworkView.o : src/DisplayOptions.hi
src/NetworkView.o : src/Shape.hi
src/NetworkView.o : src/Math.hi
src/NetworkView.o : src/Palette.hi
src/NetworkView.o : src/Common.hi
src/NetworkView.o : src/Colors.hi
src/NetworkView.o : src/Document.hi
src/NetworkView.o : src/Network.hi
src/NetworkView.o : src/CommonIO.hi
src/NetworkView.o : src/Constants.hi
src/NetworkControl.o : src/NetworkControl.hs
src/NetworkControl.o : src/InfoKind.hi
src/NetworkControl.o : src/PersistentDocument.hi
src/NetworkControl.o : src/Shape.hi
src/NetworkControl.o : src/Math.hi
src/NetworkControl.o : src/CommonIO.hi
src/NetworkControl.o : src/Common.hi
src/NetworkControl.o : src/Document.hi
src/NetworkControl.o : src/NetworkView.hi
src/NetworkControl.o : src/Network.hi
src/NetworkControl.o : src/StateUtil.hi
src/NetworkControl.o : src/State.hi
src/ContextMenu.o : src/ContextMenu.hs
src/ContextMenu.o : src/InfoKind.hi
src/ContextMenu.o : src/Palette.hi
src/ContextMenu.o : src/PersistentDocument.hi
src/ContextMenu.o : src/Math.hi
src/ContextMenu.o : src/CommonIO.hi
src/ContextMenu.o : src/SafetyNet.hi
src/ContextMenu.o : src/NetworkControl.hi
src/ContextMenu.o : src/Document.hi
src/ContextMenu.o : src/Network.hi
src/ContextMenu.o : src/State.hi
src/PersistentDocument.o : src/PersistentDocument.hs
src/PDDefaults.o : src/PDDefaults.hs
src/XTC.o : src/XTC.hs
src/Shape.o : src/Shape.hs
src/Shape.o : src/Constants.hi
src/Shape.o : src/Colors.hi
src/Shape.o : src/Math.hi
src/Shape.o : src/CommonIO.hi
src/Palette.o : src/Palette.hs
src/Palette.o : src/Shape.hi
src/InfoKind.o : src/InfoKind.hs
src/DisplayOptions.o : src/DisplayOptions.hs
src/Operations.o : src/Operations.hs
src/Operations.o : lib/DData/IntMap.hi
src/Operations.o : src/PersistentDocument.hi
src/Operations.o : src/Document.hi
src/Operations.o : src/State.hi
src/Operations.o : src/Network.hi
src/Operations.o : src/InfoKind.hi
lib/DData/IntBag.o : lib/DData/IntBag.hs
lib/DData/IntBag.o : lib/DData/IntMap.hi
lib/DData/IntMap.o : lib/DData/IntMap.hs
lib/DData/IntSet.o : lib/DData/IntSet.hs
lib/DData/Map.o : lib/DData/Map.hs
lib/DData/MultiSet.o : lib/DData/MultiSet.hs
lib/DData/MultiSet.o : lib/DData/Map.hi
lib/DData/Queue.o : lib/DData/Queue.hs
lib/DData/Scc.o : lib/DData/Scc.hs
lib/DData/Scc.o : lib/DData/Set.hi
lib/DData/Scc.o : lib/DData/Map.hi
lib/DData/Seq.o : lib/DData/Seq.hs
lib/DData/Set.o : lib/DData/Set.hs
# DO NOT DELETE: End of Haskell dependencies
