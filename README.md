Blobs
=====

Blobs from http://www.cs.york.ac.uk/fp/darcs/Blobs/

Note: wxc-0.90.0.3 fails to compile, see
https://sourceforge.net/tracker/?func=detail&atid=536845&aid=3536406&group_id=73133


A workaround: wxc install fails with eljpen.cpp not compiling.

Change line 159 to

 *_ref = ((const char* const*)NULL);

from

 *_ref = NULL;



Changes

0.3 Introduce class GuiEdit and GuiGlobalEdit to allow custom dialogs
    for editing the global,node and edge attributes

0.2 Move the contents into the Graphics.Blobs namespace

0.1 Initial import of original code, updated for GHC 7.0.4
