20070303

A simple demo that does a post-render rotation - line by line it reads the video line buffer, rotates into a buffer, and copies it back.

Ultimately, this was not really performant enough to be useful. But there's some notes about blitter timing in the source.
