#!/usr/bin/python
#
# Look for albums with only a few songs.
# This isn't necessarily a problem, but it can mean that it's
# part of a compilation that iTunes split up.
#
import os,sys,filecmp

start_dir = "."

if len(sys.argv) > 1:
    start_dir = sys.argv[1]

dirs = os.listdir(start_dir)

for d in dirs:
    albums = os.listdir(os.sep.join([start_dir, d]))
    for a in albums:
        songs = os.listdir(os.sep.join([start_dir, d, a]))
        if len(songs) < 2:
            print "Possible compilation member songs: "
            for s in songs:
                print os.sep.join([start_dir, d, a, s])
