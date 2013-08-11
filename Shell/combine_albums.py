#!/usr/bin/python
#
# iTunes import was dumb and put albums under the artist directory.
# It considered "Joe" and "Joe featuring Bob" as two different artists.
# This script will merge the two back together under just "Joe".
# The mp3 tags still have the correct artist info.
#

import os,sys,filecmp

if len(sys.argv) < 3:
    print "Usage: %s <base> <featuring>+" % (sys.argv[0])
    sys.exit(0)

# First argument is the "base" where the files should be merged to.
target = sys.argv[1]
others = sys.argv[2:]

# See if there are common albums under all the given directories.
for d in others:
    dcmp = filecmp.dircmp(target, d)
    # There should be at least one common directory.
    for cd in dcmp.common_dirs:
        fcmp = filecmp.dircmp(os.sep.join([target, cd]), os.sep.join([d, cd]))
        for f in fcmp.right_only:
            if f in fcmp.common_files:
                # If the file exists in both the target album and in the
                # other album, warn and skip that file.
                print "File %s already exists in %s. Skipping." % (f, target)
            else:
                # Otherwise, copy the file from the other location to the
                # target location.
                os.rename(os.sep.join([d, cd, f]), os.sep.join([target, cd, f]))
        # If the other directory is empty now, remove it.  If not, leave it.
        try:
            os.rmdir(os.sep.join([d, cd]))
        except OSError:
            pass
    try:
        os.rmdir(d)
    except OSError:
        pass


