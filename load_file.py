import os
import sys

def open_file(filename):
    pathname = os.path.dirname(sys.argv[0])
    inputfile = os.path.join(pathname, filename)
    return open(inputfile)
