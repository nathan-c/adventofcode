import os

def open_file(filename):
    pathname = os.path.dirname(os.path.realpath(__file__))
    inputfile = os.path.join(pathname, filename)
    return open(inputfile)
