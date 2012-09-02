#!/usr/bin/env python
# -*- coding: utf-8 -*-

import gevent
#from cowpy.wsgi.thread import Server
from cowpy.wsgi.gevent import Server

def default(environ, start_response):
    start_response('200 OK', [('Content-type', 'text/html')])
    yield """
<!DOCTYPE html>
<html>
<body>
<a href="/stream/">stream</a><br />
POST<br />
<form action=\"/save/\" method=\"POST\">
  <input type=\"hidden\" name=\"spam\" value=\"ham\" />
  <textarea name=\"egg\">aaaaaa</textarea><br />
  <input type=\"submit\" value=\"submit\" />
</form>
<br />
POST(multipart)<br />
<form action=\"/save/\" method=\"POST\" enctype=\"multipart/form-data\">
  <input type=\"hidden\" name=\"spam\" value=\"ham\" />
  <input type=\"file\" name=\"big_data\"><br />
  <input type=\"submit\" value=\"submit\" />
</form>
</body>
</html>
"""

def stream(environ, start_response):
    line_length = 1024
    count = 10
    start_response('200 OK', [('Content-Type', 'text/plain'),
                              ('Content-Length', str(line_length * count))])
    for n in xrange(0, count):
       yield 'a' * line_length
       gevent.sleep(1)

def save(environ, start_response):
    start_response('200 OK', [('Content-type', 'text/html')])
    environ
    yield """
<!DOCTYPE html>
<html>
<body>
test
</body>
</html>
"""

def application(environ, start_response):
    print environ
    if environ['PATH_INFO'] == '/save/':
        return save(environ, start_response)
    elif environ['PATH_INFO'] == '/stream/':
        return stream(environ, start_response)
    else:
        return default(environ, start_response)

def main():
    Server().run("tcp://127.0.0.1:5560", application)

if __name__ == '__main__':
    main()
