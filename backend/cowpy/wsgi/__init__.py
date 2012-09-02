# -*- coding: utf-8 -*-
"""
WSGI Server
"""

import sys
from cStringIO import StringIO
from tempfile import TemporaryFile

import msgpack

import zmq

class Request(object):
    def __init__(self, socket):
        self._socket = socket
        self._headers = None
        self._body_io = None
        self._has_body = False

    @property
    def headers(self):
        if self._headers:
            return self._headers

        headers = msgpack.unpackb(self._socket.recv())
        if not isinstance(headers, dict):
            raise RuntimeError, "headers was not dict."

        self._headers = headers
        self._has_body = self._socket.getsockopt(zmq.RCVMORE)
        return self._headers

    @property
    def body_io(self):
        if self._body_io:
            return self._body_io

        headers = self.headers
        if not self._has_body:
            return self._set_body_io(StringIO(""))

        len = headers.get("CONTENT_LENGTH", None)
        if len is None or 1024 * 1024 <= len:
            return self._set_body_io(self._make_body_io(TemporaryFile()))
        else:
            return self._set_body_io(self._make_body_io(StringIO()))

    def _set_body_io(self, body_io):
        self._body_io = body_io
        return body_io

    def _make_body_io(self, io):
        more = self._socket.getsockopt(zmq.RCVMORE)
        while more:
            io.write(self._socket.recv())
            more = self._socket.getsockopt(zmq.RCVMORE)
        io.seek(0)
        return io


class EnvironFactory(object):
    base_environ = {
        "REMOTE_HOST": "",
        "SCRIPT_NAME": "",
    }

    @classmethod
    def make(cls, request):
        environ = cls.base_environ.copy()
        environ.update(request.headers)
        cls._set_wsgi_env(request, environ)
        return environ

    @classmethod
    def _set_wsgi_env(cls, request, environ):
        environ["wsgi.input"] = request.body_io
        environ["wsgi.errors"] = sys.stderr
        environ["wsgi.version"] = (1, 0)
        environ["wsgi.multithread"] = True
        environ["wsgi.multiprocess"] = False

        if environ.get("HTTPS") in ("yes", "on", "1"):
            environ["wsgi.url_scheme"] = "https"
        else:
            environ["wsgi.url_scheme"] = "http"

        environ["wsgi.run_once"] = False


class Responder(object):
    def __init__(self, socket):
        self._socket = socket
        self._has_started = False

    def start_response(self, status, headers, exc_info=None):
        if exc_info:
            print >>sys.stderr, exc_info

        try:
            if self._has_started:
                if exc_info:
                    raise exc_info[0], exc_info[1], exc_info[2]
                raise RuntimeError, "responses has already started"
        finally:
            exc_info = None # avoid dangling circular ref

        self._status = status
        self._headers = headers
        return self.write
 
    def write(self, data):
        self._write_headers()
        self._write_body(data)

    def _write_headers(self):
        if self._has_started:
            return

        self._socket.send(msgpack.packb({"status": self._status,
                                         "headers": self._headers}),
                          zmq.SNDMORE)
        self._has_started = True

    def _write_body(self, data):
        self._socket.send(data, zmq.SNDMORE)

    def finish(self):
        self._socket.send("")


class WorkerMixin(object):
    def __init__(self, context, endpoint, application):
        super(WorkerMixin, self).__init__()
        self._context = context
        self._endpoint = endpoint
        self._application = application

    def _connect(self):
        socket = self._context.socket(zmq.REP)
        socket.connect(self._endpoint)
        return socket

    def _handler(self, socket, events=None):
        responder = Responder(socket)
        environ = None

        try:
            environ = EnvironFactory.make(Request(socket))
            chunks = self._application(environ, responder.start_response)
            for chunk in chunks:
                responder.write(chunk)
        except:
            responder.start_response("500 Server Error", [], sys.exc_info())
            responder.write("server error")
        finally:
            responder.finish()
            if isinstance(environ, dict) and environ.get("wsgi.input", None):
                environ["wsgi.input"].close()


class ServerBase(object):
    @property
    def _worker_class(self):
        raise NotImplementedError

    def __init__(self):
        self._context = zmq.Context()

    def run(self, endpoint, application, threads=1):
        workers = self._make_workers(endpoint, application, threads)
        self._start_workers(workers)
        self._join_workers(workers)

    def _make_workers(self, endpoint, application, workers):
        return [self._worker_class(self._context, endpoint, application)
                for n in xrange(0, workers)]

    def _start_workers(self, workers):
        for worker in workers:
            worker.start()

    def _join_workers(self, workers):
        raise NotImplementedError
