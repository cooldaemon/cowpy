# -*- coding: utf-8 -*-
"""
Gevent Server
"""

from __future__ import absolute_import

import gevent

from zmq import green as zmq

from cowpy.wsgi import WorkerMixin, ServerBase


class Worker(WorkerMixin, gevent.Greenlet):
    def _run(self):
        socket = self._connect()

        poller = zmq.Poller()
        poller.register(socket, zmq.POLLIN)

        while True:
            sockets = dict(poller.poll())
            if sockets.get(socket) == zmq.POLLIN:
                self._handler(socket)


class Server(ServerBase):
    _worker_class = Worker

    def _join_workers(self, workers):
        gevent.joinall(workers)
