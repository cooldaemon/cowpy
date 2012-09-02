# -*- coding: utf-8 -*-
"""
Thread Server
"""

from __future__ import absolute_import

import threading

import zmq
from zmq.eventloop import ioloop

from cowpy.wsgi import WorkerMixin, ServerBase


class Worker(WorkerMixin, threading.Thread):
    def run(self):
        socket = self._connect()

        loop = ioloop.IOLoop.instance()
        loop.add_handler(socket, self._handler, zmq.POLLIN)
        loop.start()

    def start(self):
        self.setDaemon(True)
        super(Worker, self).start()


class Server(ServerBase):
    _worker_class = Worker

    def _join_workers(self, workers):
        for worker in workers:
            while True:
                worker.join(3600)
                if not worker.isAlive():
                    break
