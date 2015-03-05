#!/usr/bin/env python

# -*- coding:utf-8 -*-

from __future__ import unicode_literals

import sys
import os
import urllib2


from config import Config
from util import error

# Constants:
FONT_HOST = 'http://fonts.openlilylib.org'
FONT_CATALOG_FILE_NAME = 'CATALOG'
FONT_CATALOG_URL = '{}/{}'.format(FONT_HOST, FONT_CATALOG_FILE_NAME)


class Catalog(object):
    """
    Represents a font catalog file,
    is able to parse it and also to write the local file
    """

    def __init__(self, file = None):

        self._file_name = ""
        if file:
            # only the local catalog has a file name
            self._file_name = file
            self._file_content = self._read_catalog()
        else:
            self._file_content = self._download_catalog()

    def _download_catalog(self):
        """
        Try to downlaod the remote catalog
        :return: string list with the content of the catalog
        """
        print "Downloading font catalog from {} ...".format(FONT_HOST)
        try:
            return urllib2.urlopen(FONT_CATALOG_URL)
        except Exception, e:
            print "An error occured while accessing the catalog:\n{}\nAborting".format(str(e))
            sys.exit(1)

    def _read_catalog(self):
        """
        Read the local catalog file and return its content as a string list.
        If the file isn't present we're assuming a first-time run and
        return an empty string list.
        :return:
        """
        if os.path.exists(self._file_name):
            try:
                f = open(self._file_name, 'r')
                result = f.readlines()
                f.close()
                return result
            except Exception, e:
                error("Error reading local font catalog.")
        return []

class Fonts(object):
    """
    Maintain the local and remote catalogs and federate their relations.
    """

    def __init__(self):

        # maintain a sorted list of all font names
        self._font_list = []

        # check local font catalog
        self.local_catalog = Catalog(os.path.join(Config.font_repo(), FONT_CATALOG_FILE_NAME))

        if not Config.local():
            self.remote_catalog = Catalog()
