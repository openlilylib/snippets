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

        if file:
            # only the local catalog has a file name
            self._file_name = file
            self._type = 'local'
            self._file_content = self._read_catalog()
        else:
            self._file_name = ""
            self._type = 'remote'
            print "\nDownloading font catalog from {} ...".format(FONT_HOST)
            self._file_content = self._download_catalog()
            print "... successful."

        # parse catalog file
        self._font_records = self.parse_catalog()

    def _download_catalog(self):
        """
        Try to downlaod the remote catalog
        :return: string list with the content of the catalog
        """
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
        else:
            print "No local catalog found. Assuming you want to create a new local repository."
            return []

    def font_records(self):
        return self._font_records

    def parse_catalog(self):
        """
        Parse the catalog file and return a simple dictionary with font records
        """
        def parse_line(line):
            try:
                lst = line.split()
                result = {}
                result['basename'] = lst[0]
                result['{}_version'.format(self._type)] = lst[1]
                result['name'] = " ".join(lst[2:])
                return result
            except:
                print "Illegal line in {} font catalog file, skipping:\n  {}".format(
                    target, line)
                return None

        result = []
        for line in self._file_content:
            line = line.strip()
            if not line.startswith("#") and len(line) > 0:
                record = parse_line(line)
                if not record:
                    continue
                result.append(record)
        return result

class Font(object):
    """
    Object representing one single font.
    Capable of downloading archives and doing the 'installation'
    """
    def __init__(self, record):
        self._name = record['name']
        self._basename = record['basename']
        # font records usually only have a remote or a local version
        # as the version is eventually compared numerically
        # they are initialized with zero-string
        self._local_version = record.get('local_version', '0')
        self._remote_version = record.get('remote_version', '0')

        # determine files and paths
        self.archive = os.path.join(Config.font_repo(), "{}.zip".format(self._basename))


    def _archive_present(self):
        return os.path.isfile(self.archive)

    def _check(self):
        """
        Determine the necessary actions for the font,
        returning a dictionary with boolean values
        """
        result = r = {}

        # download archive if
        # - font not declared locally
        # - remote is newer
        # - archive is missing locally
        r['download'] = False
        if not Config.local():
            r['download'] = True if (self._local_version == '0' or
                                 self._remote_newer() or
                                 not self._archive_present()) else False


        if not (r['download']):
            print "Font {} up to date.".format(self._name)
        return result

    def _download_archive(self):
        pass

    def _remote_newer(self):
        """
        Returns True if the remote version is newer than the local one
        """
        local = self._version_as_integer(self._local_version)
        remote = self._version_as_integer(self._remote_version)
        return remote > local

    def _version_as_integer(self, version):
        """
        Calculate an integer representation of a font version string.
        """
        ver_list = version.split('.')
        power = 12
        value = 0
        for elem in ver_list:
            value += int(elem) * 10 ** power
            power -= 3
        return value

    def handle(self):
        """
        Determine necessary actions and perform them
        """
        actions = self._check()

        #raise Exception("Continue with Font.handle()")

    def merge_font(self, record):
        """
        Merge a font record into an existing Font object.
        """

        # we can consider 'name' to be identical,
        # otherwise this wouldn't be executed.
        if 'basename' in record and not self._basename == record['basename']:
            error("Conflicting font record found:\n  {}".format(record))

        # We _assume_ this will work because from reading the catalogs
        # a record can have only a remote _or_ a local version.
        if self._remote_version == '0':
            self._remote_version = record.get('remote_version', '0')
        if self._local_version == '0':
            self._local_version = record.get('local_version', '0')



class Fonts(object):
    """
    Maintain the local and remote catalogs and federate their relations.
    """

    def __init__(self):

        # maintain a sorted list of all font names
        self._font_list = []
        # ... and a dictionary with Font() objects
        self._fonts = {}

        # check local font catalog
        self.local_catalog = Catalog(os.path.join(Config.font_repo(), FONT_CATALOG_FILE_NAME))
        self.add_fonts(self.local_catalog)

        # ... and the remote one if the command line is set
        if not Config.local():
            self.remote_catalog = Catalog()
            self.add_fonts(self.remote_catalog)

    def add_font(self, font_record):
        """
        Add a new Font object to the dictionary
        :param font_record:
        :return:
        """
        fname = font_record['name']
        self._fonts[fname] = Font(font_record)
        self._font_list.append(fname)
        self._font_list.sort()

    def add_fonts(self, catalog):
        """
        Add or merge fonts from a catalog
        :param catalog:
        :return:
        """
        for f in catalog.font_records():
            if not f['name'] in self._font_list:
                self.add_font(f)
            else:
                self._fonts[f['name']].merge_font(f)

    def handle_fonts(self):
        """
        Asks each Font object to do what is necessary
        (download, check, extract, link ...)
        """
        for f in self._font_list:
            self._fonts[f].handle()
