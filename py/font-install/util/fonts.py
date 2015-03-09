#!/usr/bin/env python

# -*- coding:utf-8 -*-

from __future__ import unicode_literals

import sys
import os
import urllib2

from config import Config
from util import error


# Constants:
from util.font import Font

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

    def write_catalog(self, lines):
        """
        Write the catalog file if we're local.
        """
        if self._type == 'remote':
            error("Trying to write to remote catalog.")
        try:
            catalog = open(self._file_name, 'w')
            catalog.write('\n'.join(lines))
            catalog.close()
        except Exception, e:
            error("An error occured, while writing local font catalog:\n{}").format(str(e))


class Fonts(object):
    """
    Maintain the local and remote catalogs and federate their relations.
    """

    def __init__(self):

        # maintain a sorted list of all font names
        self._font_list = []
        # ... and a dictionary with Font() objects
        self._fonts = {}

        # property to hold the longest name (for display)
        self._max_len_font_fname = 0
        self._max_len_font_nme = 0

        # check local font catalog
        self.local_catalog = Catalog(os.path.join(Config.font_repo(), FONT_CATALOG_FILE_NAME))
        self.add_fonts(self.local_catalog)

        # ... and the remote one if the command line is set
        if not Config.local():
            self.remote_catalog = Catalog()
            self.add_fonts(self.remote_catalog)

    def _act_string(self, bool):
        """
        Return a string representation of a boolean for the display matrix
        """
        return "-X-- " if bool else "---- "

    def _display_matrix(self):
        """
        Display the actions to be performed for the fonts
        """
        print "\nActions to be performed:"
        print ''.join(
            ['FONT'.ljust(self._max_len_font_name() + 1),
             'D/L  EXT  INSTALL']
        )
        for f in self._font_list:
            print ''.join([
                f.ljust(self._max_len_font_name() + 1),
                self._act_string(self._fonts[f]._actions['download']),
                self._act_string(self._fonts[f]._actions['extract']),
                self._act_string(self._fonts[f]._actions['update_links']),
            ])

    def _max_len_font_filename(self):
        """
        Return the length of the longest font filename
        """
        if self._max_len_font_fname:
            return self._max_len_font_fname
        result = 0
        for f in self._font_list:
            result = max(result,
                         len(self._fonts[f]._basename))
        return result

    def _max_len_font_name(self):
        """
        Return the length of the longest font filename
        """
        if self._max_len_font_nme:
            return self._max_len_font_nmme
        result = 0
        for f in self._font_list:
            result = max(result,
                         len(f))
        return result

    def _write_local_catalog(self):
        import datetime

        lines = []
        lines.append("# Local LilyPond font catalog")
        lines.append("# Written by the openLilyLib font installation script")
        lines.append("# {}\n".format(datetime.date.isoformat(datetime.date.today())))
        for f in self._font_list:
            font = self._fonts[f]
            line = ' '.join([font._basename.ljust(self._max_len_font_filename()),
                             font._local_version.ljust(8),
                             f])
            lines.append(line)
        self.local_catalog.write_catalog(lines)

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
        print "\nChecking necessary actions ..."
        for f in self._font_list:
            self._fonts[f].check()
        print "... successful."

        # display a matrix with all actions for all fonts
        self._display_matrix()

        print "\nProcessing fonts ..."
        for f in self._font_list:
            self._fonts[f].handle()

        print "\nWrite local catalog"
        self._write_local_catalog()
        print "\n Script completed successfully."
