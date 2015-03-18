#!/usr/bin/env python

# -*- coding:utf-8 -*-

from __future__ import unicode_literals

import sys
import os

import commandline
from util import error

class Config(object):
    """
    Globally available configuration data.
    Access through property methods.
    """

    # initialize static members

    # run in batch mode (i.e. no user interaction)
    _batch = None

    # don't compare anything, unconditionally download and install fonts
    _force = None

    # don't consider the online repository, simply link a LilyPond installation
    _local = None

    # path to actual local font storage directory
    _local_font_repo = ""

    # path to LilyPond font directory
    _lilypond_font_root = ""


    @staticmethod
    def _get_lilypond_font_path(arg_path):
        """
        Try to determine the LilyPond root directory from the
        given command line argument (if any, otherwise default to
        current working directory).
        :param arg_path: Path to one of:
         - LilyPond executable
         - LilyPond installation dir:
           - binary distribution
           - custom build
        :return: absolute, normalized path to the font root in the LilyPond installation.
        """

        arg_path = Config._normalize_arg_path(arg_path)

        # if the given argument points to a LilyPond executable we
        # determine the relative path to the font root
        if (os.path.isfile(arg_path) and
                os.path.basename(arg_path).startswith("lilypond")):
            return os.path.normpath(os.path.join(arg_path,
                                                 '..',
                                                 '..',
                                                 'share',
                                                 'lilypond',
                                                 'current',
                                                 'fonts'))

        # if the argument points to a directory we try to determine
        # if this is a LiylPond installation (binary or custom-built).
        bridge = ""
        if (os.path.isdir(arg_path)):
            if os.path.isfile(os.path.join(arg_path, 'usr', 'bin', 'lilypond')):
                bridge = 'usr'
            elif os.path.isfile(os.path.join(arg_path, 'out', 'bin', 'lilypond')):
                bridge = 'out'
        if bridge:
            return os.path.join(arg_path,
                                bridge,
                                'share',
                                'lilypond',
                                'current',
                                'fonts')
        else:
            # Argument doesn't point to a valid LilyPond installation
            error("No LilyPond installation found at or in \n  {}.".format(
                 arg_path))


    @staticmethod
    def _get_lilypond_font_paths(arg_dirs):
        """
        Collect paths to LilyPond font directories.
        Use either the given strings or read the catalog files
        if the string points to such a file.
        :return: List with paths
        """

        def read_targets_file(target_file):
            """
            Read a file with LiylPond paths and return a string list.
            Ignore empty or comment lines.
            """
            result = []
            try:
                f = open(target_file, 'r')
                lines = f.readlines()
                f.close()
            except Exception, e:
                error(str(e))
            for l in lines:
                l = l.strip()
                if not (l.startswith('#') or len(l) == 0):
                    result.append(l)
            return result

        for lily in [l for l in arg_dirs]:
            if os.path.isfile(lily) and (not os.path.basename(lily) == 'lilypond'):
                arg_dirs.remove(lily)
                arg_dirs.extend(read_targets_file(lily))

        return [Config._get_lilypond_font_path(lily) for lily in arg_dirs]

    @staticmethod
    def _get_local_repo_path(arg_dir):
        """
        Try to determine path to local font repository from command line argument.
        Failure to do so currently aborts the script.
        :param arg_dir: path passed on the command line (if any)
        :return: absolute path to repository dir
        """

        arg_dir = Config._normalize_arg_path(arg_dir)

        # handle invalid path:
        # - in batch mode simply abort
        # - otherwise suggest creating a new directory
        if not os.path.isdir(arg_dir):
            msg = ("Font directory argument doesn't " +
                   "point to existing directory:\n  {}".format(arg_dir))
            if Config.batch():
                error("{}\nAborting".format(msg))
            elif Config.init():
                print "Initializing new repository."
                try:
                    os.makedirs(arg_dir)
                    Config._init = True
                except Exception, e:
                        error("Problem creating dir {}.\nError message:\n{}".format(
                            arg_dir,
                            str(e)
                        ))
            else:
                if raw_input("{}\n Do you want to create it (y/anything)? ".format(msg)).lower() == "y":
                    try:
                        os.makedirs(arg_dir)
                    except Exception, e:
                        error("Problem creating dir {}.\nError message:\n{}".format(
                            arg_dir,
                            str(e)
                        ))
                else:
                    print "Aborted by user."
                    sys.exit(1)
        return arg_dir

    @staticmethod
    def _normalize_arg_path(path = None):
        """
        Return an absolute, normalized path to @path.
        If @path is None or not given we take the current working directory.
        :param path: Relative or absolute path or None
        :return: absolute path
        """
        # default to current working directory
        if not path:
            path = os.getcwd()
        else:
            # normalize path argument
            path = os.path.normpath(path) if \
                os.path.isabs(path) else \
                os.path.normpath(os.path.join(os.getcwd(), path))
        return path

    @staticmethod
    def read_args():
        args = commandline.parse()

        Config._batch = False
        if args['batch']:
            Config._batch = True
            print "Script runs in batch mode (any error condition simply aborts)."

        Config._local = False
        if args['local']:
            Config._local = True
            print "Script running locally."

        Config._force = False
        if args['force'] and not Config._local:
            Config._force = True
            print "Forcing remote font versions without checking difference."

        Config._init = False
        if args['init']:
            Config._init = True
            print "Initializing a new local font repository."

        Config._local_font_repo = Config._get_local_repo_path(args['font_directory'])
        print "\nDetermined font directory:\n  {}".format(Config.font_repo())

        Config._lilypond_font_roots = Config._get_lilypond_font_paths(args['targets'])
        print "\nDetermined LilyPond font root directories:\n - {}".format(
            '\n - '.join(Config._lilypond_font_roots))


    ##############
    # 'Properties'

    @staticmethod
    def batch():
        return Config._batch

    @staticmethod
    def font_repo():
        return Config._local_font_repo

    @staticmethod
    def force():
        return Config._force

    @staticmethod
    def init():
        return Config._init

    @staticmethod
    def lilypond_font_roots():
        return Config._lilypond_font_roots

    @staticmethod
    def local():
        return Config._local