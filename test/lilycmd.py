import subprocess as sp
import os
import os.path as osp
import shutil
import sys
import re
import datetime
import dateutil.parser
from common_functions import print_separator

class LilyCmd(object):
    """This class represents a lilypond command and provides some
    facilities to

     - run LilyPond
     - install LilyPond versions from the internet into a local cache
     - manage the cache, cleaning old versions

    """

    # Download site for LilyPond distributions
    binary_site = "http://download.linuxaudio.org/lilypond/binaries/"
    # String template for generating the LilyPond installation command
    lily_install_script = "lilypond-install.sh"

    # After this amount of time a LilyPond version will be removed by
    # a call to LilyCmd.clean_cache()
    cache_cleanup_interval = datetime.timedelta(days=1)

    # Root directory for the cache
    cache_root = osp.join(os.getenv('HOME'), '.lilypond')

    def __init__(self, command_path, cached):
        self.command = command_path
        self.cached = cached
        try:
            self.version = self._lilypond_version()
            self.installed = True
        except:
            self.installed = False

    ####################################################################
    # Public methods

    @staticmethod
    def system(cmd_path='lilypond'):
        """Get a new instance of Lilypond provided by the
        system. (eg. /usr/bin/lilypond)"""
        return LilyCmd(cmd_path, cached=False)

    @classmethod
    def with_version(cls, platform, version):
        """Get a new version of Lilypond, given the platform and the
        version.

        **Note**: This command does not install LilyPond in the local
        cache. To check if the instance returned by this method
        corresponds to an actually installed LilyPond version, check
        the `installed` attribute. The reason for not installing
        LilyPond with this command is that we may be interested in all
        kind of ancillary information (like the cache directory)
        without actually installing LilyPond.

        """
        lily_cmd_path = osp.join(
            LilyCmd._cache_directory(platform, version),
            'bin', 'lilypond')
        lily_cmd = LilyCmd(lily_cmd_path, cached=True)
        return lily_cmd

    def execute(self, args):
        """Executes the LilyPond command with the given arguments"""
        self._mark_cache()
        lily = sp.Popen([self.command] + args,
                        stdout=sp.PIPE, stderr=sp.PIPE)
        (out, err) = lily.communicate()
        return lily.returncode, out, err

    @classmethod
    def install(cls, platform, version):
        """Download and install LilyPond version if not cached"""
        lilypond_cmd = LilyCmd.with_version(platform, version)
        print "\nChecking LilyPond presence"
        if lilypond_cmd.installed:
            print ("LilyPond {} is already installed in cache," \
                   +" continuing with test script.").format(
                       lilypond_cmd.version)
        else:
            print "LilyPond {} is not installed yet.".format(version)
            print "Downloading and installing now"
            sp.check_call(
                ["wget", "-O",
                 cls.lily_install_script,
                 cls._download_url(platform, version)])
            sp.check_call(["sh", cls.lily_install_script,
                           "--prefix",
                           LilyCmd._cache_directory(platform, version),
                           "--batch"])

    @classmethod
    def clean_cache(cls):
        """Clean the cache from versions of Lilypond older than
        `cache_cleanup_interval`"""
        print "Clean cache\n"
        cached = cls._get_cached_versions()
        now = datetime.datetime.now()
        for lily in cached:
            if lily['last_used'] is None:
                print 'Removing cached LilyPond', lily['version'],\
                    lily['platform'], '(never used)'
                shutil.rmtree(lily['directory'])
            elif now - lily['last_used'] > cls.cache_cleanup_interval:
                print 'Removing cached LilyPond', lily['version'],\
                    lily['platform'], '(last used', \
                    lily['last_used'].isoformat(), ')'
                shutil.rmtree(lily['directory'])
            else:
                print 'Keeping cached LilyPond', lily['version'],\
                    lily['platform'], '(last used', \
                    lily['last_used'].isoformat(), ')'

    ####################################################################
    # Private members

    @classmethod
    def _cache_directory(cls, platform, version):
        """Get the cache directory name for the given platform and version"""
        return osp.join(cls.cache_root, platform, version)

    def _lilypond_version(self):
        """Determine the LilyPond version actually run
        by the command self.lily_command"""
        lily = sp.Popen([self.command, "--version"],
                        stdout=sp.PIPE, stderr=sp.PIPE)
        version_line = lily.communicate()[0].splitlines()[0]
        return re.search(r"\d+\.\d+\.\d+", version_line).group(0)

    def _mark_cache_file(self):
        """Get the name of the file that will store the timestamp of the last
        time this command has been used."""
        if self.cached:
            return osp.join(osp.dirname(self.command), '.oll-last-used')
        else:
            return None

    def _mark_cache(self):
        """Write the timestamp of now in the cache file"""
        if self.cached:
            with open(self._mark_cache_file(), 'w') as mark_file:
                mark_file.write(datetime.datetime.now().isoformat())

    def _last_used(self):
        """Returns the last time this command has been used, or None if it was
        never used."""
        if self.cached:
            if not osp.isfile(self._mark_cache_file()):
                return None
            with open(self._mark_cache_file(), 'r') as mark_file:
                fcontent = mark_file.readline()
                return dateutil.parser.parse(fcontent)


    @classmethod
    def _download_url(cls, lily_platform, lily_version):
        """Format a string representing the URL to
        download the requested LilyPond distribution"""
        return "{}{}/lilypond-{}.{}.sh".format(
            cls.binary_site, lily_platform, lily_version, lily_platform)


    @classmethod
    def _get_cached_versions(cls):
        """Return a list of dictionaries with the attributes of cached
        versions"""
        versions = []
        for platform in os.listdir(cls.cache_root):
            dname = osp.join(cls.cache_root, platform)
            if osp.isdir(dname):
                for version in os.listdir(dname):
                    if osp.isdir(osp.join(dname, version)):
                        cmd = LilyCmd.with_version(platform, version)
                        versions.append(
                            {'platform': platform,
                             'version': version,
                             'last_used': cmd._last_used(),
                             'directory': osp.abspath(osp.join(dname, version))})
        return versions
